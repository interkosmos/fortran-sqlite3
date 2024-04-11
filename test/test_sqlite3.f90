! test_sqlite3.f90
module callbacks
    use, intrinsic :: iso_c_binding
    use :: sqlite3
    use :: sqlite3_util
    implicit none
    private

    public :: error_log_callback
    public :: exec_callback
    public :: update_callback
contains
    ! int callback(void *client_data, int argc, char **argv, char **cols)
    integer(kind=c_int) function exec_callback(client_data, argc, argv, cols) bind(c)
        !! Callback function for `sqlite3_exec()` that just prints the passed
        !! row of the SQL query.
        type(c_ptr),         intent(in), value :: client_data
        integer(kind=c_int), intent(in), value :: argc
        type(c_ptr),         intent(in)        :: argv(*)
        type(c_ptr),         intent(in)        :: cols(*)

        character(len=:), allocatable :: buf
        integer                       :: i

        exec_callback = 1 ! No more rows on error.

        print '("--- There are ", i0, " values in selected row")', argc
        if (argc == 0) return

        do i = 1, argc
            if (.not. c_associated(argv(i))) cycle
            call c_f_str_ptr(argv(i), buf)
            print '("VALUE: ", a)', buf
            buf = ' '
        end do

        exec_callback = 0
    end function exec_callback

    ! void error_log_callback(void *udp, int err_code, const char *err_msg)
    subroutine error_log_callback(udp, err_code, err_msg) bind(c)
        type(c_ptr),         intent(in), value :: udp
        integer(kind=c_int), intent(in), value :: err_code
        type(c_ptr),         intent(in), value :: err_msg

        character(len=:), allocatable :: msg

        call c_f_str_ptr(err_msg, msg)
        print '(a)', repeat('-', 64)
        print '("ERROR ", i0, ": ", a)', err_code, msg
        print '(a)', repeat('-', 64)
        if (allocated(msg)) deallocate (msg)
    end subroutine error_log_callback

    ! void update_callback(void *udp, int type, const char *db_name, const char *tbl_name, sqlite3_int64 rowid)
    subroutine update_callback(udp, type, db_name, tbl_name, rowid) bind(c)
        !! Callback routine that is called whenever a row is inserted, updated,
        !! or deleted in the database. Has to be registered with
        !! `sqlite3_update_hook()`.
        type(c_ptr),             intent(in), value :: udp
        integer(kind=c_int),     intent(in), value :: type
        type(c_ptr),             intent(in), value :: db_name
        type(c_ptr),             intent(in), value :: tbl_name
        integer(kind=c_int64_t), intent(in), value :: rowid

        character(len=:), allocatable :: db_str, tbl_str

        call c_f_str_ptr(db_name, db_str)
        call c_f_str_ptr(tbl_name, tbl_str)

        select case (type)
            case (SQLITE_INSERT)
                print '("Row ", i0, " has been added to table ", a, " in database ", a, "!")', &
                    rowid, tbl_str, db_str

            case (SQLITE_UPDATE)
                print '("Row ", i0, " in table ", a, " of database ", a, " has been updated!")', &
                    rowid, tbl_str, db_str

            case (SQLITE_DELETE)
                print '("Row ", i0, " in table ", a, " of database ", a, " has been deleted!")', &
                    rowid, tbl_str, db_str
        end select

        if (allocated(db_str)) deallocate (db_str)
        if (allocated(tbl_str)) deallocate (tbl_str)
    end subroutine update_callback
end module callbacks

program test_sqlite3
    use, intrinsic :: iso_c_binding
    use :: sqlite3
    use :: callbacks
    implicit none (type, external)
    character(len=*), parameter :: DB_FILE  = 'test.sqlite'
    character(len=*), parameter :: DB_TABLE = 'test_table'

    character(len=:), allocatable :: db_name ! Database name.
    character(len=:), allocatable :: errmsg  ! Error message.

    integer     :: rc   ! Return code.
    type(c_ptr) :: db   ! SQLite database.
    type(c_ptr) :: stmt ! SQLite statement.
    type(c_ptr) :: udp  ! User-data pointer.

    ! Set configuration to single thread.
    rc = sqlite3_config(SQLITE_CONFIG_SINGLETHREAD)
    if (rc /= SQLITE_OK) stop 'sqlite3_config(): failed'

    rc = sqlite3_config(SQLITE_CONFIG_LOG, c_funloc(error_log_callback), c_null_ptr)
    if (rc /= SQLITE_OK) stop 'sqlite3_config(): failed'

    print '("SQLite library version: ", a)', sqlite3_libversion()
    print '("SQLite source ID: ", a)', sqlite3_sourceid()

    ! Open SQLite database.
    rc = sqlite3_open_v2(DB_FILE, db, ior(SQLITE_OPEN_READWRITE, SQLITE_OPEN_CREATE))

    if (rc /= SQLITE_OK) then
        print '("Error ", i0, ": ", a)', sqlite3_errcode(db), sqlite3_errmsg(db)
        stop 'sqlite3_open_v2(): failed'
    end if

    db_name = sqlite3_db_name(db, 0)
    if (len(db_name) == 0) stop 'sqlite3_db_name(): failed'
    print '("DB name: ", a)', db_name

    ! Testing logging.
    call sqlite3_log(1, 'TEST LOG')

    ! Enable WAL mode.
    print '("Turning WAL mode on ...")'
    rc = journal_mode_wal(db)
    if (rc /= SQLITE_OK) print '("Unable to set WAL mode")'

    ! Register update hook.
    udp = sqlite3_update_hook(db, c_funloc(update_callback), c_null_ptr)

    ! Query SQLite version.
    rc = sqlite3_prepare(db, "SELECT SQLITE_VERSION()", stmt)
    if (rc /= SQLITE_OK) print '("sqlite3_prepare(): failed")'

    if (rc /= SQLITE_OK) then
        print '("Failed to fetch data: ", a)', sqlite3_errmsg(db)
    else
        rc = sqlite3_step(stmt)

        if (rc == SQLITE_ROW) then
            print '("SQLite version from query: ", a)', sqlite3_column_text(stmt, 0)
        end if
    end if

    rc = sqlite3_finalize(stmt)
    if (rc /= SQLITE_OK) print '("sqlite3_finalize(): failed")'

    ! Create table.
    rc = sqlite3_exec(db, "CREATE TABLE " // DB_TABLE // " (" // &
                          "id     INTEGER PRIMARY KEY," // &
                          "string TEXT," // &
                          "value  INTEGER)", &
                      c_null_funptr, c_null_ptr, errmsg)
    call print_error(rc, 'sqlite3_exec', errmsg)

    ! Insert values.
    rc = sqlite3_exec(db, "INSERT INTO " // DB_TABLE // "(string, value) VALUES('one', 12345)", &
                      c_null_funptr, c_null_ptr, errmsg)
    call print_error(rc, 'sqlite3_exec', errmsg)

    ! Prepare statement.
    rc = sqlite3_prepare_v2(db, "INSERT INTO " // DB_TABLE // "(string, value) VALUES(?, ?)", stmt)
    if (rc /= SQLITE_OK) print '("sqlite3_prepare_v2(): failed")'

    ! Bind values.
    rc = sqlite3_bind_text(stmt, 1, 'two')
    if (rc /= SQLITE_OK) print '("sqlite3_bind_text(): failed")'

    rc = sqlite3_bind_int(stmt, 2, 987654321)
    if (rc /= SQLITE_OK) print '("sqlite3_bind_int(): failed")'

    ! Insert values.
    rc = sqlite3_step(stmt)
    if (rc /= SQLITE_DONE) print '("sqlite3_step(): failed")'

    ! Reset statement, add more values.
    rc = sqlite3_reset(stmt)
    if (rc /= SQLITE_OK) print '("sqlite3_reset(): failed")'
    rc = sqlite3_bind_text(stmt, 1, 'three')
    if (rc /= SQLITE_OK) print '("sqlite3_bind_text(): failed")'
    rc = sqlite3_bind_int(stmt, 2, 192837465)
    if (rc /= SQLITE_OK) print '("sqlite3_bind_int(): failed")'
    rc = sqlite3_step(stmt)
    if (rc /= SQLITE_DONE) print '("sqlite3_step(): failed")'

    ! Clean up.
    rc = sqlite3_finalize(stmt)
    if (rc /= SQLITE_OK) print '("sqlite3_finalize(): failed")'

    ! Read values.
    print '(/, "--- TESTING PREPARE/STEP")'
    rc = sqlite3_prepare_v2(db, "SELECT * FROM " // DB_TABLE, stmt)
    if (rc /= SQLITE_OK) print '("sqlite3_prepare_v2(): failed")'

    do while (sqlite3_step(stmt) /= SQLITE_DONE)
        call print_values(stmt, 3)
    end do

    rc = sqlite3_finalize(stmt)
    call print_error(rc, 'sqlite3_finalize', errmsg)

    ! Read values using callback function.
    print '(/, "--- TESTING CALLBACK FUNCTION")'
    rc = sqlite3_exec(db, "SELECT * FROM " // DB_TABLE, &
                      c_funloc(exec_callback), c_null_ptr, errmsg)
    call print_error(rc, 'sqlite3_exec', errmsg)

    ! Close SQLite handle.
    rc = sqlite3_close_v2(db)
    if (rc /= SQLITE_OK) stop 'sqlite3_close(): failed'

    if (c_associated(db)) then
        print '("warning: database handle still associated (this is probably a compiler bug)")'
    end if
contains
    integer function journal_mode_wal(db) result(rc)
        !! Enables WAL mode.
        type(c_ptr), intent(inout) :: db

        character(len=:), allocatable :: buf
        integer                       :: err
        type(c_ptr)                   :: stmt

        rc = -1

        sql_block: block
            err = sqlite3_prepare_v2(db, "PRAGMA journal_mode=WAL", stmt)
            if (err /= SQLITE_OK) exit sql_block

            err = sqlite3_step(stmt)
            if (err /= SQLITE_ROW) exit sql_block

            buf = sqlite3_column_text(stmt, 0)
            if (.not. allocated(buf)) exit sql_block
            if (buf /= 'wal') exit sql_block

            rc = 0
        end block sql_block

        err = sqlite3_finalize(stmt)
    end function journal_mode_wal

    subroutine print_error(rc, func, errmsg)
        integer,                       intent(in)    :: rc
        character(len=*),              intent(in)    :: func
        character(len=:), allocatable, intent(inout) :: errmsg

        if (rc == SQLITE_OK) return

        if (allocated(errmsg)) then
            print '(a, "(): ", a)', trim(func), errmsg
            deallocate (errmsg)
            return
        end if

        print '(a, "(): unknown error")', trim(func)
    end subroutine print_error

    subroutine print_values(stmt, ncols)
        type(c_ptr), intent(inout) :: stmt
        integer,     intent(in)    :: ncols

        integer                       :: col_type
        integer                       :: i, n
        character(len=:), allocatable :: buf

        do i = 0, ncols - 1
            col_type = sqlite3_column_type(stmt, i)

            select case (col_type)
                case (SQLITE_INTEGER)
                    write (*, '(i12)', advance='no') sqlite3_column_int(stmt, i)

                case (SQLITE_FLOAT)
                    write (*, '(f0.8)', advance='no') sqlite3_column_double(stmt, i)

                case (SQLITE_TEXT)
                    n   = sqlite3_column_bytes(stmt, i)
                    buf = sqlite3_column_text(stmt, i)
                    write (*, '(a12, " (", i0, ")")', advance='no') buf, n

                case default
                    write (*, '(" not implemented")', advance='no')
            end select
        end do

        print *
    end subroutine print_values
end program test_sqlite3
