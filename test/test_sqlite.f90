! test_sqlite.f90
module callbacks
    use, intrinsic :: iso_c_binding
    use :: sqlite
    implicit none
    private

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
        character(len=:), allocatable          :: buf
        integer                                :: i

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

    ! void update_callback(void* udp, int type, const char *db_name, const char *tbl_name, sqlite3_int64 rowid)
    subroutine update_callback(udp, type, db_name, tbl_name, rowid) bind(c)
        !! Callback routine that is called whenever a row is inserted, updated,
        !! or deleted in the database. Has to be registered with
        !! `sqlite3_update_hook()`.
        type(c_ptr),             intent(in), value :: udp
        integer(kind=c_int),     intent(in), value :: type
        type(c_ptr),             intent(in), value :: db_name
        type(c_ptr),             intent(in), value :: tbl_name
        integer(kind=c_int64_t), intent(in), value :: rowid
        character(len=:), allocatable              :: db_str, tbl_str

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
        
        if (allocated(db_str)) deallocate(db_str)
        if (allocated(db_str)) deallocate(tbl_str)
        
    end subroutine update_callback
end module callbacks

program test_sqlite
    use, intrinsic :: iso_c_binding
    use :: sqlite
    use :: callbacks
    character(len=*), parameter :: DB_FILE  = 'test.db'
    character(len=*), parameter :: DB_TABLE = 'test_table'

    character(len=:), allocatable :: errmsg ! Error message.
    character(len=:), allocatable :: buf    ! buffer.
    integer                       :: rc     ! Return code.
    type(c_ptr)                   :: db     ! SQLite database.
    type(c_ptr)                   :: stmt   ! SQLite statement.
    type(c_ptr)                   :: udp    ! User-data pointer.

    buf = sqlite3_libversion()
    print '("SQLite library version: ", a)', buf
    deallocate(buf)
    buf =  sqlite3_sourceid()
    print '("SQLite source ID: ", a)', buf
    deallocate(buf)

    ! Open SQLite database.
    rc = sqlite3_open(DB_FILE, db)
    if (rc /= SQLITE_OK) stop 'sqlite3_open(): failed'

    ! Register update hook.
    udp = sqlite3_update_hook(db, c_funloc(update_callback), c_null_ptr)

    ! Create table.
    rc = sqlite3_exec(db, "CREATE TABLE " // DB_TABLE // " (" // &
                          "id     INTEGER PRIMARY KEY AUTOINCREMENT," // &
                          "string VARCHAR(32)," // &
                          "value  INTEGER)", &
                      c_null_funptr, c_null_ptr, errmsg)
    call print_error(rc, "sqlite3_exec", errmsg) 

    ! Insert values.
    rc = sqlite3_exec(db, "INSERT INTO " // DB_TABLE // "(string, value) VALUES('one', 12345)", &
                      c_null_funptr, c_null_ptr, errmsg)
    call print_error(rc, "sqlite3_exec", errmsg) 

    ! Prepare statement.
    rc = sqlite3_prepare(db, "INSERT INTO " // DB_TABLE // "(string, value) VALUES(?, ?)", stmt)
    if (rc /= SQLITE_OK) print '("sqlite3_prepare(): failed")'

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
    print '(/, a)', '--- TESTING PREPARE/STEP'
    rc = sqlite3_prepare(db, "SELECT * FROM " // DB_TABLE, stmt)
    if (rc /= SQLITE_OK) print '("sqlite3_prepare(): failed")'

    do while (sqlite3_step(stmt) /= SQLITE_DONE)
        call print_values(stmt, 3)
    end do

    rc = sqlite3_finalize(stmt)
    if (rc /= SQLITE_OK) print '("sqlite3_finalize(): failed")'

    ! Read values using callback function.
    print '(/, a)', '--- TESTING CALLBACK FUNCTION'
    rc = sqlite3_exec(db, "SELECT * FROM " // DB_TABLE, &
                      c_funloc(exec_callback), c_null_ptr, errmsg)
    call print_error(rc, "sqlite3_exec", errmsg) 

    ! Close SQLite handle.
    rc = sqlite3_close(db)
    if (rc /= SQLITE_OK) stop 'sqlite3_close(): failed'
contains
    subroutine print_error(rc, func, errmsg)
      integer,     intent(in)        :: rc
      character(len=*), intent(in)  :: func
      character(len=:), allocatable,     intent(inout)  :: errmsg
      
      if (rc /= SQLITE_OK) print '(a, "(): ", a)', trim(func), errmsg
      if (allocated(errmsg)) deallocate(errmsg)
    end subroutine print_error
      
      
    subroutine print_values(stmt, ncols)
        type(c_ptr), intent(inout)     :: stmt
        integer,     intent(in)        :: ncols
        integer                        :: col_type
        integer                        :: i
        character(len=:), allocatable  :: buf

        do i = 0, ncols - 1
            col_type = sqlite3_column_type(stmt, i)

            select case (col_type)
                case (SQLITE_INTEGER)
                    write (*, '(i12)', advance='no') sqlite3_column_int(stmt, i)

                case (SQLITE_FLOAT)
                    write (*, '(f0.8)', advance='no') sqlite3_column_double(stmt, i)

                case (SQLITE_TEXT)
                    buf = sqlite3_column_text(stmt, i)
                    if (allocated(buf)) then
                      write (*, '(a12)', advance='no') buf
                      deallocate(buf)
                    end if

                case default
                    write (*, '(" unsupported")', advance='no')
            end select
        end do

        print *
    end subroutine print_values
end program test_sqlite
