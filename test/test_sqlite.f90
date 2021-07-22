! test_sqlite.f90
program test_sqlite
    use, intrinsic :: iso_c_binding
    use :: sqlite
    character(len=*), parameter :: DB_FILE  = 'test.db'
    character(len=*), parameter :: DB_TABLE = 'test_table'

    character(len=:), allocatable :: errmsg
    integer                       :: rc
    type(c_ptr)                   :: db
    type(c_ptr)                   :: stmt

    ! Open SQLite database.
    rc = sqlite3_open(DB_FILE, db)
    if (rc /= SQLITE_OK) stop 'sqlite3_open(): failed'

    ! Create table.
    rc = sqlite3_exec(db, "CREATE TABLE test_table (" // &
                          "id     INTEGER PRIMARY KEY AUTOINCREMENT," // &
                          "string VARCHAR(32)," // &
                          "value  INTEGER)", errmsg)
    if (rc /= SQLITE_OK) print '("sqlite3_exec(): ", a)', errmsg

    ! Insert values.
    rc = sqlite3_exec(db, "INSERT INTO " // DB_TABLE // "(string, value) VALUES('one', 12345)", errmsg)
    if (rc /= SQLITE_OK) print '("sqlite3_exec(): ", a)', errmsg

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
    rc = sqlite3_prepare(db, "SELECT * FROM " // DB_TABLE, stmt)
    if (rc /= SQLITE_OK) print '("sqlite3_prepare(): failed")'

    do while (sqlite3_step(stmt) /= SQLITE_DONE)
        call print_values(stmt, 3)
    end do

    rc = sqlite3_finalize(stmt)
    if (rc /= SQLITE_OK) print '("sqlite3_finalize(): failed")'

    ! Close SQLite handle.
    rc = sqlite3_close(db)
    if (rc /= SQLITE_OK) stop 'sqlite3_close(): failed'
contains
    subroutine print_values(stmt, ncols)
        type(c_ptr), intent(inout) :: stmt
        integer,     intent(in)    :: ncols
        integer                    :: col_type
        integer                    :: i

        do i = 0, ncols - 1
            col_type = sqlite3_column_type(stmt, i)

            select case (col_type)
                case (SQLITE_INTEGER)
                    write (*, '(i12)', advance='no') sqlite3_column_int(stmt, i)

                case (SQLITE_FLOAT)
                    write (*, '(f0.8)', advance='no') sqlite3_column_double(stmt, i)

                case (SQLITE_TEXT)
                    write (*, '(a12)', advance='no') sqlite3_column_text(stmt, i)

                case default
                    write (*, '(" unsupported")', advance='no')
            end select
        end do

        print *
    end subroutine print_values
end program test_sqlite
