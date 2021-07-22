! sqlite.f90
!
! Fortran 2018 interface bindings to SQLite 3.
!
! Author:  Philipp Engel
! Licence: ISC
module sqlite
    use, intrinsic :: iso_c_binding
    implicit none (type, external)
    private

    integer, parameter, public :: SQLITE_INTEGER = 1
    integer, parameter, public :: SQLITE_FLOAT   = 2
    integer, parameter, public :: SQLITE_TEXT    = 3
    integer, parameter, public :: SQLITE_BLOB    = 4
    integer, parameter, public :: SQLITE_NULL    = 5

    integer, parameter, public :: SQLITE_OK         = 0
    integer, parameter, public :: SQLITE_ERROR      = 1
    integer, parameter, public :: SQLITE_INTERNAL   = 2
    integer, parameter, public :: SQLITE_PERM       = 3
    integer, parameter, public :: SQLITE_ABORT      = 4
    integer, parameter, public :: SQLITE_BUSY       = 5
    integer, parameter, public :: SQLITE_LOCKED     = 6
    integer, parameter, public :: SQLITE_NOMEM      = 7
    integer, parameter, public :: SQLITE_READONLY   = 8
    integer, parameter, public :: SQLITE_INTERRUPT  = 9
    integer, parameter, public :: SQLITE_IOERR      = 10
    integer, parameter, public :: SQLITE_CORRUPT    = 11
    integer, parameter, public :: SQLITE_NOTFOUND   = 12
    integer, parameter, public :: SQLITE_FULL       = 13
    integer, parameter, public :: SQLITE_CANTOPEN   = 14
    integer, parameter, public :: SQLITE_PROTOCOL   = 15
    integer, parameter, public :: SQLITE_EMPTY      = 16
    integer, parameter, public :: SQLITE_SCHEMA     = 17
    integer, parameter, public :: SQLITE_TOOBIG     = 18
    integer, parameter, public :: SQLITE_CONSTRAINT = 19
    integer, parameter, public :: SQLITE_MISMATCH   = 20
    integer, parameter, public :: SQLITE_MISUSE     = 21
    integer, parameter, public :: SQLITE_NOLFS      = 22
    integer, parameter, public :: SQLITE_AUTH       = 23
    integer, parameter, public :: SQLITE_FORMAT     = 24
    integer, parameter, public :: SQLITE_RANGE      = 25
    integer, parameter, public :: SQLITE_NOTADB     = 26
    integer, parameter, public :: SQLITE_NOTICE     = 27
    integer, parameter, public :: SQLITE_WARNING    = 28
    integer, parameter, public :: SQLITE_ROW        = 100
    integer, parameter, public :: SQLITE_DONE       = 101

    public :: sqlite3_bind_double
    public :: sqlite3_bind_int
    public :: sqlite3_bind_text
    public :: sqlite3_bind_text_
    public :: sqlite3_close
    public :: sqlite3_column_double
    public :: sqlite3_column_int
    public :: sqlite3_column_text
    public :: sqlite3_column_type
    public :: sqlite3_exec
    public :: sqlite3_exec_
    public :: sqlite3_finalize
    public :: sqlite3_open
    public :: sqlite3_open_
    public :: sqlite3_prepare
    public :: sqlite3_prepare_
    public :: sqlite3_reset
    public :: sqlite3_step

    interface
        ! int sqlite3_bind_double(sqlite3_stmt* stmt, int idx, double val)
        function sqlite3_bind_double(stmt, idx, val) bind(c, name='sqlite3_bind_double')
            import :: c_double, c_int, c_ptr
            type(c_ptr),         intent(in), value :: stmt
            integer(kind=c_int), intent(in), value :: idx
            real(kind=c_double), intent(in), value :: val
            integer(kind=c_int)                    :: sqlite3_bind_double
        end function sqlite3_bind_double

        ! int sqlite3_bind_int(sqlite3_stmt* stmt, int idx, int val)
        function sqlite3_bind_int(stmt, idx, val) bind(c, name='sqlite3_bind_int')
            import :: c_int, c_ptr
            type(c_ptr),         intent(in), value :: stmt
            integer(kind=c_int), intent(in), value :: idx
            integer(kind=c_int), intent(in), value :: val
            integer(kind=c_int)                    :: sqlite3_bind_int
        end function sqlite3_bind_int

        ! int sqlite3_bind_text(sqlite3_stmt* stmt, int idx, const char *val, int l, void(*)(void*))
        function sqlite3_bind_text_(stmt, idx, val, l, funptr) bind(c, name='sqlite3_bind_text')
            import :: c_char, c_funptr, c_int, c_ptr
            type(c_ptr),            intent(in), value :: stmt
            integer(kind=c_int),    intent(in), value :: idx
            character(kind=c_char), intent(in)        :: val
            integer(kind=c_int),    intent(in), value :: l
            type(c_funptr),         intent(in), value :: funptr
            integer(kind=c_int)                       :: sqlite3_bind_text_
        end function sqlite3_bind_text_

        ! int sqlite3_close(sqlite3*)
        function sqlite3_close(db) bind(c, name='sqlite3_close')
            import :: c_int, c_ptr
            type(c_ptr), intent(in), value :: db
            integer(kind=c_int)            :: sqlite3_close
        end function sqlite3_close

        ! double sqlite3_column_double(sqlite3_stmt*, int icol)
        function sqlite3_column_double(stmt, icol) bind(c, name='sqlite3_column_double')
            import :: c_double, c_int, c_ptr
            type(c_ptr),         intent(in), value :: stmt
            integer(kind=c_int), intent(in), value :: icol
            real(kind=c_double)                    :: sqlite3_column_double
        end function sqlite3_column_double

        ! int sqlite3_column_int(sqlite3_stmt*, int icol)
        function sqlite3_column_int(stmt, icol) bind(c, name='sqlite3_column_int')
            import :: c_int, c_ptr
            type(c_ptr),         intent(in), value :: stmt
            integer(kind=c_int), intent(in), value :: icol
            integer(kind=c_int)                    :: sqlite3_column_int
        end function sqlite3_column_int

        ! const unsigned char *sqlite3_column_text(sqlite3_stmt*, int icol)
        function sqlite3_column_text_(stmt, icol) bind(c, name='sqlite3_column_text')
            import :: c_int, c_ptr
            type(c_ptr),         intent(in), value :: stmt
            integer(kind=c_int), intent(in), value :: icol
            type(c_ptr)                            :: sqlite3_column_text_
        end function sqlite3_column_text_

        ! int sqlite3_column_type(sqlite3_stmt*, int icol)
        function sqlite3_column_type(stmt, icol) bind(c, name='sqlite3_column_type')
            import :: c_int, c_ptr
            type(c_ptr),         intent(in), value :: stmt
            integer(kind=c_int), intent(in), value :: icol
            integer(kind=c_int)                    :: sqlite3_column_type
        end function sqlite3_column_type

        ! int sqlite3_exec(sqlite3* db, const char *sql, int (*callback)(void*,int,char**,char**), void *client_data, char **errmsg)
        function sqlite3_exec_(db, sql, callback, client_data, errmsg) bind(c, name='sqlite3_exec')
            import :: c_char, c_funptr, c_int, c_ptr
            type(c_ptr),            intent(in), value :: db
            character(kind=c_char), intent(in)        :: sql
            type(c_funptr),         intent(in), value :: callback
            type(c_ptr),            intent(in), value :: client_data
            type(c_ptr),            intent(in)        :: errmsg
            integer(kind=c_int)                       :: sqlite3_exec_
        end function sqlite3_exec_

        ! int sqlite3_finalize(sqlite3_stmt *stmt)
        function sqlite3_finalize(stmt) bind(c, name='sqlite3_finalize')
            import :: c_int, c_ptr
            type(c_ptr), intent(in), value :: stmt
            integer(kind=c_int)            :: sqlite3_finalize
        end function sqlite3_finalize

        ! int sqlite3_open(const char *filename, sqlite3 **db)
        function sqlite3_open_(filename, db) bind(c, name='sqlite3_open')
            import :: c_char, c_int, c_ptr
            character(kind=c_char), intent(in) :: filename
            type(c_ptr),            intent(in) :: db
            integer(kind=c_int)                :: sqlite3_open_
        end function sqlite3_open_

        ! int sqlite3_prepare(sqlite3 *db, const char *sql, int nbyte, sqlite3_stmt **stmt, const char **tail)
        function sqlite3_prepare_(db, sql, nbyte, stmt, tail) bind(c, name='sqlite3_prepare')
            import :: c_char, c_int, c_ptr
            type(c_ptr),            intent(in), value :: db
            character(kind=c_char), intent(in)        :: sql
            integer(kind=c_int),    intent(in), value :: nbyte
            type(c_ptr),            intent(in)        :: stmt
            type(c_ptr),            intent(in)        :: tail
            integer(kind=c_int)                       :: sqlite3_prepare_
        end function sqlite3_prepare_

        ! int sqlite3_reset(sqlite3_stmt *stmt)
        function sqlite3_reset(stmt) bind(c, name='sqlite3_reset')
            import :: c_int, c_ptr
            type(c_ptr), intent(in), value :: stmt
            integer(kind=c_int)            :: sqlite3_reset
        end function sqlite3_reset

        ! int sqlite3_step(sqlite3_stmt *stmt)
        function sqlite3_step(stmt) bind(c, name='sqlite3_step')
            import :: c_int, c_ptr
            type(c_ptr), intent(in), value :: stmt
            integer(kind=c_int)            :: sqlite3_step
        end function sqlite3_step
    end interface

    interface
        function c_strlen(str) bind(c, name='strlen')
            import :: c_ptr, c_size_t
            type(c_ptr), intent(in), value :: str
            integer(c_size_t)              :: c_strlen
        end function c_strlen
    end interface
contains
    subroutine c_f_str_ptr(c_str, f_str)
        type(c_ptr),      intent(in)           :: c_str
        character(len=*), intent(out)          :: f_str
        character(kind=c_char, len=1), pointer :: chars(:)
        integer                                :: i

        if (.not. c_associated(c_str)) then
            f_str = ' '
            return
        end if

        call c_f_pointer(c_str, chars, [ huge(0) ])
        i = 1

        do while (chars(i) /= c_null_char .and. i <= len(f_str))
            f_str(i:i) = chars(i)
            i = i + 1
        end do

        if (i < len(f_str)) f_str(i:) = ' '
    end subroutine c_f_str_ptr

    function sqlite3_bind_text(stmt, idx, val)
        type(c_ptr),      intent(inout) :: stmt
        integer,          intent(in)    :: idx
        character(len=*), intent(in)    :: val
        integer                         :: sqlite3_bind_text

        sqlite3_bind_text = sqlite3_bind_text_(stmt, idx, val, len(val), c_null_ptr)
    end function sqlite3_bind_text

    function sqlite3_column_text(stmt, icol)
        type(c_ptr), intent(inout)    :: stmt
        integer,     intent(in)       :: icol
        type(c_ptr)                   :: ptr
        character(len=:), allocatable :: sqlite3_column_text

        ptr = sqlite3_column_text_(stmt, icol)
        if (.not. c_associated(ptr)) return
        allocate (character(len=c_strlen(ptr)) :: sqlite3_column_text)
        call c_f_str_ptr(ptr, sqlite3_column_text)
    end function sqlite3_column_text

    function sqlite3_exec(db, sql, errmsg)
        type(c_ptr),                   intent(in)            :: db
        character(len=*),              intent(in)            :: sql
        character(len=:), allocatable, intent(out), optional :: errmsg
        integer                                              :: sqlite3_exec
        type(c_ptr)                                          :: ptr

        sqlite3_exec = sqlite3_exec_(db, sql // c_null_char, c_null_ptr, c_null_ptr, ptr)
        if (.not. c_associated(ptr)) return

        if (present(errmsg)) then
            allocate (character(len=c_strlen(ptr)) :: errmsg)
            call c_f_str_ptr(ptr, errmsg)
        end if
    end function sqlite3_exec

    function sqlite3_open(filename, db)
        character(len=*), intent(in)  :: filename
        type(c_ptr),      intent(out) :: db
        integer                       :: sqlite3_open

        sqlite3_open = sqlite3_open_(filename // c_null_char, db)
    end function sqlite3_open

    function sqlite3_prepare(db, sql, stmt)
        type(c_ptr),      intent(inout) :: db
        character(len=*), intent(in)    :: sql
        type(c_ptr),      intent(out)   :: stmt
        integer                         :: sqlite3_prepare

        sqlite3_prepare = sqlite3_prepare_(db, sql, len(sql), stmt, c_null_ptr)
    end function sqlite3_prepare
end module sqlite
