! sqlite3.f90
!
! Fortran 2018 interface bindings to SQLite 3.
!
! Author:  Philipp Engel
! Licence: ISC
module sqlite3
    use, intrinsic :: iso_c_binding
    use, intrinsic :: iso_fortran_env, only: i8 => int64
    use :: sqlite3_util
    implicit none (type, external)
    private

    integer, parameter :: c_unsigned_int = c_int

    integer(kind=c_int), parameter, public :: SQLITE_INTEGER = 1
    integer(kind=c_int), parameter, public :: SQLITE_FLOAT   = 2
    integer(kind=c_int), parameter, public :: SQLITE_TEXT    = 3
    integer(kind=c_int), parameter, public :: SQLITE_BLOB    = 4
    integer(kind=c_int), parameter, public :: SQLITE_NULL    = 5

    integer(kind=c_int), parameter, public :: SQLITE_OK         = 0
    integer(kind=c_int), parameter, public :: SQLITE_ERROR      = 1
    integer(kind=c_int), parameter, public :: SQLITE_INTERNAL   = 2
    integer(kind=c_int), parameter, public :: SQLITE_PERM       = 3
    integer(kind=c_int), parameter, public :: SQLITE_ABORT      = 4
    integer(kind=c_int), parameter, public :: SQLITE_BUSY       = 5
    integer(kind=c_int), parameter, public :: SQLITE_LOCKED     = 6
    integer(kind=c_int), parameter, public :: SQLITE_NOMEM      = 7
    integer(kind=c_int), parameter, public :: SQLITE_READONLY   = 8
    integer(kind=c_int), parameter, public :: SQLITE_INTERRUPT  = 9
    integer(kind=c_int), parameter, public :: SQLITE_IOERR      = 10
    integer(kind=c_int), parameter, public :: SQLITE_CORRUPT    = 11
    integer(kind=c_int), parameter, public :: SQLITE_NOTFOUND   = 12
    integer(kind=c_int), parameter, public :: SQLITE_FULL       = 13
    integer(kind=c_int), parameter, public :: SQLITE_CANTOPEN   = 14
    integer(kind=c_int), parameter, public :: SQLITE_PROTOCOL   = 15
    integer(kind=c_int), parameter, public :: SQLITE_EMPTY      = 16
    integer(kind=c_int), parameter, public :: SQLITE_SCHEMA     = 17
    integer(kind=c_int), parameter, public :: SQLITE_TOOBIG     = 18
    integer(kind=c_int), parameter, public :: SQLITE_CONSTRAINT = 19
    integer(kind=c_int), parameter, public :: SQLITE_MISMATCH   = 20
    integer(kind=c_int), parameter, public :: SQLITE_MISUSE     = 21
    integer(kind=c_int), parameter, public :: SQLITE_NOLFS      = 22
    integer(kind=c_int), parameter, public :: SQLITE_AUTH       = 23
    integer(kind=c_int), parameter, public :: SQLITE_FORMAT     = 24
    integer(kind=c_int), parameter, public :: SQLITE_RANGE      = 25
    integer(kind=c_int), parameter, public :: SQLITE_NOTADB     = 26
    integer(kind=c_int), parameter, public :: SQLITE_NOTICE     = 27
    integer(kind=c_int), parameter, public :: SQLITE_WARNING    = 28
    integer(kind=c_int), parameter, public :: SQLITE_ROW        = 100
    integer(kind=c_int), parameter, public :: SQLITE_DONE       = 101

    integer(kind=c_int), parameter, public :: SQLITE_CREATE_INDEX        = 1
    integer(kind=c_int), parameter, public :: SQLITE_CREATE_TABLE        = 2
    integer(kind=c_int), parameter, public :: SQLITE_CREATE_TEMP_INDEX   = 3
    integer(kind=c_int), parameter, public :: SQLITE_CREATE_TEMP_TABLE   = 4
    integer(kind=c_int), parameter, public :: SQLITE_CREATE_TEMP_TRIGGER = 5
    integer(kind=c_int), parameter, public :: SQLITE_CREATE_TEMP_VIEW    = 6
    integer(kind=c_int), parameter, public :: SQLITE_CREATE_TRIGGER      = 7
    integer(kind=c_int), parameter, public :: SQLITE_CREATE_VIEW         = 8
    integer(kind=c_int), parameter, public :: SQLITE_DELETE              = 9
    integer(kind=c_int), parameter, public :: SQLITE_DROP_INDEX          = 10
    integer(kind=c_int), parameter, public :: SQLITE_DROP_TABLE          = 11
    integer(kind=c_int), parameter, public :: SQLITE_DROP_TEMP_INDEX     = 12
    integer(kind=c_int), parameter, public :: SQLITE_DROP_TEMP_TABLE     = 13
    integer(kind=c_int), parameter, public :: SQLITE_DROP_TEMP_TRIGGER   = 14
    integer(kind=c_int), parameter, public :: SQLITE_DROP_TEMP_VIEW      = 15
    integer(kind=c_int), parameter, public :: SQLITE_DROP_TRIGGER        = 16
    integer(kind=c_int), parameter, public :: SQLITE_DROP_VIEW           = 17
    integer(kind=c_int), parameter, public :: SQLITE_INSERT              = 18
    integer(kind=c_int), parameter, public :: SQLITE_PRAGMA              = 19
    integer(kind=c_int), parameter, public :: SQLITE_READ                = 20
    integer(kind=c_int), parameter, public :: SQLITE_SELECT              = 21
    integer(kind=c_int), parameter, public :: SQLITE_TRANSACTION         = 22
    integer(kind=c_int), parameter, public :: SQLITE_UPDATE              = 23
    integer(kind=c_int), parameter, public :: SQLITE_ATTACH              = 24
    integer(kind=c_int), parameter, public :: SQLITE_DETACH              = 25
    integer(kind=c_int), parameter, public :: SQLITE_ALTER_TABLE         = 26
    integer(kind=c_int), parameter, public :: SQLITE_REINDEX             = 27
    integer(kind=c_int), parameter, public :: SQLITE_ANALYZE             = 28
    integer(kind=c_int), parameter, public :: SQLITE_CREATE_VTABLE       = 29
    integer(kind=c_int), parameter, public :: SQLITE_DROP_VTABLE         = 30
    integer(kind=c_int), parameter, public :: SQLITE_FUNCTION            = 31
    integer(kind=c_int), parameter, public :: SQLITE_SAVEPOINT           = 32
    integer(kind=c_int), parameter, public :: SQLITE_COPY                = 0
    integer(kind=c_int), parameter, public :: SQLITE_RECURSIVE           = 33

    integer(kind=c_int), parameter, public :: SQLITE_STATUS_MEMORY_USED        = 0
    integer(kind=c_int), parameter, public :: SQLITE_STATUS_PAGECACHE_USED     = 1
    integer(kind=c_int), parameter, public :: SQLITE_STATUS_PAGECACHE_OVERFLOW = 2
    integer(kind=c_int), parameter, public :: SQLITE_STATUS_SCRATCH_USED       = 3 ! Not used.
    integer(kind=c_int), parameter, public :: SQLITE_STATUS_SCRATCH_OVERFLOW   = 4 ! Not used.
    integer(kind=c_int), parameter, public :: SQLITE_STATUS_MALLOC_SIZE        = 5
    integer(kind=c_int), parameter, public :: SQLITE_STATUS_PARSER_STACK       = 6
    integer(kind=c_int), parameter, public :: SQLITE_STATUS_PAGECACHE_SIZE     = 7
    integer(kind=c_int), parameter, public :: SQLITE_STATUS_SCRATCH_SIZE       = 8 ! Not used.
    integer(kind=c_int), parameter, public :: SQLITE_STATUS_MALLOC_COUNT       = 9

    integer(kind=c_int), parameter, public :: SQLITE_DBSTATUS_LOOKASIDE_USED      = 0
    integer(kind=c_int), parameter, public :: SQLITE_DBSTATUS_CACHE_USED          = 1
    integer(kind=c_int), parameter, public :: SQLITE_DBSTATUS_SCHEMA_USED         = 2
    integer(kind=c_int), parameter, public :: SQLITE_DBSTATUS_STMT_USED           = 3
    integer(kind=c_int), parameter, public :: SQLITE_DBSTATUS_LOOKASIDE_HIT       = 4
    integer(kind=c_int), parameter, public :: SQLITE_DBSTATUS_LOOKASIDE_MISS_SIZE = 5
    integer(kind=c_int), parameter, public :: SQLITE_DBSTATUS_LOOKASIDE_MISS_FULL = 6
    integer(kind=c_int), parameter, public :: SQLITE_DBSTATUS_CACHE_HIT           = 7
    integer(kind=c_int), parameter, public :: SQLITE_DBSTATUS_CACHE_MISS          = 8
    integer(kind=c_int), parameter, public :: SQLITE_DBSTATUS_CACHE_WRITE         = 9
    integer(kind=c_int), parameter, public :: SQLITE_DBSTATUS_DEFERRED_FKS        = 10
    integer(kind=c_int), parameter, public :: SQLITE_DBSTATUS_CACHE_USED_SHARED   = 11
    integer(kind=c_int), parameter, public :: SQLITE_DBSTATUS_CACHE_SPILL         = 12
    integer(kind=c_int), parameter, public :: SQLITE_DBSTATUS_MAX                 = 12 ! Largest defined DBSTATUS.

    integer(kind=c_int), parameter, public :: SQLITE_CONFIG_SINGLETHREAD        =  1 ! nil
    integer(kind=c_int), parameter, public :: SQLITE_CONFIG_MULTITHREAD         =  2 ! nil
    integer(kind=c_int), parameter, public :: SQLITE_CONFIG_SERIALIZED          =  3 ! nil
    integer(kind=c_int), parameter, public :: SQLITE_CONFIG_MALLOC              =  4 ! sqlite3_mem_methods*
    integer(kind=c_int), parameter, public :: SQLITE_CONFIG_GETMALLOC           =  5 ! sqlite3_mem_methods*
    integer(kind=c_int), parameter, public :: SQLITE_CONFIG_SCRATCH             =  6 ! No longer used
    integer(kind=c_int), parameter, public :: SQLITE_CONFIG_PAGECACHE           =  7 ! void*, int sz, int N
    integer(kind=c_int), parameter, public :: SQLITE_CONFIG_HEAP                =  8 ! void*, int nByte, int min
    integer(kind=c_int), parameter, public :: SQLITE_CONFIG_MEMSTATUS           =  9 ! boolean
    integer(kind=c_int), parameter, public :: SQLITE_CONFIG_MUTEX               = 10 ! sqlite3_mutex_methods*
    integer(kind=c_int), parameter, public :: SQLITE_CONFIG_GETMUTEX            = 11 ! sqlite3_mutex_methods*
    integer(kind=c_int), parameter, public :: SQLITE_CONFIG_CHUNKALLOC          = 12 ! unused
    integer(kind=c_int), parameter, public :: SQLITE_CONFIG_LOOKASIDE           = 13 ! int int
    integer(kind=c_int), parameter, public :: SQLITE_CONFIG_PCACHE              = 14 ! no-op
    integer(kind=c_int), parameter, public :: SQLITE_CONFIG_GETPCACHE           = 15 ! no-op
    integer(kind=c_int), parameter, public :: SQLITE_CONFIG_LOG                 = 16 ! xFunc, void*
    integer(kind=c_int), parameter, public :: SQLITE_CONFIG_URI                 = 17 ! int
    integer(kind=c_int), parameter, public :: SQLITE_CONFIG_PCACHE2             = 18 ! sqlite3_pcache_methods2*
    integer(kind=c_int), parameter, public :: SQLITE_CONFIG_GETPCACHE2          = 19 ! sqlite3_pcache_methods2*
    integer(kind=c_int), parameter, public :: SQLITE_CONFIG_COVERING_INDEX_SCAN = 20 ! int
    integer(kind=c_int), parameter, public :: SQLITE_CONFIG_SQLLOG              = 21 ! xSqllog, void*
    integer(kind=c_int), parameter, public :: SQLITE_CONFIG_MMAP_SIZE           = 22 ! sqlite3_int64, sqlite3_int64
    integer(kind=c_int), parameter, public :: SQLITE_CONFIG_WIN32_HEAPSIZE      = 23 ! int nByte
    integer(kind=c_int), parameter, public :: SQLITE_CONFIG_PCACHE_HDRSZ        = 24 ! int *psz
    integer(kind=c_int), parameter, public :: SQLITE_CONFIG_PMASZ               = 25 ! unsigned int szPma
    integer(kind=c_int), parameter, public :: SQLITE_CONFIG_STMTJRNL_SPILL      = 26 ! int nByte
    integer(kind=c_int), parameter, public :: SQLITE_CONFIG_SMALL_MALLOC        = 27 ! boolean
    integer(kind=c_int), parameter, public :: SQLITE_CONFIG_SORTERREF_SIZE      = 28 ! int nByte
    integer(kind=c_int), parameter, public :: SQLITE_CONFIG_MEMDB_MAXSIZE       = 29 ! sqlite3_int64

    integer(kind=c_int), parameter, public :: SQLITE_OPEN_READONLY      = int(z'00000001') ! Ok for sqlite3_open_v2()
    integer(kind=c_int), parameter, public :: SQLITE_OPEN_READWRITE     = int(z'00000002') ! Ok for sqlite3_open_v2()
    integer(kind=c_int), parameter, public :: SQLITE_OPEN_CREATE        = int(z'00000004') ! Ok for sqlite3_open_v2()
    integer(kind=c_int), parameter, public :: SQLITE_OPEN_DELETEONCLOSE = int(z'00000008') ! VFS only
    integer(kind=c_int), parameter, public :: SQLITE_OPEN_EXCLUSIVE     = int(z'00000010') ! VFS only
    integer(kind=c_int), parameter, public :: SQLITE_OPEN_AUTOPROXY     = int(z'00000020') ! VFS only
    integer(kind=c_int), parameter, public :: SQLITE_OPEN_URI           = int(z'00000040') ! Ok for sqlite3_open_v2()
    integer(kind=c_int), parameter, public :: SQLITE_OPEN_MEMORY        = int(z'00000080') ! Ok for sqlite3_open_v2()
    integer(kind=c_int), parameter, public :: SQLITE_OPEN_MAIN_DB       = int(z'00000100') ! VFS only
    integer(kind=c_int), parameter, public :: SQLITE_OPEN_TEMP_DB       = int(z'00000200') ! VFS only
    integer(kind=c_int), parameter, public :: SQLITE_OPEN_TRANSIENT_DB  = int(z'00000400') ! VFS only
    integer(kind=c_int), parameter, public :: SQLITE_OPEN_MAIN_JOURNAL  = int(z'00000800') ! VFS only
    integer(kind=c_int), parameter, public :: SQLITE_OPEN_TEMP_JOURNAL  = int(z'00001000') ! VFS only
    integer(kind=c_int), parameter, public :: SQLITE_OPEN_SUBJOURNAL    = int(z'00002000') ! VFS only
    integer(kind=c_int), parameter, public :: SQLITE_OPEN_SUPER_JOURNAL = int(z'00004000') ! VFS only
    integer(kind=c_int), parameter, public :: SQLITE_OPEN_NOMUTEX       = int(z'00008000') ! Ok for sqlite3_open_v2()
    integer(kind=c_int), parameter, public :: SQLITE_OPEN_FULLMUTEX     = int(z'00010000') ! Ok for sqlite3_open_v2()
    integer(kind=c_int), parameter, public :: SQLITE_OPEN_SHAREDCACHE   = int(z'00020000') ! Ok for sqlite3_open_v2()
    integer(kind=c_int), parameter, public :: SQLITE_OPEN_PRIVATECACHE  = int(z'00040000') ! Ok for sqlite3_open_v2()
    integer(kind=c_int), parameter, public :: SQLITE_OPEN_WAL           = int(z'00080000') ! VFS only
    integer(kind=c_int), parameter, public :: SQLITE_OPEN_NOFOLLOW      = int(z'01000000') ! Ok for sqlite3_open_v2()
    integer(kind=c_int), parameter, public :: SQLITE_OPEN_EXRESCODE     = int(z'02000000') ! Extended result codes

    integer(kind=c_int), parameter, public :: SQLITE_PREPARE_PERSISTENT = int(z'01')
    integer(kind=c_int), parameter, public :: SQLITE_PREPARE_NORMALIZE  = int(z'02')
    integer(kind=c_int), parameter, public :: SQLITE_PREPARE_NO_VTAB    = int(z'04')

    integer(kind=c_size_t), parameter, public :: SQLITE_STATIC    = 0
    integer(kind=c_size_t), parameter, public :: SQLITE_TRANSIENT = -1

    public :: sqlite3_backup_finish
    public :: sqlite3_backup_init
    public :: sqlite3_backup_init_
    public :: sqlite3_backup_pagecount
    public :: sqlite3_backup_remaining
    public :: sqlite3_backup_step
    public :: sqlite3_bind_double
    public :: sqlite3_bind_int
    public :: sqlite3_bind_int64
    public :: sqlite3_bind_null
    public :: sqlite3_bind_parameter_count
    public :: sqlite3_bind_parameter_index
    public :: sqlite3_bind_parameter_index_
    public :: sqlite3_bind_text
    public :: sqlite3_bind_text_
    public :: sqlite3_busy_handler
    public :: sqlite3_busy_timeout
    public :: sqlite3_clear_bindings
    public :: sqlite3_close
    public :: sqlite3_close_
    public :: sqlite3_column_count
    public :: sqlite3_column_database_name
    public :: sqlite3_column_database_name_
    public :: sqlite3_column_double
    public :: sqlite3_column_int
    public :: sqlite3_column_int64
    public :: sqlite3_column_name
    public :: sqlite3_column_origin_name
    public :: sqlite3_column_origin_name_
    public :: sqlite3_column_table_name
    public :: sqlite3_column_table_name_
    public :: sqlite3_column_text
    public :: sqlite3_column_type
    public :: sqlite3_config
    public :: sqlite3_config_funptr_ptr_
    public :: sqlite3_config_int_
    public :: sqlite3_config_null_
    public :: sqlite3_data_count
    public :: sqlite3_db_name
    public :: sqlite3_db_name_
    public :: sqlite3_db_status
    public :: sqlite3_errcode
    public :: sqlite3_errmsg
    public :: sqlite3_errmsg_
    public :: sqlite3_errstr
    public :: sqlite3_errstr_
    public :: sqlite3_error_offset
    public :: sqlite3_exec
    public :: sqlite3_exec_
    public :: sqlite3_finalize
    public :: sqlite3_finalize_
    public :: sqlite3_free
    public :: sqlite3_initialize
    public :: sqlite3_last_insert_rowid
    public :: sqlite3_libversion
    public :: sqlite3_libversion_
    public :: sqlite3_libversion_number
    public :: sqlite3_limit
    public :: sqlite3_log
    public :: sqlite3_log_
    public :: sqlite3_open
    public :: sqlite3_open_
    public :: sqlite3_open_v2
    public :: sqlite3_open_v2_
    public :: sqlite3_prepare
    public :: sqlite3_prepare_
    public :: sqlite3_prepare_v2
    public :: sqlite3_prepare_v2_
    public :: sqlite3_prepare_v3
    public :: sqlite3_prepare_v3_
    public :: sqlite3_reset
    public :: sqlite3_shutdown
    public :: sqlite3_sleep
    public :: sqlite3_sourceid
    public :: sqlite3_sourceid_
    public :: sqlite3_sql
    public :: sqlite3_sql_
    public :: sqlite3_status
    public :: sqlite3_step
    public :: sqlite3_stmt_readonly
    public :: sqlite3_stmt_readonly_
    public :: sqlite3_str_append
    public :: sqlite3_str_appendall
    public :: sqlite3_str_appendchar
    public :: sqlite3_str_finish
    public :: sqlite3_str_length
    public :: sqlite3_str_new
    public :: sqlite3_str_reset
    public :: sqlite3_str_value
    public :: sqlite3_threadsafe
    public :: sqlite3_update_hook

    interface
        ! int sqlite3_backup_finish(sqlite3_backup *p)
        function sqlite3_backup_finish(p) bind(c, name='sqlite3_backup_finish')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: p
            integer(kind=c_int)            :: sqlite3_backup_finish
        end function sqlite3_backup_finish

        ! sqlite3_backup *sqlite3_backup_init(sqlite3 *pDest, const char *zDestName, sqlite3 *pSource, const char *zSourceName)
        function sqlite3_backup_init_(dest, dest_name, source, source_name) bind(c, name='sqlite3_backup_init')
            import :: c_char, c_ptr
            implicit none
            type(c_ptr),            intent(in), value :: dest
            character(kind=c_char), intent(in)        :: dest_name
            type(c_ptr),            intent(in), value :: source
            character(kind=c_char), intent(in)        :: source_name
            type(c_ptr)                               :: sqlite3_backup_init_
        end function sqlite3_backup_init_

        ! int sqlite3_backup_pagecount(sqlite3_backup *p)
        function sqlite3_backup_pagecount(p) bind(c, name='sqlite3_backup_pagecount')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: p
            integer(kind=c_int)            :: sqlite3_backup_pagecount
        end function sqlite3_backup_pagecount

        ! int sqlite3_backup_remaining(sqlite3_backup *p)
        function sqlite3_backup_remaining(p) bind(c, name='sqlite3_backup_remaining')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: p
            integer(kind=c_int)            :: sqlite3_backup_remaining
        end function sqlite3_backup_remaining

        ! int sqlite3_backup_step(sqlite3_backup *p, int nPage)
        function sqlite3_backup_step(p, npage) bind(c, name='sqlite3_backup_step')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),         intent(in), value :: p
            integer(kind=c_int), intent(in), value :: npage
            integer(kind=c_int)                    :: sqlite3_backup_step
        end function sqlite3_backup_step

        ! int sqlite3_bind_double(sqlite3_stmt *stmt, int idx, double val)
        function sqlite3_bind_double(stmt, idx, val) bind(c, name='sqlite3_bind_double')
            import :: c_double, c_int, c_ptr
            implicit none
            type(c_ptr),         intent(in), value :: stmt
            integer(kind=c_int), intent(in), value :: idx
            real(kind=c_double), intent(in), value :: val
            integer(kind=c_int)                    :: sqlite3_bind_double
        end function sqlite3_bind_double

        ! int sqlite3_bind_int(sqlite3_stmt *stmt, int idx, int val)
        function sqlite3_bind_int(stmt, idx, val) bind(c, name='sqlite3_bind_int')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),         intent(in), value :: stmt
            integer(kind=c_int), intent(in), value :: idx
            integer(kind=c_int), intent(in), value :: val
            integer(kind=c_int)                    :: sqlite3_bind_int
        end function sqlite3_bind_int

        ! int sqlite3_bind_int(sqlite3_stmt *stmt, int idx, int val)
        function sqlite3_bind_int64(stmt, idx, val) bind(c, name='sqlite3_bind_int64')
            import :: c_int, c_int64_t, c_ptr
            implicit none
            type(c_ptr),             intent(in), value :: stmt
            integer(kind=c_int),     intent(in), value :: idx
            integer(kind=c_int64_t), intent(in), value :: val
            integer(kind=c_int)                        :: sqlite3_bind_int64
        end function sqlite3_bind_int64

        ! int sqlite3_bind_null(sqlite3_stmt *stmt, int idx)
        function sqlite3_bind_null(stmt, idx) bind(c, name='sqlite3_bind_null')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),         intent(in), value :: stmt
            integer(kind=c_int), intent(in), value :: idx
            integer(kind=c_int)                    :: sqlite3_bind_null
        end function sqlite3_bind_null

        ! int sqlite3_bind_parameter_count(sqlite3_stmt *stmt)
        function sqlite3_bind_parameter_count(stmt) bind(c, name='sqlite3_bind_parameter_count')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: stmt
            integer(kind=c_int)            :: sqlite3_bind_parameter_count
        end function sqlite3_bind_parameter_count

        ! int sqlite3_bind_parameter_index(sqlite3_stmt *stmt, const char *zName)
        function sqlite3_bind_parameter_index_(stmt, name) bind(c, name='sqlite3_bind_parameter_index')
            import :: c_char, c_int, c_ptr
            implicit none
            type(c_ptr),            intent(in), value :: stmt
            character(kind=c_char), intent(in)        :: name
            integer(kind=c_int)                       :: sqlite3_bind_parameter_index_
        end function sqlite3_bind_parameter_index_

        ! int sqlite3_bind_text(sqlite3_stmt *stmt, int idx, const char *val, int l, void(*)(void*))
        function sqlite3_bind_text_(stmt, idx, val, l, destructor) bind(c, name='sqlite3_bind_text')
            import :: c_char, c_funptr, c_int, c_ptr, c_size_t
            implicit none
            type(c_ptr),            intent(in), value :: stmt
            integer(kind=c_int),    intent(in), value :: idx
            character(kind=c_char), intent(in)        :: val
            integer(kind=c_int),    intent(in), value :: l
            integer(kind=c_size_t), intent(in), value :: destructor
            integer(kind=c_int)                       :: sqlite3_bind_text_
        end function sqlite3_bind_text_

        ! int sqlite3_busy_handler(sqlite3 *db, int (*)(void *udp, int n), void *udp)
        function sqlite3_busy_handler(db, funptr, udp) bind(c, name='sqlite3_busy_handler')
            import :: c_funptr, c_int, c_ptr
            implicit none
            type(c_ptr),    intent(in), value :: db
            type(c_funptr), intent(in), value :: funptr
            type(c_ptr),    intent(in), value :: udp
            integer(kind=c_int)               :: sqlite3_busy_handler
        end function sqlite3_busy_handler

        ! int sqlite3_busy_timeout(sqlite3 *db, int ms)
        function sqlite3_busy_timeout(db, ms) bind(c, name='sqlite3_busy_timeout')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),         intent(in), value :: db
            integer(kind=c_int), intent(in), value :: ms
            integer(kind=c_int)                    :: sqlite3_busy_timeout
        end function sqlite3_busy_timeout

        ! int sqlite3_clear_bindings(sqlite3_stmt *stmt)
        function sqlite3_clear_bindings(stmt) bind(c, name='sqlite3_clear_bindings')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: stmt
            integer(kind=c_int)            :: sqlite3_clear_bindings
        end function sqlite3_clear_bindings

        ! int sqlite3_close(sqlite3 *db)
        function sqlite3_close_(db) bind(c, name='sqlite3_close')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: db
            integer(kind=c_int)            :: sqlite3_close_
        end function sqlite3_close_

        ! int sqlite3_column_count(sqlite3_stmt *stmt)
        function sqlite3_column_count(stmt) bind(c, name='sqlite3_column_count')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: stmt
            integer(kind=c_int)            :: sqlite3_column_count
        end function sqlite3_column_count

        ! const char *sqlite3_column_database_name(sqlite3_stmt *stmt, int idx)
        function sqlite3_column_database_name_(stmt, idx) bind(c, name='sqlite3_column_database_name')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),         intent(in), value :: stmt
            integer(kind=c_int), intent(in), value :: idx
            type(c_ptr)                            :: sqlite3_column_database_name_
        end function sqlite3_column_database_name_

        ! double sqlite3_column_double(sqlite3_stmt *stmt, int idx)
        function sqlite3_column_double(stmt, idx) bind(c, name='sqlite3_column_double')
            import :: c_double, c_int, c_ptr
            implicit none
            type(c_ptr),         intent(in), value :: stmt
            integer(kind=c_int), intent(in), value :: idx
            real(kind=c_double)                    :: sqlite3_column_double
        end function sqlite3_column_double

        ! int sqlite3_column_int(sqlite3_stmt *stmt, int idx)
        function sqlite3_column_int(stmt, idx) bind(c, name='sqlite3_column_int')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),         intent(in), value :: stmt
            integer(kind=c_int), intent(in), value :: idx
            integer(kind=c_int)                    :: sqlite3_column_int
        end function sqlite3_column_int

       ! sqlite3_int64 sqlite3_column_int64(sqlite3_stmt *stmt, int idx)
        function sqlite3_column_int64(stmt, idx) bind(c, name='sqlite3_column_int64')
            import :: c_int, c_int64_t, c_ptr
            implicit none
            type(c_ptr),         intent(in), value :: stmt
            integer(kind=c_int), intent(in), value :: idx
            integer(kind=c_int)                    :: sqlite3_column_int64
        end function sqlite3_column_int64

        ! const char *sqlite3_column_name(sqlite3_stmt *stmt, int idx)
        function sqlite3_column_name_(stmt, idx) bind(c, name='sqlite3_column_name')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),         intent(in), value :: stmt
            integer(kind=c_int), intent(in), value :: idx
            type(c_ptr)                            :: sqlite3_column_name_
        end function sqlite3_column_name_

        ! const char *sqlite3_column_origin_name(sqlite3_stmt *stmt, int idx)
        function sqlite3_column_origin_name_(stmt, idx) bind(c, name='sqlite3_column_origin_name')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),         intent(in), value :: stmt
            integer(kind=c_int), intent(in), value :: idx
            type(c_ptr)                            :: sqlite3_column_origin_name_
        end function sqlite3_column_origin_name_

        ! const char *sqlite3_column_table_name(sqlite3_stmt *stmt, int idx)
        function sqlite3_column_table_name_(stmt, idx) bind(c, name='sqlite3_column_table_name')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),         intent(in), value :: stmt
            integer(kind=c_int), intent(in), value :: idx
            type(c_ptr)                            :: sqlite3_column_table_name_
        end function sqlite3_column_table_name_

        ! const unsigned char *sqlite3_column_text(sqlite3_stmt *stmt, int idx)
        function sqlite3_column_text_(stmt, idx) bind(c, name='sqlite3_column_text')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),         intent(in), value :: stmt
            integer(kind=c_int), intent(in), value :: idx
            type(c_ptr)                            :: sqlite3_column_text_
        end function sqlite3_column_text_

        ! int sqlite3_column_type(sqlite3_stmt *stmt, int idx)
        function sqlite3_column_type(stmt, idx) bind(c, name='sqlite3_column_type')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),         intent(in), value :: stmt
            integer(kind=c_int), intent(in), value :: idx
            integer(kind=c_int)                    :: sqlite3_column_type
        end function sqlite3_column_type

        ! int sqlite3_config_funptr_ptr_(int option, void *funptr, void *ptr)
        function sqlite3_config_funptr_ptr_(option, funptr, ptr) bind(c, name='sqlite3_config_funptr_ptr_')
            import :: c_funptr, c_int, c_ptr
            implicit none
            integer(kind=c_int), intent(in), value :: option
            type(c_funptr),      intent(in), value :: funptr
            type(c_ptr),         intent(in), value :: ptr
            integer(kind=c_int)                    :: sqlite3_config_funptr_ptr_
        end function sqlite3_config_funptr_ptr_

        ! int sqlite3_config_int_(int option, int arg)
        function sqlite3_config_int_(option, arg) bind(c, name='sqlite3_config_int_')
            import :: c_int
            implicit none
            integer(kind=c_int), intent(in), value :: option
            integer(kind=c_int), intent(in), value :: arg
            integer(kind=c_int)                    :: sqlite3_config_int_
        end function sqlite3_config_int_

        ! int sqlite3_config_null_(int option)
        function sqlite3_config_null_(option) bind(c, name='sqlite3_config_null_')
            import :: c_int
            implicit none
            integer(kind=c_int), intent(in), value :: option
            integer(kind=c_int)                    :: sqlite3_config_null_
        end function sqlite3_config_null_

        ! int sqlite3_data_count(sqlite3_stmt *stmt)
        function sqlite3_data_count(stmt) bind(c, name='sqlite3_data_count')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: stmt
            integer(kind=c_int)            :: sqlite3_data_count
        end function sqlite3_data_count

        ! const char *sqlite3_db_name(sqlite3 *db, int n)
        function sqlite3_db_name_(db, n) bind(c, name='sqlite3_db_name')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),         intent(in), value :: db
            integer(kind=c_int), intent(in), value :: n
            type(c_ptr)                            :: sqlite3_db_name_
        end function sqlite3_db_name_

        ! int sqlite3_db_status(sqlite3 *db, int op, int *pCurrent, int *pHighwater, int resetFlag)
        function sqlite3_db_status(db, op, current, highwater, reset_flag) bind(c, name='sqlite3_db_status')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),         intent(in), value :: db
            integer(kind=c_int), intent(in), value :: op
            integer(kind=c_int), intent(in)        :: current
            integer(kind=c_int), intent(in)        :: highwater
            integer(kind=c_int), intent(in), value :: reset_flag
            integer(kind=c_int)                    :: sqlite3_db_status
        end function sqlite3_db_status

        ! int sqlite3_errcode(sqlite3 *db)
        function sqlite3_errcode(db) bind(c, name='sqlite3_errcode')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: db
            integer(kind=c_int)            :: sqlite3_errcode
        end function sqlite3_errcode

        ! const char *sqlite3_errmsg(sqlite3 *db)
        function sqlite3_errmsg_(db) bind(c, name='sqlite3_errmsg')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: db
            type(c_ptr)                    :: sqlite3_errmsg_
        end function sqlite3_errmsg_

        ! const char *sqlite3_errstr(int err)
        function sqlite3_errstr_(err) bind(c, name='sqlite3_errstr')
            import :: c_int, c_ptr
            implicit none
            integer(kind=c_int), intent(in), value :: err
            type(c_ptr)                            :: sqlite3_errstr_
        end function sqlite3_errstr_

        ! int sqlite3_error_offset(sqlite3 *db)
        function sqlite3_error_offset(db) bind(c, name='sqlite3_error_offset')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: db
            integer(kind=c_int)            :: sqlite3_error_offset
        end function sqlite3_error_offset

        ! int sqlite3_exec(sqlite3 *db, const char *sql, int (*callback)(void *, int, char **, char **), void *client_data, char **errmsg)
        function sqlite3_exec_(db, sql, callback, client_data, errmsg) bind(c, name='sqlite3_exec')
            import :: c_char, c_funptr, c_int, c_ptr
            implicit none
            type(c_ptr),            intent(in), value :: db
            character(kind=c_char), intent(in)        :: sql
            type(c_funptr),         intent(in), value :: callback
            type(c_ptr),            intent(in), value :: client_data
            type(c_ptr),            intent(in)        :: errmsg
            integer(kind=c_int)                       :: sqlite3_exec_
        end function sqlite3_exec_

        ! int sqlite3_finalize(sqlite3_stmt *stmt)
        function sqlite3_finalize_(stmt) bind(c, name='sqlite3_finalize')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: stmt
            integer(kind=c_int)            :: sqlite3_finalize_
        end function sqlite3_finalize_

        ! int sqlite3_initialize(void)
        function sqlite3_initialize() bind(c, name='sqlite3_initialize')
            import :: c_int
            implicit none
            integer(kind=c_int) :: sqlite3_initialize
        end function sqlite3_initialize

        ! sqlite3_int64 sqlite3_last_insert_rowid(sqlite3 *db)
        function sqlite3_last_insert_rowid(db) bind(c, name='sqlite3_last_insert_rowid')
            import :: c_int64_t, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: db
            integer(kind=c_int64_t)        :: sqlite3_last_insert_rowid
        end function sqlite3_last_insert_rowid

        ! const char *sqlite3_libversion(void)
        function sqlite3_libversion_() bind(c, name='sqlite3_libversion')
            import :: c_ptr
            implicit none
            type(c_ptr) :: sqlite3_libversion_
        end function sqlite3_libversion_

        ! int sqlite3_libversion_number(void)
        function sqlite3_libversion_number() bind(c, name='sqlite3_libversion_number')
            import :: c_int
            implicit none
            integer(kind=c_int) :: sqlite3_libversion_number
        end function sqlite3_libversion_number

        ! int sqlite3_limit(sqlite3 *db, int id, int newVal)
        function sqlite3_limit(db, id, new_val) bind(c, name='sqlite3_limit')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),         intent(in), value :: db
            integer(kind=c_int), intent(in), value :: id
            integer(kind=c_int), intent(in), value :: new_val
            integer(kind=c_int)                    :: sqlite3_limit
        end function sqlite3_limit

        ! int sqlite3_open(const char *filename, sqlite3 **db)
        function sqlite3_open_(file_name, db) bind(c, name='sqlite3_open')
            import :: c_char, c_int, c_ptr
            implicit none
            character(kind=c_char), intent(in)    :: file_name
            type(c_ptr),            intent(inout) :: db
            integer(kind=c_int)                   :: sqlite3_open_
        end function sqlite3_open_

        ! int sqlite3_open_v2(const char *filename, sqlite3 **db, int flags, const char *vfs)
        function sqlite3_open_v2_(file_name, db, flags, vfs) bind(c, name='sqlite3_open_v2')
            import :: c_char, c_int, c_ptr
            implicit none
            character(kind=c_char), intent(in)        :: file_name
            type(c_ptr),            intent(inout)     :: db
            integer(kind=c_int),    intent(in), value :: flags
            type(c_ptr),            intent(in), value :: vfs
            integer(kind=c_int)                       :: sqlite3_open_v2_
        end function sqlite3_open_v2_

        ! int sqlite3_prepare(sqlite3 *db, const char *sql, int nbyte, sqlite3_stmt **stmt, const char **tail)
        function sqlite3_prepare_(db, sql, nbyte, stmt, tail) bind(c, name='sqlite3_prepare')
            import :: c_char, c_int, c_ptr
            implicit none
            type(c_ptr),            intent(in), value :: db
            character(kind=c_char), intent(in)        :: sql
            integer(kind=c_int),    intent(in), value :: nbyte
            type(c_ptr),            intent(inout)     :: stmt
            type(c_ptr),            intent(in)        :: tail
            integer(kind=c_int)                       :: sqlite3_prepare_
        end function sqlite3_prepare_

        ! int sqlite3_prepare_v2(sqlite3 *db, const char *sql, int nbyte, sqlite3_stmt **stmt, const char **tail)
        function sqlite3_prepare_v2_(db, sql, nbyte, stmt, tail) bind(c, name='sqlite3_prepare_v2')
            import :: c_char, c_int, c_ptr
            implicit none
            type(c_ptr),            intent(in), value :: db
            character(kind=c_char), intent(in)        :: sql
            integer(kind=c_int),    intent(in), value :: nbyte
            type(c_ptr),            intent(inout)     :: stmt
            type(c_ptr),            intent(in)        :: tail
            integer(kind=c_int)                       :: sqlite3_prepare_v2_
        end function sqlite3_prepare_v2_

        ! int sqlite3_prepare_v3(sqlite3 *db, const char *sql, int nbyte, unsigned int prepFlags, sqlite3_stmt **stmt, const char **tail)
        function sqlite3_prepare_v3_(db, sql, nbyte, prep_flags, stmt, tail) bind(c, name='sqlite3_prepare_v3')
            import :: c_char, c_int, c_ptr, c_unsigned_int
            implicit none
            type(c_ptr),                  intent(in), value :: db
            character(kind=c_char),       intent(in)        :: sql
            integer(kind=c_int),          intent(in), value :: nbyte
            integer(kind=c_unsigned_int), intent(in), value :: prep_flags
            type(c_ptr),                  intent(inout)     :: stmt
            type(c_ptr),                  intent(in)        :: tail
            integer(kind=c_int)                             :: sqlite3_prepare_v3_
        end function sqlite3_prepare_v3_

        ! int sqlite3_reset(sqlite3_stmt *stmt)
        function sqlite3_reset(stmt) bind(c, name='sqlite3_reset')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: stmt
            integer(kind=c_int)            :: sqlite3_reset
        end function sqlite3_reset

        ! int sqlite3_shutdown(void)
        function sqlite3_shutdown() bind(c, name='sqlite3_shutdown')
            import :: c_int
            implicit none
            integer(kind=c_int) :: sqlite3_shutdown
        end function sqlite3_shutdown

        ! int sqlite3_sleep(int t)
        function sqlite3_sleep(t) bind(c, name='sqlite3_sleep')
            import :: c_int
            implicit none
            integer(kind=c_int), intent(in), value :: t
            integer(kind=c_int)                    :: sqlite3_sleep
        end function sqlite3_sleep

        ! const char *sqlite3_sourceid(void)
        function sqlite3_sourceid_() bind(c, name='sqlite3_sourceid')
            import :: c_ptr
            implicit none
            type(c_ptr) :: sqlite3_sourceid_
        end function sqlite3_sourceid_

        ! const char *sqlite3_sql(sqlite3_stmt *stmt)
        function sqlite3_sql_(stmt) bind(c, name='sqlite3_sql')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: stmt
            type(c_ptr)                    :: sqlite3_sql_
        end function sqlite3_sql_

        ! int sqlite3_status(int op, int *pCurrent, int *pHighwater, int resetFlag)
        function sqlite3_status(op, current, highwater, reset_flag) bind(c, name='sqlite3_status')
            import :: c_int
            implicit none
            integer(kind=c_int), intent(in), value :: op
            integer(kind=c_int), intent(in)        :: current
            integer(kind=c_int), intent(in)        :: highwater
            integer(kind=c_int), intent(in), value :: reset_flag
            integer(kind=c_int)                    :: sqlite3_status
        end function sqlite3_status

        ! int sqlite3_step(sqlite3_stmt *stmt)
        function sqlite3_step(stmt) bind(c, name='sqlite3_step')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: stmt
            integer(kind=c_int)            :: sqlite3_step
        end function sqlite3_step

        ! int sqlite3_stmt_readonly(sqlite3_stmt *stmt)
        function sqlite3_stmt_readonly_(stmt) bind(c, name='sqlite3_stmt_readonly')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: stmt
            integer(kind=c_int)            :: sqlite3_stmt_readonly_
        end function sqlite3_stmt_readonly_

        ! char *sqlite3_str_finish(sqlite3_str *str)
        function sqlite3_str_finish(str) bind(c, name='sqlite3_str_finish')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: str
            type(c_ptr)                    :: sqlite3_str_finish
        end function sqlite3_str_finish

        ! int sqlite3_str_length(sqlite3_str *str)
        function sqlite3_str_length(str) bind(c, name='sqlite3_str_length')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: str
            integer(kind=c_int)            :: sqlite3_str_length
        end function sqlite3_str_length

        ! sqlite3_str *sqlite3_str_new(sqlite3 *db)
        function sqlite3_str_new(str) bind(c, name='sqlite3_str_new')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: str
            type(c_ptr)                    :: sqlite3_str_new
        end function sqlite3_str_new

        ! char *sqlite3_str_value(sqlite3_str *str)
        function sqlite3_str_value_(str) bind(c, name='sqlite3_str_value')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: str
            type(c_ptr)                    :: sqlite3_str_value_
        end function sqlite3_str_value_

        ! void *sqlite3_update_hook(sqlite3 *db, void *update_callback, void *udp)
        !
        ! Returns the previous user-data pointer (if applicable).
        !
        ! Arguments:
        !
        !   db                  -   A database connection.
        !   update_callback     -   An application-defined callback function
        !                           that is called when a database row is
        !                           modified.
        !   udp                 -   An application-defined user-data pointer.
        !                           This value is made available to the update
        !                           callback.
        !
        !
        ! void update_callback(void *udp, int type, const char *db_name, const char *tbl_name, sqlite3_int64 rowid)
        !
        ! Arguments:
        !
        !   udp                 -   The application-defined user-data pointer.
        !   type                -   The type of database update. Possible values
        !                           are SQLITE_INSERT, SQLITE_UPDATE, and
        !                           SQLITE_DELETE.
        !   db_name             -   The logical name of the database that is
        !                           being modified. Names include main, temp, or
        !                           any name passed to ATTACH DATABASE.
        !   tbl_name            -   The name of the table that is being
        !                           modified.
        !   rowid               -   The ROWID of the row being modified. In the
        !                           case of an UPDATE, this is the ROWID value
        !                           after the modification has taken place.
        !
        function sqlite3_update_hook(db, update_callback, udp) bind(c, name='sqlite3_update_hook')
            import :: c_funptr, c_ptr
            implicit none
            type(c_ptr),    intent(in), value :: db
            type(c_funptr), intent(in), value :: update_callback
            type(c_ptr),    intent(in), value :: udp
            type(c_ptr)                       :: sqlite3_update_hook
        end function sqlite3_update_hook

        function sqlite3_threadsafe() bind(c, name='sqlite3_threadsafe')
            import :: c_int
            implicit none
            integer(kind=c_int) :: sqlite3_threadsafe
        end function sqlite3_threadsafe

        ! void sqlite3_free(void *ptr)
        subroutine sqlite3_free(ptr) bind(c, name='sqlite3_free')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: ptr
        end subroutine sqlite3_free

        ! void sqlite3_log_(int iErrCode, const char *str)
        subroutine sqlite3_log_(ierr_code, str) bind(c, name='sqlite3_log_')
            import :: c_char, c_int
            implicit none
            integer(kind=c_int),    intent(in), value :: ierr_code
            character(kind=c_char), intent(in)        :: str
        end subroutine sqlite3_log_

        ! void sqlite3_str_append(sqlite3_str *str, const char *zIn, int N)
        subroutine sqlite3_str_append(str, zin, n) bind(c, name='slqite3_str_append')
            import :: c_char, c_int, c_ptr
            implicit none
            type(c_ptr),            intent(in), value :: str
            character(kind=c_char), intent(in)        :: zin
            integer(kind=c_int),    intent(in), value :: n
        end subroutine sqlite3_str_append

        ! void sqlite3_str_appendall(sqlite3_str *str, const char *zIn)
        subroutine sqlite3_str_appendall(str, zin) bind(c, name='slqite3_str_appendall')
            import :: c_char, c_ptr
            implicit none
            type(c_ptr),            intent(in), value :: str
            character(kind=c_char), intent(in)        :: zin
        end subroutine sqlite3_str_appendall

        ! void sqlite3_str_appendchar(sqlite3_str *str, int N, char C)
        subroutine sqlite3_str_appendchar(str, n, c) bind(c, name='slqite3_str_appendchar')
            import :: c_char, c_int, c_ptr
            implicit none
            type(c_ptr),            intent(in), value :: str
            integer(kind=c_int),    intent(in), value :: n
            character(kind=c_char), intent(in), value :: c
        end subroutine sqlite3_str_appendchar

        ! void sqlite3_str_reset(sqlite3_str *str)
        subroutine sqlite3_str_reset(str) bind(c, name='slqite3_str_reset')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: str
        end subroutine sqlite3_str_reset
    end interface

    interface sqlite3_config
        module procedure :: sqlite3_config_funptr_ptr
        module procedure :: sqlite3_config_int
        module procedure :: sqlite3_config_null
    end interface
contains
    function sqlite3_backup_init(dest, dest_name, source, source_name)
        type(c_ptr),      intent(in) :: dest
        character(len=*), intent(in) :: dest_name
        type(c_ptr),      intent(in) :: source
        character(len=*), intent(in) :: source_name
        type(c_ptr)                  :: sqlite3_backup_init

        sqlite3_backup_init = sqlite3_backup_init_(dest, &
                                                   dest_name // c_null_char, &
                                                   source, &
                                                   source_name // c_null_char)
    end function sqlite3_backup_init

    function sqlite3_bind_parameter_index(stmt, name)
        type(c_ptr),      intent(inout) :: stmt
        character(len=*), intent(in)    :: name
        integer                         :: sqlite3_bind_parameter_index

        sqlite3_bind_parameter_index = sqlite3_bind_parameter_index_(stmt, name // c_null_char)
    end function sqlite3_bind_parameter_index

    function sqlite3_bind_text(stmt, idx, val, destructor)
        !! Binds text to column. This wrapper passes destructor
        !! `SQLITE_TRANSIENT` by default!
        type(c_ptr),      intent(inout)        :: stmt
        integer,          intent(in)           :: idx
        character(len=*), intent(in)           :: val
        integer(kind=i8), intent(in), optional :: destructor
        integer                                :: sqlite3_bind_text

        if (present(destructor)) then
            sqlite3_bind_text = sqlite3_bind_text_(stmt, idx, val, len(val), int(destructor, kind=c_size_t))
            return
        end if

        sqlite3_bind_text = sqlite3_bind_text_(stmt, idx, val, len(val), SQLITE_TRANSIENT)
    end function sqlite3_bind_text

    function sqlite3_close(db)
        type(c_ptr), intent(inout) :: db
        integer                    :: sqlite3_close

        sqlite3_close = sqlite3_close_(db)
        if (sqlite3_close == SQLITE_OK) db = c_null_ptr
    end function sqlite3_close

    function sqlite3_column_database_name(stmt, idx)
        type(c_ptr), intent(inout)    :: stmt
        integer,     intent(in)       :: idx
        character(len=:), allocatable :: sqlite3_column_database_name
        type(c_ptr)                   :: ptr

        ptr = sqlite3_column_database_name_(stmt, idx)
        call c_f_str_ptr(ptr, sqlite3_column_database_name)
    end function sqlite3_column_database_name

    function sqlite3_column_name(stmt, idx)
        type(c_ptr), intent(inout)    :: stmt
        integer,     intent(in)       :: idx
        character(len=:), allocatable :: sqlite3_column_name
        type(c_ptr)                   :: ptr

        ptr = sqlite3_column_name_(stmt, idx)
        call c_f_str_ptr(ptr, sqlite3_column_name)
    end function sqlite3_column_name

    function sqlite3_column_origin_name(stmt, idx)
        type(c_ptr), intent(inout)    :: stmt
        integer,     intent(in)       :: idx
        character(len=:), allocatable :: sqlite3_column_origin_name
        type(c_ptr)                   :: ptr

        ptr = sqlite3_column_origin_name_(stmt, idx)
        call c_f_str_ptr(ptr, sqlite3_column_origin_name)
    end function sqlite3_column_origin_name

    function sqlite3_column_table_name(stmt, idx)
        type(c_ptr), intent(inout)    :: stmt
        integer,     intent(in)       :: idx
        character(len=:), allocatable :: sqlite3_column_table_name
        type(c_ptr)                   :: ptr

        ptr = sqlite3_column_table_name_(stmt, idx)
        call c_f_str_ptr(ptr, sqlite3_column_table_name)
    end function sqlite3_column_table_name

    function sqlite3_column_text(stmt, idx)
        type(c_ptr), intent(inout)    :: stmt
        integer,     intent(in)       :: idx
        character(len=:), allocatable :: sqlite3_column_text
        type(c_ptr)                   :: ptr

        ptr = sqlite3_column_text_(stmt, idx)
        call c_f_str_ptr(ptr, sqlite3_column_text)
    end function sqlite3_column_text

    function sqlite3_config_int(option, arg)
        integer, intent(in) :: option
        integer, intent(in) :: arg
        integer             :: sqlite3_config_int

        sqlite3_config_int = sqlite3_config_int_(option, arg)
    end function sqlite3_config_int

    function sqlite3_config_funptr_ptr(option, funptr, ptr)
        integer,        intent(in) :: option
        type(c_funptr), intent(in) :: funptr
        type(c_ptr),    intent(in) :: ptr
        integer                    :: sqlite3_config_funptr_ptr

        sqlite3_config_funptr_ptr = sqlite3_config_funptr_ptr_(option, funptr, ptr)
    end function sqlite3_config_funptr_ptr

    function sqlite3_config_null(option)
        integer, intent(in) :: option
        integer             :: sqlite3_config_null

        sqlite3_config_null = sqlite3_config_null_(option)
    end function sqlite3_config_null

    function sqlite3_db_name(db, n)
        type(c_ptr), intent(inout)    :: db
        integer,     intent(in)       :: n
        character(len=:), allocatable :: sqlite3_db_name
        type(c_ptr)                   :: ptr

        ptr = sqlite3_db_name_(db, n)
        call c_f_str_ptr(ptr, sqlite3_db_name)
    end function sqlite3_db_name

    function sqlite3_errmsg(db)
        type(c_ptr), intent(inout)    :: db
        character(len=:), allocatable :: sqlite3_errmsg
        type(c_ptr)                   :: ptr

        ptr = sqlite3_errmsg_(db)
        call c_f_str_ptr(ptr, sqlite3_errmsg)
    end function sqlite3_errmsg

    function sqlite3_errstr(err)
        integer, intent(in)           :: err
        character(len=:), allocatable :: sqlite3_errstr
        type(c_ptr)                   :: ptr

        ptr = sqlite3_errstr_(err)
        call c_f_str_ptr(ptr, sqlite3_errstr)
    end function sqlite3_errstr

    function sqlite3_exec(db, sql, callback, client_data, errmsg)
        type(c_ptr),                   intent(inout)         :: db
        character(len=*),              intent(in)            :: sql
        type(c_funptr),                intent(in)            :: callback
        type(c_ptr),                   intent(in)            :: client_data
        character(len=:), allocatable, intent(out), optional :: errmsg
        integer                                              :: sqlite3_exec
        type(c_ptr)                                          :: ptr

        sqlite3_exec = sqlite3_exec_(db, sql // c_null_char, callback, client_data, ptr)
        if (present(errmsg)) call c_f_str_ptr(ptr, errmsg)
    end function sqlite3_exec

    function sqlite3_finalize(stmt)
        type(c_ptr), intent(inout) :: stmt
        integer                    :: sqlite3_finalize

        sqlite3_finalize = sqlite3_finalize_(stmt)
        if (sqlite3_finalize == SQLITE_OK) stmt = c_null_ptr
    end function sqlite3_finalize

    function sqlite3_libversion()
        type(c_ptr)                   :: ptr
        character(len=:), allocatable :: sqlite3_libversion

        ptr = sqlite3_libversion_()
        call c_f_str_ptr(ptr, sqlite3_libversion)
    end function sqlite3_libversion

    function sqlite3_open(file_name, db)
        character(len=*), intent(in)  :: file_name
        type(c_ptr),      intent(out) :: db
        integer                       :: sqlite3_open

        sqlite3_open = sqlite3_open_(file_name // c_null_char, db)
    end function sqlite3_open

    function sqlite3_open_v2(file_name, db, flags, vfs)
        character(len=*), intent(in)           :: file_name
        type(c_ptr),      intent(out)          :: db
        integer,          intent(in)           :: flags
        type(c_ptr),      intent(in), optional :: vfs
        integer                                :: sqlite3_open_v2

        if (present(vfs)) then
            sqlite3_open_v2 = sqlite3_open_v2_(file_name // c_null_char, db, flags, vfs)
            return
        end if

        sqlite3_open_v2 = sqlite3_open_v2_(file_name // c_null_char, db, flags, c_null_ptr)
    end function sqlite3_open_v2

    function sqlite3_prepare(db, sql, stmt)
        type(c_ptr),      intent(inout) :: db
        character(len=*), intent(in)    :: sql
        type(c_ptr),      intent(out)   :: stmt
        integer                         :: sqlite3_prepare

        sqlite3_prepare = sqlite3_prepare_(db, sql, len(sql), stmt, c_null_ptr)
    end function sqlite3_prepare

    function sqlite3_prepare_v2(db, sql, stmt)
        type(c_ptr),      intent(inout) :: db
        character(len=*), intent(in)    :: sql
        type(c_ptr),      intent(out)   :: stmt
        integer                         :: sqlite3_prepare_v2

        sqlite3_prepare_v2 = sqlite3_prepare_v2_(db, sql, len(sql), stmt, c_null_ptr)
    end function sqlite3_prepare_v2

    function sqlite3_prepare_v3(db, sql, prep_flags, stmt)
        type(c_ptr),      intent(inout) :: db
        character(len=*), intent(in)    :: sql
        integer,          intent(in)    :: prep_flags
        type(c_ptr),      intent(out)   :: stmt
        integer                         :: sqlite3_prepare_v3

        sqlite3_prepare_v3 = sqlite3_prepare_v3_(db, sql, len(sql), prep_flags, stmt, c_null_ptr)
    end function sqlite3_prepare_v3

    function sqlite3_sourceid()
        type(c_ptr)                   :: ptr
        character(len=:), allocatable :: sqlite3_sourceid

        ptr = sqlite3_sourceid_()
        call c_f_str_ptr(ptr, sqlite3_sourceid)
    end function sqlite3_sourceid

    function sqlite3_sql(stmt)
        type(c_ptr), intent(inout)    :: stmt
        character(len=:), allocatable :: sqlite3_sql
        type(c_ptr)                   :: ptr

        ptr = sqlite3_sql_(stmt)
        call c_f_str_ptr(ptr, sqlite3_sql)
    end function sqlite3_sql

    function sqlite3_stmt_readonly(stmt)
        type(c_ptr), intent(inout) :: stmt
        logical                    :: sqlite3_stmt_readonly
        integer                    :: i

        i = sqlite3_stmt_readonly_(stmt)
        sqlite3_stmt_readonly = (i /= 0)
    end function sqlite3_stmt_readonly

    function sqlite3_str_value(str)
        type(c_ptr)                   :: str
        type(c_ptr)                   :: ptr
        character(len=:), allocatable :: sqlite3_str_value

        ptr = sqlite3_str_value_(str)
        call c_f_str_ptr(ptr, sqlite3_str_value)
    end function sqlite3_str_value

    subroutine sqlite3_log(ierr_code, str)
        integer,          intent(in) :: ierr_code
        character(len=*), intent(in) :: str

        call sqlite3_log_(ierr_code, str // c_null_char)
    end subroutine sqlite3_log
end module sqlite3
