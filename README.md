# fortran-sqlite3

[![Build and Test](https://github.com/interkosmos/fortran-sqlite3/actions/workflows/build.yml/badge.svg)](https://github.com/interkosmos/fortran-sqlite3/actions/workflows/build.yml)

A work-in-progress collection of Fortran 2018 interface bindings to SQLite 3
(≥ 3.39.0). See [COVERAGE](COVERAGE.md) for an overview of bound functions.

## Build Instructions

Make sure that `libsqlite3` is installed with development headers. On FreeBSD,
run:

```
# pkg install databases/sqlite3
```

On Linux:

```
# apt-get install sqlite3 libsqlite3-0 libsqlite3-dev
```

Or, build SQLite 3 from source:

```
$ git clone --depth 1 https://github.com/sqlite/sqlite.git
$ cd sqlite/
$ ./configure
$ make
$ sudo make install
```

Then, clone the GitHub repository:

```
$ git clone --depth 1 https://github.com/interkosmos/fortran-sqlite3
$ cd fortran-sqlite3/
```

Either build the library with _fpm(1)_ or _make(1)_. Once compiled, link your
Fortran application against `libfortran-sqlite3.a` and `-lsqlite3`.

### Fortran Package Manager

Simply execute the Fortran Package Manager:

```
$ fpm build --profile release
```

The output files are written to `build/`.

### Make

Execute the provided `Makefile`:

```
$ make
```

Pass the preprocessor flag `-DSQLITE_ENABLE_COLUMN_METADATA=1` to add interface
bindings to column meta data functions:

```
$ make PPFLAGS="-DSQLITE_ENABLE_COLUMN_METADATA=1"
```

You may want to override the default compilers by passing the arguments `CC` (C
compiler) and `FC` (Fortran compiler). To install the library and the module
files system-wide, for example, to `/opt`, run:

```
$ make install PREFIX=/opt
```

## Source-Code Documentation

Use [FORD](https://github.com/Fortran-FOSS-Programmers/ford) to generate the
source-code documentation:

```
$ ford ford.md
```

The output files are written to `doc/`.

## Example

The following SQL schema will be created by the example:

```sql
CREATE TABLE example (
    id     INTEGER PRIMARY KEY,
    string TEXT,
    value  INTEGER
);
```

The program opens a database `example.sqlite`, creates the table `example`,
inserts some values, then reads them back in, and prints them to console.

```fortran
! example.f90
program example
    use, intrinsic :: iso_c_binding
    use :: sqlite3
    implicit none (type, external)

    character(len=:), allocatable :: errmsg
    integer                       :: rc
    type(c_ptr)                   :: db
    type(c_ptr)                   :: stmt

    ! Open SQLite database.
    rc = sqlite3_open('example.sqlite', db)

    ! Create table.
    rc = sqlite3_exec(db, "CREATE TABLE example_table (" // &
                          "id     INTEGER PRIMARY KEY," // &
                          "string TEXT," // &
                          "value  INTEGER)", c_null_ptr, c_null_ptr, errmsg)
    if (rc /= SQLITE_OK) print '("sqlite3_exec(): ", a)', errmsg

    ! Create a prepared statement.
    rc = sqlite3_prepare_v2(db, "INSERT INTO example(string, value) VALUES (?, ?)", stmt)

    ! Bind the values to the statement.
    rc = sqlite3_bind_text(stmt, 1, 'one')
    rc = sqlite3_bind_int(stmt, 2, 12345)

    ! Run the statement.
    rc = sqlite3_step(stmt)
    if (rc /= SQLITE_DONE) print '("sqlite3_step(): failed")'

    ! Delete the statement.
    rc = sqlite3_finalize(stmt)

    ! Read values from database.
    rc = sqlite3_prepare_v2(db, "SELECT * FROM example", stmt)

    ! Print rows line by line.
    do while (sqlite3_step(stmt) /= SQLITE_DONE)
        call print_values(stmt, 3)
    end do

    ! Delete the statement.
    rc = sqlite3_finalize(stmt)

    ! Close SQLite handle.
    rc = sqlite3_close(db)
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
                    write (*, '(" not implemented")', advance='no')
            end select
        end do

        print *
    end subroutine print_values
end program example
```

If the library has been installed to `/opt`, then compile, link, and run the
example with:

```
$ gfortran -I/opt/include/libfortran-sqlite3 -o example example.f90 /opt/lib/libfortran-sqlite3.a -lsqlite3
$ ./example
           1         one       12345
```

## Fortran Package Manager

You can add *fortran-sqlite3* as an [fpm](https://github.com/fortran-lang/fpm)
dependency to your `fpm.toml`:

```toml
[dependencies]
fortran-sqlite3 = { git = "https://github.com/interkosmos/fortran-sqlite3.git" }
```

## Compatibility

The Fortran interfaces to the SQLite functions `sqlite3_bind_text()` and
`sqlite3_column_text()` support only 8-bit character strings.

The Fortran wrapper function `sqlite3_bind_text()` selects destructor type
`SQLITE_TRANSIENT` if argument `destructor` is not passed, in contrast to the
SQLite default `SQLITE_STATIC`. Therefore, SQLite will make a copy of the given
value. Otherwise, the passed variable could go out of scope before SQLite was
able to read the string completely, leading to possible data corruption.

The module `sqlite3_util` contains C interoperability functions/interfaces to
convert C char pointer to Fortran allocatable character.

## Licence

ISC
