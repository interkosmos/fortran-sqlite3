.POSIX:
.SUFFIXES:

CC      = gcc
FC      = gfortran
AR      = ar
PREFIX  = /usr/local

DEBUG   = -g -O0 -Wall
RELEASE = -O2 -march=native

CFLAGS  = $(RELEASE)
FFLAGS  = $(RELEASE)
LDFLAGS = -I$(PREFIX)/include -L$(PREFIX)/lib
LDLIBS  = -lsqlite3
ARFLAGS = rcs
TARGET  = libfortran-sqlite3.a

.PHONY: all clean test

all: $(TARGET)

test: test_sqlite3

$(TARGET): src/sqlite3_util.f90 src/sqlite3.f90
	$(CC) $(CFLAGS) -c src/sqlite3_macro.c
	$(FC) $(FFLAGS) -c src/sqlite3_util.f90
	$(FC) $(FFLAGS) -c src/sqlite3.f90
	$(AR) $(ARFLAGS) $(TARGET) sqlite3.o sqlite3_macro.o sqlite3_util.o

test_sqlite3: $(TARGET) test/test_sqlite3.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -o test_sqlite3 test/test_sqlite3.f90 $(TARGET) $(LDLIBS)

clean:
	if [ `ls -1 *.mod 2>/dev/null | wc -l` -gt 0 ]; then rm *.mod; fi
	if [ `ls -1 *.o 2>/dev/null | wc -l` -gt 0 ]; then rm *.o; fi
	if [ -e $(TARGET) ]; then rm $(TARGET); fi
	if [ -e test_sqlite3 ]; then rm test_sqlite3; fi
