.POSIX:
.SUFFIXES:

FC      = gfortran
AR      = ar
PREFIX  = /usr/local
DEBUG   = -Wno-unused-dummy-argument #-ggdb3 -O0

FFLAGS  = $(DEBUG) -Wall -fmax-errors=1 -fcheck=all
LDFLAGS = -I$(PREFIX)/include/ -L$(PREFIX)/lib/
LDLIBS  = -lsqlite3
ARFLAGS = rcs
TARGET  = libfortran-sqlite3.a
TEST    = test_sqlite

.PHONY: all clean test

all: $(TARGET)
test: $(TEST)

$(TARGET):
	$(FC) $(FFLAGS) -c src/sqlite_util.f90
	$(FC) $(FFLAGS) -c src/sqlite.f90
	$(AR) $(ARFLAGS) $(TARGET) sqlite_util.o sqlite.o

$(TEST): $(TARGET)
	$(FC) $(FFLAGS) $(LDFLAGS) -o test_sqlite test/test_sqlite.f90 $(TARGET) $(LDLIBS)

clean:
	if [ `ls -1 *.mod 2>/dev/null | wc -l` -gt 0 ]; then rm *.mod; fi
	if [ `ls -1 *.o 2>/dev/null | wc -l` -gt 0 ]; then rm *.o; fi
	if [ -e $(TARGET) ]; then rm $(TARGET); fi
	if [ -e $(TEST) ]; then rm $(TEST); fi
