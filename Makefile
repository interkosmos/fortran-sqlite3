.POSIX:
.SUFFIXES:

CC      = gcc
FC      = gfortran
AR      = ar
RM      = /bin/rm
PREFIX  = /usr/local

DEBUG   = -g -O0 -Wall -std=f2018
RELEASE = -O2
FLAGS   = $(RELEASE)

CFLAGS  = $(FLAGS) -I$(PREFIX)/include
FFLAGS  = $(FLAGS)
PPFLAGS = -DSQLITE_ENABLE_COLUMN_METADATA=0
LDFLAGS = -L$(PREFIX)/lib
LDLIBS  = -lsqlite3
ARFLAGS = rcs
INCDIR  = $(PREFIX)/include/libfortran-sqlite3
LIBDIR  = $(PREFIX)/lib
TARGET  = libfortran-sqlite3.a
SRC     = src/sqlite3.F90 src/sqlite3_macro.c src/sqlite3_util.F90
OBJ     = sqlite3.o sqlite3_macro.o sqlite3_util.o

.PHONY: all clean install test

all: $(TARGET)

test: test_sqlite3

$(TARGET): $(SRC)
	$(CC) $(CFLAGS) -c src/sqlite3_macro.c
	$(FC) $(FFLAGS) $(PPFLAGS) -c src/sqlite3_util.F90
	$(FC) $(FFLAGS) $(PPFLAGS) -c src/sqlite3.F90
	$(AR) $(ARFLAGS) $(TARGET) $(OBJ)

install: $(TARGET)
	@echo "--- Installing $(TARGET) to $(LIBDIR)/ ..."
	install -d $(LIBDIR)
	install -m 644 $(TARGET) $(LIBDIR)/
	@echo "--- Installing module files to $(INCDIR)/ ..."
	install -d $(INCDIR)
	install -m 644 *.mod $(INCDIR)/

test_sqlite3: $(TARGET) test/test_sqlite3.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -o test_sqlite3 test/test_sqlite3.f90 $(TARGET) $(LDLIBS)

clean:
	$(RM) -rf *.mod
	$(RM) -rf *.o
	$(RM) -rf $(TARGET)
	$(RM) -rf test_sqlite3
	$(RM) -rf test.sqlite
