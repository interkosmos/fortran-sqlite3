/* sqlite3_macro.c
 *
 * Wrapper functions for compatibility with variadic SQLite 3 procedures.
 *
 * Author:  Philipp Engel
 * Licence: ISC
 * */
#include <sqlite3.h>

#ifdef __cplusplus
extern "C" {
#endif

int sqlite3_config_int_(int);
int sqlite3_config_int_funptr_ptr_(int, void *, void *);
int sqlite3_config_int_int_(int, int);
int sqlite3_config_int_int64_(int, sqlite3_int64);
int sqlite3_config_int_int_int_(int, int, int);
int sqlite3_config_int_ptr_(int, void *);
int sqlite3_config_int_ptr_int_int_(int, void *, int, int);
void sqlite3_log_(int, const char *);

int sqlite3_config_int_(int option)
{
    return sqlite3_config(option);
}

int sqlite3_config_int_funptr_ptr_(int option, void *funptr, void *ptr)
{
    return sqlite3_config(option, funptr, ptr);
}

int sqlite3_config_int_int_(int option, int arg)
{
    return sqlite3_config(option, arg);
}

int sqlite3_config_int_int64_(int option, sqlite3_int64 arg)
{
    return sqlite3_config(option, arg);
}

int sqlite3_config_int_int_int_(int option, int arg1, int arg2)
{
    return sqlite3_config(option, arg1, arg2);
}

int sqlite3_config_int_ptr_(int option, void *ptr)
{
    return sqlite3_config(option, ptr);
}

int sqlite3_config_int_ptr_int_int_(int option, void *ptr, int arg1, int arg2)
{
    return sqlite3_config(option, ptr, arg1, arg2);
}

void sqlite3_log_(int iErrCode, const char *str)
{
    sqlite3_log(iErrCode, "%s", str);
}

#ifdef __cplusplus
}
#endif
