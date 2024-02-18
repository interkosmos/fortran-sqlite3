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

int sqlite3_config_funptr_ptr_(int, void *, void *);
int sqlite3_config_int_(int, int);
int sqlite3_config_null_(int);
void sqlite3_log_(int, const char *);

int sqlite3_config_funptr_ptr_(int option, void *funptr, void *ptr)
{
    return sqlite3_config(option, funptr, ptr);
}

int sqlite3_config_int_(int option, int arg)
{
    return sqlite3_config(option, arg);
}

int sqlite3_config_null_(int option)
{
    return sqlite3_config(option);
}

void sqlite3_log_(int iErrCode, const char *str)
{
    sqlite3_log(iErrCode, "%s", str);
}

#ifdef __cplusplus
}
#endif
