# Coverage

| C Function Name                  | Bound |
|----------------------------------|-------|
| `sqlite3_aggregate_context`      |       |
| `sqlite3_aggregate_count`        |       |
| `sqlite3_auto_extension`         |       |
| `sqlite3_autovacuum_pages`       |       |
| `sqlite3_backup_finish`          |   ✓   |
| `sqlite3_backup_init`            |   ✓   |
| `sqlite3_backup_pagecount`       |   ✓   |
| `sqlite3_backup_remaining`       |   ✓   |
| `sqlite3_backup_step`            |   ✓   |
| `sqlite3_bind_blob`              |       |
| `sqlite3_bind_blob64`            |       |
| `sqlite3_bind_double`            |   ✓   |
| `sqlite3_bind_int`               |   ✓   |
| `sqlite3_bind_int64`             |   ✓   |
| `sqlite3_bind_null`              |       |
| `sqlite3_bind_parameter_count`   |       |
| `sqlite3_bind_parameter_index`   |       |
| `sqlite3_bind_parameter_name`    |       |
| `sqlite3_bind_pointer`           |       |
| `sqlite3_bind_text`              |   ✓   |
| `sqlite3_bind_text16`            |       |
| `sqlite3_bind_text64`            |       |
| `sqlite3_bind_value`             |       |
| `sqlite3_bind_zeroblob`          |       |
| `sqlite3_bind_zeroblob64`        |       |
| `sqlite3_blob_bytes`             |       |
| `sqlite3_blob_close`             |       |
| `sqlite3_blob_open`              |       |
| `sqlite3_blob_read`              |       |
| `sqlite3_blob_reopen`            |       |
| `sqlite3_blob_write`             |       |
| `sqlite3_busy_handler`           |   ✓   |
| `sqlite3_busy_timeout`           |   ✓   |
| `sqlite3_cancel_auto_extension`  |       |
| `sqlite3_changes`                |       |
| `sqlite3_changes64`              |       |
| `sqlite3_clear_bindings`         |   ✓   |
| `sqlite3_close`                  |   ✓   |
| `sqlite3_close_v2`               |       |
| `sqlite3_collation_needed`       |       |
| `sqlite3_collation_needed16`     |       |
| `sqlite3_column_blob`            |       |
| `sqlite3_column_bytes`           |       |
| `sqlite3_column_bytes16`         |       |
| `sqlite3_column_count`           |       |
| `sqlite3_column_database_name`   |       |
| `sqlite3_column_database_name16` |       |
| `sqlite3_column_decltype`        |       |
| `sqlite3_column_decltype16`      |       |
| `sqlite3_column_double`          |   ✓   |
| `sqlite3_column_int`             |   ✓   |
| `sqlite3_column_int64`           |   ✓   |
| `sqlite3_column_name`            |       |
| `sqlite3_column_name16`          |       |
| `sqlite3_column_origin_name`     |       |
| `sqlite3_column_origin_name16`   |       |
| `sqlite3_column_table_name`      |       |
| `sqlite3_column_table_name16`    |       |
| `sqlite3_column_text`            |   ✓   |
| `sqlite3_column_text16`          |       |
| `sqlite3_column_type`            |   ✓   |
| `sqlite3_column_value`           |       |
| `sqlite3_commit_hook`            |       |
| `sqlite3_compileoption_get`      |       |
| `sqlite3_compileoption_used`     |       |
| `sqlite3_complete`               |       |
| `sqlite3_complete16`             |       |
| `sqlite3_config`                 |   ✓   |
| `sqlite3_context_db_handle`      |       |
| `sqlite3_create_collation`       |       |
| `sqlite3_create_collation16`     |       |
| `sqlite3_create_collation_v2`    |       |
| `sqlite3_create_filename`        |       |
| `sqlite3_create_function`        |       |
| `sqlite3_create_function16`      |       |
| `sqlite3_create_function_v2`     |       |
| `sqlite3_create_module`          |       |
| `sqlite3_create_module_v2`       |       |
| `sqlite3_create_window_function` |       |
| `sqlite3_data_count`             |   ✓   |
| `sqlite3_database_file_object`   |       |
| `sqlite3_db_cacheflush`          |       |
| `sqlite3_db_config`              |       |
| `sqlite3_db_filename`            |       |
| `sqlite3_db_handle`              |       |
| `sqlite3_db_mutex`               |       |
| `sqlite3_db_name`                |   ✓   |
| `sqlite3_db_readonly`            |       |
| `sqlite3_db_release_memory`      |       |
| `sqlite3_db_status`              |   ✓   |
| `sqlite3_declare_vtab`           |       |
| `sqlite3_deserialize`            |       |
| `sqlite3_drop_modules`           |       |
| `sqlite3_enable_load_extension`  |       |
| `sqlite3_enable_shared_cache`    |       |
| `sqlite3_errcode`                |   ✓   |
| `sqlite3_errmsg`                 |   ✓   |
| `sqlite3_errmsg16`               |       |
| `sqlite3_error_offset`           |       |
| `sqlite3_errstr`                 |       |
| `sqlite3_exec`                   |   ✓   |
| `sqlite3_expanded_sql`           |       |
| `sqlite3_expired`                |       |
| `sqlite3_extended_errcode`       |       |
| `sqlite3_extended_result_codes`  |       |
| `sqlite3_file_control`           |       |
| `sqlite3_filename_database`      |       |
| `sqlite3_filename_journal`       |       |
| `sqlite3_filename_wal`           |       |
| `sqlite3_finalize`               |   ✓   |
| `sqlite3_free`                   |   ✓   |
| `sqlite3_free_filename`          |       |
| `sqlite3_free_table`             |       |
| `sqlite3_get_autocommit`         |       |
| `sqlite3_get_auxdata`            |       |
| `sqlite3_get_table`              |       |
| `sqlite3_global_recover`         |       |
| `sqlite3_hard_heap_limit64`      |       |
| `sqlite3_initialize`             |   ✓   |
| `sqlite3_interrupt`              |       |
| `sqlite3_keyword_check`          |       |
| `sqlite3_keyword_count`          |       |
| `sqlite3_keyword_name`           |       |
| `sqlite3_last_insert_rowid`      |   ✓   |
| `sqlite3_libversion`             |   ✓   |
| `sqlite3_libversion_number`      |   ✓   |
| `sqlite3_limit`                  |       |
| `sqlite3_load_extension`         |       |
| `sqlite3_log`                    |   ✓   |
| `sqlite3_malloc`                 |       |
| `sqlite3_malloc64`               |       |
| `sqlite3_memory_alarm`           |       |
| `sqlite3_memory_highwater`       |       |
| `sqlite3_memory_used`            |       |
| `sqlite3_mprintf`                |       |
| `sqlite3_msize`                  |       |
| `sqlite3_mutex_alloc`            |       |
| `sqlite3_mutex_enter`            |       |
| `sqlite3_mutex_free`             |       |
| `sqlite3_mutex_held`             |       |
| `sqlite3_mutex_leave`            |       |
| `sqlite3_mutex_notheld`          |       |
| `sqlite3_mutex_try`              |       |
| `sqlite3_next_stmt`              |       |
| `sqlite3_normalized_sql`         |       |
| `sqlite3_open`                   |   ✓   |
| `sqlite3_open16`                 |       |
| `sqlite3_open_v2`                |   ✓   |
| `sqlite3_os_end`                 |       |
| `sqlite3_os_init`                |       |
| `sqlite3_overload_function`      |       |
| `sqlite3_prepare`                |   ✓   |
| `sqlite3_prepare16`              |       |
| `sqlite3_prepare16_v2`           |       |
| `sqlite3_prepare16_v3`           |       |
| `sqlite3_prepare_v2`             |   ✓   |
| `sqlite3_prepare_v3`             |       |
| `sqlite3_preupdate_blobwrite`    |       |
| `sqlite3_preupdate_count`        |       |
| `sqlite3_preupdate_depth`        |       |
| `sqlite3_preupdate_hook`         |       |
| `sqlite3_preupdate_new`          |       |
| `sqlite3_preupdate_old`          |       |
| `sqlite3_profile`                |       |
| `sqlite3_progress_handler`       |       |
| `sqlite3_randomness`             |       |
| `sqlite3_realloc`                |       |
| `sqlite3_realloc64`              |       |
| `sqlite3_release_memory`         |       |
| `sqlite3_reset`                  |   ✓   |
| `sqlite3_reset_auto_extension`   |       |
| `sqlite3_result_blob`            |       |
| `sqlite3_result_blob64`          |       |
| `sqlite3_result_double`          |       |
| `sqlite3_result_error`           |       |
| `sqlite3_result_error16`         |       |
| `sqlite3_result_error_code`      |       |
| `sqlite3_result_error_nomem`     |       |
| `sqlite3_result_error_toobig`    |       |
| `sqlite3_result_int`             |       |
| `sqlite3_result_int64`           |       |
| `sqlite3_result_null`            |       |
| `sqlite3_result_pointer`         |       |
| `sqlite3_result_subtype`         |       |
| `sqlite3_result_text`            |       |
| `sqlite3_result_text16`          |       |
| `sqlite3_result_text16be`        |       |
| `sqlite3_result_text16le`        |       |
| `sqlite3_result_text64`          |       |
| `sqlite3_result_value`           |       |
| `sqlite3_result_zeroblob`        |       |
| `sqlite3_result_zeroblob64`      |       |
| `sqlite3_rollback_hook`          |       |
| `sqlite3_serialize`              |       |
| `sqlite3_set_authorizer`         |       |
| `sqlite3_set_auxdata`            |       |
| `sqlite3_set_last_insert_rowid`  |       |
| `sqlite3_shutdown`               |   ✓   |
| `sqlite3_sleep`                  |   ✓   |
| `sqlite3_snapshot_cmp`           |       |
| `sqlite3_snapshot_free`          |       |
| `sqlite3_snapshot_get`           |       |
| `sqlite3_snapshot_open`          |       |
| `sqlite3_snapshot_recover`       |       |
| `sqlite3_snprintf`               |       |
| `sqlite3_soft_heap_limit`        |       |
| `sqlite3_soft_heap_limit64`      |       |
| `sqlite3_sourceid`               |   ✓   |
| `sqlite3_sql`                    |       |
| `sqlite3_status`                 |   ✓   |
| `sqlite3_status64`               |       |
| `sqlite3_step`                   |   ✓   |
| `sqlite3_stmt_busy`              |       |
| `sqlite3_stmt_isexplain`         |       |
| `sqlite3_stmt_readonly`          |       |
| `sqlite3_stmt_scanstatus`        |       |
| `sqlite3_stmt_scanstatus_reset`  |       |
| `sqlite3_stmt_status`            |       |
| `sqlite3_str_append`             |   ✓   |
| `sqlite3_str_appendall`          |   ✓   |
| `sqlite3_str_appendchar`         |       |
| `sqlite3_str_appendf`            |       |
| `sqlite3_str_errcode`            |       |
| `sqlite3_str_finish`             |   ✓   |
| `sqlite3_str_length`             |   ✓   |
| `sqlite3_str_new`                |   ✓   |
| `sqlite3_str_reset`              |   ✓   |
| `sqlite3_str_value`              |   ✓   |
| `sqlite3_str_vappendf`           |       |
| `sqlite3_strglob`                |       |
| `sqlite3_stricmp`                |       |
| `sqlite3_strlike`                |       |
| `sqlite3_strnicmp`               |       |
| `sqlite3_system_errno`           |       |
| `sqlite3_table_column_metadata`  |       |
| `sqlite3_test_control`           |       |
| `sqlite3_thread_cleanup`         |       |
| `sqlite3_threadsafe`             |   ✓   |
| `sqlite3_total_changes`          |       |
| `sqlite3_total_changes64`        |       |
| `sqlite3_trace`                  |       |
| `sqlite3_trace_v2`               |       |
| `sqlite3_transfer_bindings`      |       |
| `sqlite3_txn_state`              |       |
| `sqlite3_unlock_notify`          |       |
| `sqlite3_update_hook`            |   ✓   |
| `sqlite3_uri_boolean`            |       |
| `sqlite3_uri_int64`              |       |
| `sqlite3_uri_key`                |       |
| `sqlite3_uri_parameter`          |       |
| `sqlite3_user_data`              |       |
| `sqlite3_value_blob`             |       |
| `sqlite3_value_bytes`            |       |
| `sqlite3_value_bytes16`          |       |
| `sqlite3_value_double`           |       |
| `sqlite3_value_dup`              |       |
| `sqlite3_value_encoding`         |       |
| `sqlite3_value_free`             |       |
| `sqlite3_value_frombind`         |       |
| `sqlite3_value_int`              |       |
| `sqlite3_value_int64`            |       |
| `sqlite3_value_nochange`         |       |
| `sqlite3_value_numeric_type`     |       |
| `sqlite3_value_pointer`          |       |
| `sqlite3_value_subtype`          |       |
| `sqlite3_value_text`             |       |
| `sqlite3_value_text16`           |       |
| `sqlite3_value_text16be`         |       |
| `sqlite3_value_text16le`         |       |
| `sqlite3_value_type`             |       |
| `sqlite3_version`                |       |
| `sqlite3_vfs_find`               |       |
| `sqlite3_vfs_register`           |       |
| `sqlite3_vfs_unregister`         |       |
| `sqlite3_vmprintf`               |       |
| `sqlite3_vsnprintf`              |       |
| `sqlite3_vtab_collation`         |       |
| `sqlite3_vtab_config`            |       |
| `sqlite3_vtab_distinct`          |       |
| `sqlite3_vtab_in`                |       |
| `sqlite3_vtab_in_first`          |       |
| `sqlite3_vtab_in_next`           |       |
| `sqlite3_vtab_nochange`          |       |
| `sqlite3_vtab_on_conflict`       |       |
| `sqlite3_vtab_rhs_value`         |       |
| `sqlite3_wal_autocheckpoint`     |       |
| `sqlite3_wal_checkpoint`         |       |
| `sqlite3_wal_checkpoint_v2`      |       |
| `sqlite3_wal_hook`               |       |
| `sqlite3_win32_set_directory`    |       |
| `sqlite3_win32_set_directory16`  |       |
| `sqlite3_win32_set_directory8`   |       |