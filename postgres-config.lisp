
(in-package :cl-sysop)

(defpackage :pg) ; for postgres config properties

;; It would be better to do this on a live database
(defun generate-postgres-config-class (query-results)
  (with-output-to-string (stream)
    (format stream "(defclass postgres-config ()~%")
    (mapcar (lambda (x)
              (setf x (cl-ppcre:regex-replace-all "\\s+$" x ""))
              (let ((property (cl-ppcre:regex-replace "\\s.*" x ""))
                    (description (cl-ppcre:regex-replace "^[^\\s]+\\s+" x "")))

                (format stream "   (pg::~A :initarg :~A ~%    :documentation ~S)~%"
                        property
                        (cl-ppcre:regex-replace-all "_" property "-")
                        description)
            
                )
              )
            (cl-ppcre:split "\\n"
                            query-results))
    (format stream "  )~%~%~%")))

;; use 'show all;' in psql to get the list of settings

;; ... which results in this:-
(defclass postgres-config ()
  (
   (pg::DateStyle :initarg :DateStyle 
                  :documentation "Sets the display format for date and time values.")
   (pg::IntervalStyle :initarg :IntervalStyle 
                      :documentation "Sets the display format for interval values.")
   (pg::TimeZone :initarg :TimeZone 
                 :documentation "Sets the time zone for displaying and interpreting time stamps.")
   (pg::allow_system_table_mods :initarg :allow-system-table-mods 
                                :documentation "Allows modifications of the structure of system tables.")
   (pg::application_name :initarg :application-name 
                         :documentation "Sets the application name to be reported in statistics and logs.")
   (pg::archive_cleanup_command :initarg :archive-cleanup-command 
                                :documentation "Sets the shell command that will be executed at every restart point.")
   (pg::archive_command :initarg :archive-command 
                        :documentation "Sets the shell command that will be called to archive a WAL file.")
   (pg::archive_mode :initarg :archive-mode 
                     :documentation "Allows archiving of WAL files using archive_command.")
   (pg::archive_timeout :initarg :archive-timeout 
                        :documentation "Forces a switch to the next WAL file if a new file has not been started within N seconds.")
   (pg::array_nulls :initarg :array-nulls 
                    :documentation "Enable input of NULL elements in arrays.")
   (pg::authentication_timeout :initarg :authentication-timeout 
                               :documentation "Sets the maximum allowed time to complete client authentication.")
   (pg::autovacuum :initarg :autovacuum 
                   :documentation "Starts the autovacuum subprocess.")
   (pg::autovacuum_analyze_scale_factor :initarg :autovacuum-analyze-scale-factor 
                                        :documentation "Number of tuple inserts, updates, or deletes prior to analyze as a fraction of reltuples.")
   (pg::autovacuum_analyze_threshold :initarg :autovacuum-analyze-threshold 
                                     :documentation "Minimum number of tuple inserts, updates, or deletes prior to analyze.")
   (pg::autovacuum_freeze_max_age :initarg :autovacuum-freeze-max-age 
                                  :documentation "Age at which to autovacuum a table to prevent transaction ID wraparound.")
   (pg::autovacuum_max_workers :initarg :autovacuum-max-workers 
                               :documentation "Sets the maximum number of simultaneously running autovacuum worker processes.")
   (pg::autovacuum_multixact_freeze_max_age :initarg :autovacuum-multixact-freeze-max-age 
                                            :documentation "Multixact age at which to autovacuum a table to prevent multixact wraparound.")
   (pg::autovacuum_naptime :initarg :autovacuum-naptime 
                           :documentation "Time to sleep between autovacuum runs.")
   (pg::autovacuum_vacuum_cost_delay :initarg :autovacuum-vacuum-cost-delay 
                                     :documentation "Vacuum cost delay in milliseconds, for autovacuum.")
   (pg::autovacuum_vacuum_cost_limit :initarg :autovacuum-vacuum-cost-limit 
                                     :documentation "Vacuum cost amount available before napping, for autovacuum.")
   (pg::autovacuum_vacuum_scale_factor :initarg :autovacuum-vacuum-scale-factor 
                                       :documentation "Number of tuple updates or deletes prior to vacuum as a fraction of reltuples.")
   (pg::autovacuum_vacuum_threshold :initarg :autovacuum-vacuum-threshold 
                                    :documentation "Minimum number of tuple updates or deletes prior to vacuum.")
   (pg::autovacuum_work_mem :initarg :autovacuum-work-mem 
                            :documentation "Sets the maximum memory to be used by each autovacuum worker process.")
   (pg::backend_flush_after :initarg :backend-flush-after 
                            :documentation "Number of pages after which previously performed writes are flushed to disk.")
   (pg::backslash_quote :initarg :backslash-quote 
                        :documentation "Sets whether \"'\" is allowed in string literals.")
   (pg::bgwriter_delay :initarg :bgwriter-delay 
                       :documentation "Background writer sleep time between rounds.")
   (pg::bgwriter_flush_after :initarg :bgwriter-flush-after 
                             :documentation "Number of pages after which previously performed writes are flushed to disk.")
   (pg::bgwriter_lru_maxpages :initarg :bgwriter-lru-maxpages 
                              :documentation "Background writer maximum number of LRU pages to flush per round.")
   (pg::bgwriter_lru_multiplier :initarg :bgwriter-lru-multiplier 
                                :documentation "Multiple of the average buffer usage to free per round.")
   (pg::block_size :initarg :block-size 
                   :documentation "Shows the size of a disk block.")
   (pg::bonjour :initarg :bonjour 
                :documentation "Enables advertising the server via Bonjour.")
   (pg::bonjour_name :initarg :bonjour-name 
                     :documentation "Sets the Bonjour service name.")
   (pg::bytea_output :initarg :bytea-output 
                     :documentation "Sets the output format for bytea.")
   (pg::check_function_bodies :initarg :check-function-bodies 
                              :documentation "Check function bodies during CREATE FUNCTION.")
   (pg::checkpoint_completion_target :initarg :checkpoint-completion-target 
                                     :documentation "Time spent flushing dirty buffers during checkpoint, as fraction of checkpoint interval.")
   (pg::checkpoint_flush_after :initarg :checkpoint-flush-after 
                               :documentation "Number of pages after which previously performed writes are flushed to disk.")
   (pg::checkpoint_timeout :initarg :checkpoint-timeout 
                           :documentation "Sets the maximum time between automatic WAL checkpoints.")
   (pg::checkpoint_warning :initarg :checkpoint-warning 
                           :documentation "Enables warnings if checkpoint segments are filled more frequently than this.")
   (pg::client_encoding :initarg :client-encoding 
                        :documentation "Sets the client's character set encoding.")
   (pg::client_min_messages :initarg :client-min-messages 
                            :documentation "Sets the message levels that are sent to the client.")
   (pg::cluster_name :initarg :cluster-name 
                     :documentation "Sets the name of the cluster, which is included in the process title.")
   (pg::commit_delay :initarg :commit-delay 
                     :documentation "Sets the delay in microseconds between transaction commit and flushing WAL to disk.")
   (pg::commit_siblings :initarg :commit-siblings 
                        :documentation "Sets the minimum concurrent open transactions before performing commit_delay.")
   (pg::config_file :initarg :config-file 
                    :documentation "Sets the server's main configuration file.")
   (pg::constraint_exclusion :initarg :constraint-exclusion 
                             :documentation "Enables the planner to use constraints to optimize queries.")
   (pg::cpu_index_tuple_cost :initarg :cpu-index-tuple-cost 
                             :documentation "Sets the planner's estimate of the cost of processing each index entry during an index scan.")
   (pg::cpu_operator_cost :initarg :cpu-operator-cost 
                          :documentation "Sets the planner's estimate of the cost of processing each operator or function call.")
   (pg::cpu_tuple_cost :initarg :cpu-tuple-cost 
                       :documentation "Sets the planner's estimate of the cost of processing each tuple (row).")
   (pg::cursor_tuple_fraction :initarg :cursor-tuple-fraction 
                              :documentation "Sets the planner's estimate of the fraction of a cursor's rows that will be retrieved.")
   (pg::data_checksums :initarg :data-checksums 
                       :documentation "Shows whether data checksums are turned on for this cluster.")
   (pg::data_directory :initarg :data-directory 
                       :documentation "Sets the server's data directory.")
   (pg::data_directory_mode :initarg :data-directory-mode 
                            :documentation "Mode of the data directory.")
   (pg::data_sync_retry :initarg :data-sync-retry 
                        :documentation "Whether to continue running after a failure to sync data files.")
   (pg::db_user_namespace :initarg :db-user-namespace 
                          :documentation "Enables per-database user names.")
   (pg::deadlock_timeout :initarg :deadlock-timeout 
                         :documentation "Sets the time to wait on a lock before checking for deadlock.")
   (pg::debug_assertions :initarg :debug-assertions 
                         :documentation "Shows whether the running server has assertion checks enabled.")
   (pg::debug_pretty_print :initarg :debug-pretty-print 
                           :documentation "Indents parse and plan tree displays.")
   (pg::debug_print_parse :initarg :debug-print-parse 
                          :documentation "Logs each query's parse tree.")
   (pg::debug_print_plan :initarg :debug-print-plan 
                         :documentation "Logs each query's execution plan.")
   (pg::debug_print_rewritten :initarg :debug-print-rewritten 
                              :documentation "Logs each query's rewritten parse tree.")
   (pg::default_statistics_target :initarg :default-statistics-target 
                                  :documentation "Sets the default statistics target.")
   (pg::default_table_access_method :initarg :default-table-access-method 
                                    :documentation "Sets the default table access method for new tables.")
   (pg::default_tablespace :initarg :default-tablespace 
                           :documentation "Sets the default tablespace to create tables and indexes in.")
   (pg::default_text_search_config :initarg :default-text-search-config 
                                   :documentation "Sets default text search configuration.")
   (pg::default_transaction_deferrable :initarg :default-transaction-deferrable 
                                       :documentation "Sets the default deferrable status of new transactions.")
   (pg::default_transaction_isolation :initarg :default-transaction-isolation 
                                      :documentation "Sets the transaction isolation level of each new transaction.")
   (pg::default_transaction_read_only :initarg :default-transaction-read-only 
                                      :documentation "Sets the default read-only status of new transactions.")
   (pg::dynamic_library_path :initarg :dynamic-library-path 
                             :documentation "Sets the path for dynamically loadable modules.")
   (pg::dynamic_shared_memory_type :initarg :dynamic-shared-memory-type 
                                   :documentation "Selects the dynamic shared memory implementation used.")
   (pg::effective_cache_size :initarg :effective-cache-size 
                             :documentation "Sets the planner's assumption about the total size of the data caches.")
   (pg::effective_io_concurrency :initarg :effective-io-concurrency 
                                 :documentation "Number of simultaneous requests that can be handled efficiently by the disk subsystem.")
   (pg::enable_bitmapscan :initarg :enable-bitmapscan 
                          :documentation "Enables the planner's use of bitmap-scan plans.")
   (pg::enable_gathermerge :initarg :enable-gathermerge 
                           :documentation "Enables the planner's use of gather merge plans.")
   (pg::enable_hashagg :initarg :enable-hashagg 
                       :documentation "Enables the planner's use of hashed aggregation plans.")
   (pg::enable_hashjoin :initarg :enable-hashjoin 
                        :documentation "Enables the planner's use of hash join plans.")
   (pg::enable_indexonlyscan :initarg :enable-indexonlyscan 
                             :documentation "Enables the planner's use of index-only-scan plans.")
   (pg::enable_indexscan :initarg :enable-indexscan 
                         :documentation "Enables the planner's use of index-scan plans.")
   (pg::enable_material :initarg :enable-material 
                        :documentation "Enables the planner's use of materialization.")
   (pg::enable_mergejoin :initarg :enable-mergejoin 
                         :documentation "Enables the planner's use of merge join plans.")
   (pg::enable_nestloop :initarg :enable-nestloop 
                        :documentation "Enables the planner's use of nested-loop join plans.")
   (pg::enable_parallel_append :initarg :enable-parallel-append 
                               :documentation "Enables the planner's use of parallel append plans.")
   (pg::enable_parallel_hash :initarg :enable-parallel-hash 
                             :documentation "Enables the planner's use of parallel hash plans.")
   (pg::enable_partition_pruning :initarg :enable-partition-pruning 
                                 :documentation "Enables plan-time and run-time partition pruning.")
   (pg::enable_partitionwise_aggregate :initarg :enable-partitionwise-aggregate 
                                       :documentation "Enables partitionwise aggregation and grouping.")
   (pg::enable_partitionwise_join :initarg :enable-partitionwise-join 
                                  :documentation "Enables partitionwise join.")
   (pg::enable_seqscan :initarg :enable-seqscan 
                       :documentation "Enables the planner's use of sequential-scan plans.")
   (pg::enable_sort :initarg :enable-sort 
                    :documentation "Enables the planner's use of explicit sort steps.")
   (pg::enable_tidscan :initarg :enable-tidscan 
                       :documentation "Enables the planner's use of TID scan plans.")
   (pg::escape_string_warning :initarg :escape-string-warning 
                              :documentation "Warn about backslash escapes in ordinary string literals.")
   (pg::event_source :initarg :event-source 
                     :documentation "Sets the application name used to identify PostgreSQL messages in the event log.")
   (pg::exit_on_error :initarg :exit-on-error 
                      :documentation "Terminate session on any error.")
   (pg::external_pid_file :initarg :external-pid-file 
                          :documentation "Writes the postmaster PID to the specified file.")
   (pg::extra_float_digits :initarg :extra-float-digits 
                           :documentation "Sets the number of digits displayed for floating-point values.")
   (pg::force_parallel_mode :initarg :force-parallel-mode 
                            :documentation "Forces use of parallel query facilities.")
   (pg::from_collapse_limit :initarg :from-collapse-limit 
                            :documentation "Sets the FROM-list size beyond which subqueries are not collapsed.")
   (pg::fsync :initarg :fsync 
              :documentation "Forces synchronization of updates to disk.")
   (pg::full_page_writes :initarg :full-page-writes 
                         :documentation "Writes full pages to WAL when first modified after a checkpoint.")
   (pg::geqo :initarg :geqo 
             :documentation "Enables genetic query optimization.")
   (pg::geqo_effort :initarg :geqo-effort 
                    :documentation "GEQO: effort is used to set the default for other GEQO parameters.")
   (pg::geqo_generations :initarg :geqo-generations 
                         :documentation "GEQO: number of iterations of the algorithm.")
   (pg::geqo_pool_size :initarg :geqo-pool-size 
                       :documentation "GEQO: number of individuals in the population.")
   (pg::geqo_seed :initarg :geqo-seed 
                  :documentation "GEQO: seed for random path selection.")
   (pg::geqo_selection_bias :initarg :geqo-selection-bias 
                            :documentation "GEQO: selective pressure within the population.")
   (pg::geqo_threshold :initarg :geqo-threshold 
                       :documentation "Sets the threshold of FROM items beyond which GEQO is used.")
   (pg::gin_fuzzy_search_limit :initarg :gin-fuzzy-search-limit 
                               :documentation "Sets the maximum allowed result for exact search by GIN.")
   (pg::gin_pending_list_limit :initarg :gin-pending-list-limit 
                               :documentation "Sets the maximum size of the pending list for GIN index.")
   (pg::hba_file :initarg :hba-file 
                 :documentation "Sets the server's \"hba\" configuration file.")
   (pg::hot_standby :initarg :hot-standby 
                    :documentation "Allows connections and queries during recovery.")
   (pg::hot_standby_feedback :initarg :hot-standby-feedback 
                             :documentation "Allows feedback from a hot standby to the primary that will avoid query conflicts.")
   (pg::huge_pages :initarg :huge-pages 
                   :documentation "Use of huge pages on Linux or Windows.")
   (pg::ident_file :initarg :ident-file 
                   :documentation "Sets the server's \"ident\" configuration file.")
   (pg::idle_in_transaction_session_timeout :initarg :idle-in-transaction-session-timeout 
                                            :documentation "Sets the maximum allowed duration of any idling transaction.")
   (pg::ignore_checksum_failure :initarg :ignore-checksum-failure 
                                :documentation "Continues processing after a checksum failure.")
   (pg::ignore_system_indexes :initarg :ignore-system-indexes 
                              :documentation "Disables reading from system indexes.")
   (pg::integer_datetimes :initarg :integer-datetimes 
                          :documentation "Datetimes are integer based.")
   (pg::jit :initarg :jit 
            :documentation "Allow JIT compilation.")
   (pg::jit_above_cost :initarg :jit-above-cost 
                       :documentation "Perform JIT compilation if query is more expensive.")
   (pg::jit_debugging_support :initarg :jit-debugging-support 
                              :documentation "Register JIT compiled function with debugger.")
   (pg::jit_dump_bitcode :initarg :jit-dump-bitcode 
                         :documentation "Write out LLVM bitcode to facilitate JIT debugging.")
   (pg::jit_expressions :initarg :jit-expressions 
                        :documentation "Allow JIT compilation of expressions.")
   (pg::jit_inline_above_cost :initarg :jit-inline-above-cost 
                              :documentation "Perform JIT inlining if query is more expensive.")
   (pg::jit_optimize_above_cost :initarg :jit-optimize-above-cost 
                                :documentation "Optimize JITed functions if query is more expensive.")
   (pg::jit_profiling_support :initarg :jit-profiling-support 
                              :documentation "Register JIT compiled function with perf profiler.")
   (pg::jit_provider :initarg :jit-provider 
                     :documentation "JIT provider to use.")
   (pg::jit_tuple_deforming :initarg :jit-tuple-deforming 
                            :documentation "Allow JIT compilation of tuple deforming.")
   (pg::join_collapse_limit :initarg :join-collapse-limit 
                            :documentation "Sets the FROM-list size beyond which JOIN constructs are not flattened.")
   (pg::krb_caseins_users :initarg :krb-caseins-users 
                          :documentation "Sets whether Kerberos and GSSAPI user names should be treated as case-insensitive.")
   (pg::krb_server_keyfile :initarg :krb-server-keyfile 
                           :documentation "Sets the location of the Kerberos server key file.")
   (pg::lc_collate :initarg :lc-collate 
                   :documentation "Shows the collation order locale.")
   (pg::lc_ctype :initarg :lc-ctype 
                 :documentation "Shows the character classification and case conversion locale.")
   (pg::lc_messages :initarg :lc-messages 
                    :documentation "Sets the language in which messages are displayed.")
   (pg::lc_monetary :initarg :lc-monetary 
                    :documentation "Sets the locale for formatting monetary amounts.")
   (pg::lc_numeric :initarg :lc-numeric 
                   :documentation "Sets the locale for formatting numbers.")
   (pg::lc_time :initarg :lc-time 
                :documentation "Sets the locale for formatting date and time values.")
   (pg::listen_addresses :initarg :listen-addresses 
                         :documentation "Sets the host name or IP address(es) to listen to.")
   (pg::lo_compat_privileges :initarg :lo-compat-privileges 
                             :documentation "Enables backward compatibility mode for privilege checks on large objects.")
   (pg::local_preload_libraries :initarg :local-preload-libraries 
                                :documentation "Lists unprivileged shared libraries to preload into each backend.")
   (pg::lock_timeout :initarg :lock-timeout 
                     :documentation "Sets the maximum allowed duration of any wait for a lock.")
   (pg::log_autovacuum_min_duration :initarg :log-autovacuum-min-duration 
                                    :documentation "Sets the minimum execution time above which autovacuum actions will be logged.")
   (pg::log_checkpoints :initarg :log-checkpoints 
                        :documentation "Logs each checkpoint.")
   (pg::log_connections :initarg :log-connections 
                        :documentation "Logs each successful connection.")
   (pg::log_destination :initarg :log-destination 
                        :documentation "Sets the destination for server log output.")
   (pg::log_directory :initarg :log-directory 
                      :documentation "Sets the destination directory for log files.")
   (pg::log_disconnections :initarg :log-disconnections 
                           :documentation "Logs end of a session, including duration.")
   (pg::log_duration :initarg :log-duration 
                     :documentation "Logs the duration of each completed SQL statement.")
   (pg::log_error_verbosity :initarg :log-error-verbosity 
                            :documentation "Sets the verbosity of logged messages.")
   (pg::log_executor_stats :initarg :log-executor-stats 
                           :documentation "Writes executor performance statistics to the server log.")
   (pg::log_file_mode :initarg :log-file-mode 
                      :documentation "Sets the file permissions for log files.")
   (pg::log_filename :initarg :log-filename 
                     :documentation "Sets the file name pattern for log files.")
   (pg::log_hostname :initarg :log-hostname 
                     :documentation "Logs the host name in the connection logs.")
   (pg::log_line_prefix :initarg :log-line-prefix 
                        :documentation "Controls information prefixed to each log line.")
   (pg::log_lock_waits :initarg :log-lock-waits 
                       :documentation "Logs long lock waits.")
   (pg::log_min_duration_statement :initarg :log-min-duration-statement 
                                   :documentation "Sets the minimum execution time above which statements will be logged.")
   (pg::log_min_error_statement :initarg :log-min-error-statement 
                                :documentation "Causes all statements generating error at or above this level to be logged.")
   (pg::log_min_messages :initarg :log-min-messages 
                         :documentation "Sets the message levels that are logged.")
   (pg::log_parser_stats :initarg :log-parser-stats 
                         :documentation "Writes parser performance statistics to the server log.")
   (pg::log_planner_stats :initarg :log-planner-stats 
                          :documentation "Writes planner performance statistics to the server log.")
   (pg::log_replication_commands :initarg :log-replication-commands 
                                 :documentation "Logs each replication command.")
   (pg::log_rotation_age :initarg :log-rotation-age 
                         :documentation "Automatic log file rotation will occur after N minutes.")
   (pg::log_rotation_size :initarg :log-rotation-size 
                          :documentation "Automatic log file rotation will occur after N kilobytes.")
   (pg::log_statement :initarg :log-statement 
                      :documentation "Sets the type of statements logged.")
   (pg::log_statement_stats :initarg :log-statement-stats 
                            :documentation "Writes cumulative performance statistics to the server log.")
   (pg::log_temp_files :initarg :log-temp-files 
                       :documentation "Log the use of temporary files larger than this number of kilobytes.")
   (pg::log_timezone :initarg :log-timezone 
                     :documentation "Sets the time zone to use in log messages.")
   (pg::log_transaction_sample_rate :initarg :log-transaction-sample-rate 
                                    :documentation "Set the fraction of transactions to log for new transactions.")
   (pg::log_truncate_on_rotation :initarg :log-truncate-on-rotation 
                                 :documentation "Truncate existing log files of same name during log rotation.")
   (pg::logging_collector :initarg :logging-collector 
                          :documentation "Start a subprocess to capture stderr output and/or csvlogs into log files.")
   (pg::maintenance_work_mem :initarg :maintenance-work-mem 
                             :documentation "Sets the maximum memory to be used for maintenance operations.")
   (pg::max_connections :initarg :max-connections 
                        :documentation "Sets the maximum number of concurrent connections.")
   (pg::max_files_per_process :initarg :max-files-per-process 
                              :documentation "Sets the maximum number of simultaneously open files for each server process.")
   (pg::max_function_args :initarg :max-function-args 
                          :documentation "Shows the maximum number of function arguments.")
   (pg::max_identifier_length :initarg :max-identifier-length 
                              :documentation "Shows the maximum identifier length.")
   (pg::max_index_keys :initarg :max-index-keys 
                       :documentation "Shows the maximum number of index keys.")
   (pg::max_locks_per_transaction :initarg :max-locks-per-transaction 
                                  :documentation "Sets the maximum number of locks per transaction.")
   (pg::max_logical_replication_workers :initarg :max-logical-replication-workers 
                                        :documentation "Maximum number of logical replication worker processes.")
   (pg::max_parallel_maintenance_workers :initarg :max-parallel-maintenance-workers 
                                         :documentation "Sets the maximum number of parallel processes per maintenance operation.")
   (pg::max_parallel_workers :initarg :max-parallel-workers 
                             :documentation "Sets the maximum number of parallel workers that can be active at one time.")
   (pg::max_parallel_workers_per_gather :initarg :max-parallel-workers-per-gather 
                                        :documentation "Sets the maximum number of parallel processes per executor node.")
   (pg::max_pred_locks_per_page :initarg :max-pred-locks-per-page 
                                :documentation "Sets the maximum number of predicate-locked tuples per page.")
   (pg::max_pred_locks_per_relation :initarg :max-pred-locks-per-relation 
                                    :documentation "Sets the maximum number of predicate-locked pages and tuples per relation.")
   (pg::max_pred_locks_per_transaction :initarg :max-pred-locks-per-transaction 
                                       :documentation "Sets the maximum number of predicate locks per transaction.")
   (pg::max_prepared_transactions :initarg :max-prepared-transactions 
                                  :documentation "Sets the maximum number of simultaneously prepared transactions.")
   (pg::max_replication_slots :initarg :max-replication-slots 
                              :documentation "Sets the maximum number of simultaneously defined replication slots.")
   (pg::max_stack_depth :initarg :max-stack-depth 
                        :documentation "Sets the maximum stack depth, in kilobytes.")
   (pg::max_standby_archive_delay :initarg :max-standby-archive-delay 
                                  :documentation "Sets the maximum delay before canceling queries when a hot standby server is processing archived WAL data.")
   (pg::max_standby_streaming_delay :initarg :max-standby-streaming-delay 
                                    :documentation "Sets the maximum delay before canceling queries when a hot standby server is processing streamed WAL data.")
   (pg::max_sync_workers_per_subscription :initarg :max-sync-workers-per-subscription 
                                          :documentation "Maximum number of table synchronization workers per subscription.")
   (pg::max_wal_senders :initarg :max-wal-senders 
                        :documentation "Sets the maximum number of simultaneously running WAL sender processes.")
   (pg::max_wal_size :initarg :max-wal-size 
                     :documentation "Sets the WAL size that triggers a checkpoint.")
   (pg::max_worker_processes :initarg :max-worker-processes 
                             :documentation "Maximum number of concurrent worker processes.")
   (pg::min_parallel_index_scan_size :initarg :min-parallel-index-scan-size 
                                     :documentation "Sets the minimum amount of index data for a parallel scan.")
   (pg::min_parallel_table_scan_size :initarg :min-parallel-table-scan-size 
                                     :documentation "Sets the minimum amount of table data for a parallel scan.")
   (pg::min_wal_size :initarg :min-wal-size 
                     :documentation "Sets the minimum size to shrink the WAL to.")
   (pg::old_snapshot_threshold :initarg :old-snapshot-threshold 
                               :documentation "Time before a snapshot is too old to read pages changed after the snapshot was taken.")
   (pg::operator_precedence_warning :initarg :operator-precedence-warning 
                                    :documentation "Emit a warning for constructs that changed meaning since PostgreSQL 9.4.")
   (pg::parallel_leader_participation :initarg :parallel-leader-participation 
                                      :documentation "Controls whether Gather and Gather Merge also run subplans.")
   (pg::parallel_setup_cost :initarg :parallel-setup-cost 
                            :documentation "Sets the planner's estimate of the cost of starting up worker processes for parallel query.")
   (pg::parallel_tuple_cost :initarg :parallel-tuple-cost 
                            :documentation "Sets the planner's estimate of the cost of passing each tuple (row) from worker to master backend.")
   (pg::password_encryption :initarg :password-encryption 
                            :documentation "Encrypt passwords.")
   (pg::plan_cache_mode :initarg :plan-cache-mode 
                        :documentation "Controls the planner's selection of custom or generic plan.")
   (pg::port :initarg :port 
             :documentation "Sets the TCP port the server listens on.")
   (pg::post_auth_delay :initarg :post-auth-delay 
                        :documentation "Waits N seconds on connection startup after authentication.")
   (pg::pre_auth_delay :initarg :pre-auth-delay 
                       :documentation "Waits N seconds on connection startup before authentication.")
   (pg::primary_conninfo :initarg :primary-conninfo 
                         :documentation "Sets the connection string to be used to connect to the sending server.")
   (pg::primary_slot_name :initarg :primary-slot-name 
                          :documentation "Sets the name of the replication slot to use on the sending server.")
   (pg::promote_trigger_file :initarg :promote-trigger-file 
                             :documentation "Specifies a file name whose presence ends recovery in the standby.")
   (pg::quote_all_identifiers :initarg :quote-all-identifiers 
                              :documentation "When generating SQL fragments, quote all identifiers.")
   (pg::random_page_cost :initarg :random-page-cost 
                         :documentation "Sets the planner's estimate of the cost of a nonsequentially fetched disk page.")
   (pg::recovery_end_command :initarg :recovery-end-command 
                             :documentation "Sets the shell command that will be executed once at the end of recovery.")
   (pg::recovery_min_apply_delay :initarg :recovery-min-apply-delay 
                                 :documentation "Sets the minimum delay for applying changes during recovery.")
   (pg::recovery_target :initarg :recovery-target 
                        :documentation "Set to \"immediate\" to end recovery as soon as a consistent state is reached.")
   (pg::recovery_target_action :initarg :recovery-target-action 
                               :documentation "Sets the action to perform upon reaching the recovery target.")
   (pg::recovery_target_inclusive :initarg :recovery-target-inclusive 
                                  :documentation "Sets whether to include or exclude transaction with recovery target.")
   (pg::recovery_target_lsn :initarg :recovery-target-lsn 
                            :documentation "Sets the LSN of the write-ahead log location up to which recovery will proceed.")
   (pg::recovery_target_name :initarg :recovery-target-name 
                             :documentation "Sets the named restore point up to which recovery will proceed.")
   (pg::recovery_target_time :initarg :recovery-target-time 
                             :documentation "Sets the time stamp up to which recovery will proceed.")
   (pg::recovery_target_timeline :initarg :recovery-target-timeline 
                                 :documentation "Specifies the timeline to recover into.")
   (pg::recovery_target_xid :initarg :recovery-target-xid 
                            :documentation "Sets the transaction ID up to which recovery will proceed.")
   (pg::restart_after_crash :initarg :restart-after-crash 
                            :documentation "Reinitialize server after backend crash.")
   (pg::restore_command :initarg :restore-command 
                        :documentation "Sets the shell command that will retrieve an archived WAL file.")
   (pg::row_security :initarg :row-security 
                     :documentation "Enable row security.")
   (pg::search_path :initarg :search-path 
                    :documentation "Sets the schema search order for names that are not schema-qualified.")
   (pg::segment_size :initarg :segment-size 
                     :documentation "Shows the number of pages per disk file.")
   (pg::seq_page_cost :initarg :seq-page-cost 
                      :documentation "Sets the planner's estimate of the cost of a sequentially fetched disk page.")
   (pg::server_encoding :initarg :server-encoding 
                        :documentation "Sets the server (database) character set encoding.")
   (pg::server_version :initarg :server-version 
                       :documentation "Shows the server version.")
   (pg::server_version_num :initarg :server-version-num 
                           :documentation "Shows the server version as an integer.")
   (pg::session_preload_libraries :initarg :session-preload-libraries 
                                  :documentation "Lists shared libraries to preload into each backend.")
   (pg::session_replication_role :initarg :session-replication-role 
                                 :documentation "Sets the session's behavior for triggers and rewrite rules.")
   (pg::shared_buffers :initarg :shared-buffers 
                       :documentation "Sets the number of shared memory buffers used by the server.")
   (pg::shared_memory_type :initarg :shared-memory-type 
                           :documentation "Selects the shared memory implementation used for the main shared memory region.")
   (pg::shared_preload_libraries :initarg :shared-preload-libraries 
                                 :documentation "Lists shared libraries to preload into server.")
   (pg::ssl :initarg :ssl 
            :documentation "Enables SSL connections.")
   (pg::ssl_ca_file :initarg :ssl-ca-file 
                    :documentation "Location of the SSL certificate authority file.")
   (pg::ssl_cert_file :initarg :ssl-cert-file 
                      :documentation "Location of the SSL server certificate file.")
   (pg::ssl_ciphers :initarg :ssl-ciphers 
                    :documentation "Sets the list of allowed SSL ciphers.")
   (pg::ssl_crl_file :initarg :ssl-crl-file 
                     :documentation "Location of the SSL certificate revocation list file.")
   (pg::ssl_dh_params_file :initarg :ssl-dh-params-file 
                           :documentation "Location of the SSL DH parameters file.")
   (pg::ssl_ecdh_curve :initarg :ssl-ecdh-curve 
                       :documentation "Sets the curve to use for ECDH.")
   (pg::ssl_key_file :initarg :ssl-key-file 
                     :documentation "Location of the SSL server private key file.")
   (pg::ssl_library :initarg :ssl-library 
                    :documentation "Name of the SSL library.")
   (pg::ssl_max_protocol_version :initarg :ssl-max-protocol-version 
                                 :documentation "Sets the maximum SSL/TLS protocol version to use.")
   (pg::ssl_min_protocol_version :initarg :ssl-min-protocol-version 
                                 :documentation "Sets the minimum SSL/TLS protocol version to use.")
   (pg::ssl_passphrase_command :initarg :ssl-passphrase-command 
                               :documentation "Command to obtain passphrases for SSL.")
   (pg::ssl_passphrase_command_supports_reload :initarg :ssl-passphrase-command-supports-reload 
                                               :documentation "Also use ssl_passphrase_command during server reload.")
   (pg::ssl_prefer_server_ciphers :initarg :ssl-prefer-server-ciphers 
                                  :documentation "Give priority to server ciphersuite order.")
   (pg::standard_conforming_strings :initarg :standard-conforming-strings 
                                    :documentation "Causes '...' strings to treat backslashes literally.")
   (pg::statement_timeout :initarg :statement-timeout 
                          :documentation "Sets the maximum allowed duration of any statement.")
   (pg::stats_temp_directory :initarg :stats-temp-directory 
                             :documentation "Writes temporary statistics files to the specified directory.")
   (pg::superuser_reserved_connections :initarg :superuser-reserved-connections 
                                       :documentation "Sets the number of connection slots reserved for superusers.")
   (pg::synchronize_seqscans :initarg :synchronize-seqscans 
                             :documentation "Enable synchronized sequential scans.")
   (pg::synchronous_commit :initarg :synchronous-commit 
                           :documentation "Sets the current transaction's synchronization level.")
   (pg::synchronous_standby_names :initarg :synchronous-standby-names 
                                  :documentation "Number of synchronous standbys and list of names of potential synchronous ones.")
   (pg::syslog_facility :initarg :syslog-facility 
                        :documentation "Sets the syslog \"facility\" to be used when syslog enabled.")
   (pg::syslog_ident :initarg :syslog-ident 
                     :documentation "Sets the program name used to identify PostgreSQL messages in syslog.")
   (pg::syslog_sequence_numbers :initarg :syslog-sequence-numbers 
                                :documentation "Add sequence number to syslog messages to avoid duplicate suppression.")
   (pg::syslog_split_messages :initarg :syslog-split-messages 
                              :documentation "Split messages sent to syslog by lines and to fit into 1024 bytes.")
   (pg::tcp_keepalives_count :initarg :tcp-keepalives-count 
                             :documentation "Maximum number of TCP keepalive retransmits.")
   (pg::tcp_keepalives_idle :initarg :tcp-keepalives-idle 
                            :documentation "Time between issuing TCP keepalives.")
   (pg::tcp_keepalives_interval :initarg :tcp-keepalives-interval 
                                :documentation "Time between TCP keepalive retransmits.")
   (pg::tcp_user_timeout :initarg :tcp-user-timeout 
                         :documentation "TCP user timeout.")
   (pg::temp_buffers :initarg :temp-buffers 
                     :documentation "Sets the maximum number of temporary buffers used by each session.")
   (pg::temp_file_limit :initarg :temp-file-limit 
                        :documentation "Limits the total size of all temporary files used by each process.")
   (pg::temp_tablespaces :initarg :temp-tablespaces 
                         :documentation "Sets the tablespace(s) to use for temporary tables and sort files.")
   (pg::timezone_abbreviations :initarg :timezone-abbreviations 
                               :documentation "Selects a file of time zone abbreviations.")
   (pg::trace_notify :initarg :trace-notify 
                     :documentation "Generates debugging output for LISTEN and NOTIFY.")
   (pg::trace_recovery_messages :initarg :trace-recovery-messages 
                                :documentation "Enables logging of recovery-related debugging information.")
   (pg::trace_sort :initarg :trace-sort 
                   :documentation "Emit information about resource usage in sorting.")
   (pg::track_activities :initarg :track-activities 
                         :documentation "Collects information about executing commands.")
   (pg::track_activity_query_size :initarg :track-activity-query-size 
                                  :documentation "Sets the size reserved for pg_stat_activity.query, in bytes.")
   (pg::track_commit_timestamp :initarg :track-commit-timestamp 
                               :documentation "Collects transaction commit time.")
   (pg::track_counts :initarg :track-counts 
                     :documentation "Collects statistics on database activity.")
   (pg::track_functions :initarg :track-functions 
                        :documentation "Collects function-level statistics on database activity.")
   (pg::track_io_timing :initarg :track-io-timing 
                        :documentation "Collects timing statistics for database I/O activity.")
   (pg::transaction_deferrable :initarg :transaction-deferrable 
                               :documentation "Whether to defer a read-only serializable transaction until it can be executed with no possible serialization failures.")
   (pg::transaction_isolation :initarg :transaction-isolation 
                              :documentation "Sets the current transaction's isolation level.")
   (pg::transaction_read_only :initarg :transaction-read-only 
                              :documentation "Sets the current transaction's read-only status.")
   (pg::transform_null_equals :initarg :transform-null-equals 
                              :documentation "Treats \"expr=NULL\" as \"expr IS NULL\".")
   (pg::unix_socket_directories :initarg :unix-socket-directories 
                                :documentation "Sets the directories where Unix-domain sockets will be created.")
   (pg::unix_socket_group :initarg :unix-socket-group 
                          :documentation "Sets the owning group of the Unix-domain socket.")
   (pg::unix_socket_permissions :initarg :unix-socket-permissions 
                                :documentation "Sets the access permissions of the Unix-domain socket.")
   (pg::update_process_title :initarg :update-process-title 
                             :documentation "Updates the process title to show the active SQL command.")
   (pg::vacuum_cleanup_index_scale_factor :initarg :vacuum-cleanup-index-scale-factor 
                                          :documentation "Number of tuple inserts prior to index cleanup as a fraction of reltuples.")
   (pg::vacuum_cost_delay :initarg :vacuum-cost-delay 
                          :documentation "Vacuum cost delay in milliseconds.")
   (pg::vacuum_cost_limit :initarg :vacuum-cost-limit 
                          :documentation "Vacuum cost amount available before napping.")
   (pg::vacuum_cost_page_dirty :initarg :vacuum-cost-page-dirty 
                               :documentation "Vacuum cost for a page dirtied by vacuum.")
   (pg::vacuum_cost_page_hit :initarg :vacuum-cost-page-hit 
                             :documentation "Vacuum cost for a page found in the buffer cache.")
   (pg::vacuum_cost_page_miss :initarg :vacuum-cost-page-miss 
                              :documentation "Vacuum cost for a page not found in the buffer cache.")
   (pg::vacuum_defer_cleanup_age :initarg :vacuum-defer-cleanup-age 
                                 :documentation "Number of transactions by which VACUUM and HOT cleanup should be deferred, if any.")
   (pg::vacuum_freeze_min_age :initarg :vacuum-freeze-min-age 
                              :documentation "Minimum age at which VACUUM should freeze a table row.")
   (pg::vacuum_freeze_table_age :initarg :vacuum-freeze-table-age 
                                :documentation "Age at which VACUUM should scan whole table to freeze tuples.")
   (pg::vacuum_multixact_freeze_min_age :initarg :vacuum-multixact-freeze-min-age 
                                        :documentation "Minimum age at which VACUUM should freeze a MultiXactId in a table row.")
   (pg::vacuum_multixact_freeze_table_age :initarg :vacuum-multixact-freeze-table-age 
                                          :documentation "Multixact age at which VACUUM should scan whole table to freeze tuples.")
   (pg::wal_block_size :initarg :wal-block-size 
                       :documentation "Shows the block size in the write ahead log.")
   (pg::wal_buffers :initarg :wal-buffers 
                    :documentation "Sets the number of disk-page buffers in shared memory for WAL.")
   (pg::wal_compression :initarg :wal-compression 
                        :documentation "Compresses full-page writes written in WAL file.")
   (pg::wal_consistency_checking :initarg :wal-consistency-checking 
                                 :documentation "Sets the WAL resource managers for which WAL consistency checks are done.")
   (pg::wal_init_zero :initarg :wal-init-zero 
                      :documentation "Writes zeroes to new WAL files before first use.")
   (pg::wal_keep_segments :initarg :wal-keep-segments 
                          :documentation "Sets the number of WAL files held for standby servers.")
   (pg::wal_level :initarg :wal-level 
                  :documentation "Set the level of information written to the WAL.")
   (pg::wal_log_hints :initarg :wal-log-hints 
                      :documentation "Writes full pages to WAL when first modified after a checkpoint, even for a non-critical modifications.")
   (pg::wal_receiver_status_interval :initarg :wal-receiver-status-interval 
                                     :documentation "Sets the maximum interval between WAL receiver status reports to the sending server.")
   (pg::wal_receiver_timeout :initarg :wal-receiver-timeout 
                             :documentation "Sets the maximum wait time to receive data from the sending server.")
   (pg::wal_recycle :initarg :wal-recycle 
                    :documentation "Recycles WAL files by renaming them.")
   (pg::wal_retrieve_retry_interval :initarg :wal-retrieve-retry-interval 
                                    :documentation "Sets the time to wait before retrying to retrieve WAL after a failed attempt.")
   (pg::wal_segment_size :initarg :wal-segment-size 
                         :documentation "Shows the size of write ahead log segments.")
   (pg::wal_sender_timeout :initarg :wal-sender-timeout 
                           :documentation "Sets the maximum time to wait for WAL replication.")
   (pg::wal_sync_method :initarg :wal-sync-method 
                        :documentation "Selects the method used for forcing WAL updates to disk.")
   (pg::wal_writer_delay :initarg :wal-writer-delay 
                         :documentation "Time between WAL flushes performed in the WAL writer.")
   (pg::wal_writer_flush_after :initarg :wal-writer-flush-after 
                               :documentation "Amount of WAL written out by WAL writer that triggers a flush.")
   (pg::work_mem :initarg :work-mem 
                 :documentation "Sets the maximum memory to be used for query workspaces.")
   (pg::xmlbinary :initarg :xmlbinary 
                  :documentation "Sets how binary values are to be encoded in XML.")
   (pg::xmloption :initarg :xmloption 
                  :documentation "Sets whether XML data in implicit parsing and serialization operations is to be considered as documents or content fragments.")
   (pg::zero_damaged_pages :initarg :zero-damaged-pages 
                           :documentation "Continues processing past damaged page headers.")

   ))
