(**
   Boilerplate to be used as a template when mapping the sqlite CST
   to another type of tree.
*)

module R = Tree_sitter_run.Raw_tree

(* Disable warnings against unused variables *)
[@@@warning "-26-27"]

(* Disable warning against unused 'rec' *)
[@@@warning "-39"]

type env = unit

let token (env : env) (tok : Tree_sitter_run.Token.t) =
  R.Token tok

let blank (env : env) () =
  R.Tuple []

let map_rowid (env : env) (tok : CST.rowid) =
  (* pattern [rR][oO][wW][iI][dD] *) token env tok

let map_escape (env : env) (tok : CST.escape) =
  (* pattern [eE][sS][cC][aA][pP][eE] *) token env tok

let map_range (env : env) (tok : CST.range) =
  (* pattern [rR][aA][nN][gG][eE] *) token env tok

let map_immediate (env : env) (tok : CST.immediate) =
  (* pattern [iI][mM][mM][eE][dD][iI][aA][tT][eE] *) token env tok

let map_natural (env : env) (tok : CST.natural) =
  (* pattern [nN][aA][tT][uU][rR][aA][lL] *) token env tok

let map_before (env : env) (tok : CST.before) =
  (* pattern [bB][eE][fF][oO][rR][eE] *) token env tok

let map_create (env : env) (tok : CST.create) =
  (* pattern [cC][rR][eE][aA][tT][eE] *) token env tok

let map_all (env : env) (tok : CST.all) =
  (* pattern [aA][lL][lL] *) token env tok

let map_to_ (env : env) (tok : CST.to_) =
  (* pattern [tT][oO] *) token env tok

let map_deferrable (env : env) (tok : CST.deferrable) =
  (* pattern [dD][eE][fF][eE][rR][rR][aA][bB][lL][eE] *) token env tok

let map_ignore (env : env) (tok : CST.ignore) =
  (* pattern [iI][gG][nN][oO][rR][eE] *) token env tok

let map_if_ (env : env) (tok : CST.if_) =
  (* pattern [iI][fF] *) token env tok

let map_index (env : env) (tok : CST.index) =
  (* pattern [iI][nN][dD][eE][xX] *) token env tok

let map_union (env : env) (tok : CST.union) =
  (* pattern [uU][nN][iI][oO][nN] *) token env tok

let map_insert (env : env) (tok : CST.insert) =
  (* pattern [iI][nN][sS][eE][rR][tT] *) token env tok

let map_over (env : env) (tok : CST.over) =
  (* pattern [oO][vV][eE][rR] *) token env tok

let map_exclude (env : env) (tok : CST.exclude) =
  (* pattern [eE][xX][cC][lL][uU][dD][eE] *) token env tok

let map_nothing (env : env) (tok : CST.nothing) =
  (* pattern [nN][oO][tT][hH][iI][nN][gG] *) token env tok

let map_first (env : env) (tok : CST.first) =
  (* pattern [fF][iI][rR][sS][tT] *) token env tok

let map_with_ (env : env) (tok : CST.with_) =
  (* pattern [wW][iI][tT][hH] *) token env tok

let map_current (env : env) (tok : CST.current) =
  (* pattern [cC][uU][rR][rR][eE][nN][tT] *) token env tok

let map_when_ (env : env) (tok : CST.when_) =
  (* pattern [wW][hH][eE][nN] *) token env tok

let map_raise (env : env) (tok : CST.raise) =
  (* pattern [rR][aA][iI][sS][eE] *) token env tok

let map_by (env : env) (tok : CST.by) =
  (* pattern [bB][yY] *) token env tok

let map_last (env : env) (tok : CST.last) =
  (* pattern [lL][aA][sS][tT] *) token env tok

let map_anon_choice_PLUS_da42005 (env : env) (x : CST.anon_choice_PLUS_da42005) =
  (match x with
  | `PLUS tok -> R.Case ("PLUS",
      (* "+" *) token env tok
    )
  | `DASH tok -> R.Case ("DASH",
      (* "-" *) token env tok
    )
  )

let map_or_ (env : env) (tok : CST.or_) =
  (* pattern [oO][rR] *) token env tok

let map_virtual_ (env : env) (tok : CST.virtual_) =
  (* pattern [vV][iI][rR][tT][uU][aA][lL] *) token env tok

let map_references (env : env) (tok : CST.references) =
  (* pattern [rR][eE][fF][eE][rR][eE][nN][cC][eE][sS] *) token env tok

let map_distinct (env : env) (tok : CST.distinct) =
  (* pattern [dD][iI][sS][tT][iI][nN][cC][tT] *) token env tok

let map_pat_93c883a (env : env) (tok : CST.pat_93c883a) =
  (* pattern [$_0-9a-zA-Z\x80-\xFF]+ *) token env tok

let map_unbounded (env : env) (tok : CST.unbounded) =
  (* pattern [uU][nN][bB][oO][uU][nN][dD][eE][dD] *) token env tok

let map_analyze (env : env) (tok : CST.analyze) =
  (* pattern [aA][nN][aA][lL][yY][zZ][eE] *) token env tok

let map_preceding (env : env) (tok : CST.preceding) =
  (* pattern [pP][rR][eE][cC][eE][dD][iI][nN][gG] *) token env tok

let map_transaction (env : env) (tok : CST.transaction) =
  (* pattern [tT][rR][aA][nN][sS][aA][cC][tT][iI][oO][nN] *) token env tok

let map_true_ (env : env) (tok : CST.true_) =
  (* pattern [tT][rR][uU][eE] *) token env tok

let map_generated (env : env) (tok : CST.generated) =
  (* pattern [gG][eE][nN][eE][rR][aA][tT][eE][dD] *) token env tok

let map_end_ (env : env) (tok : CST.end_) =
  (* pattern [eE][nN][dD] *) token env tok

let map_limit (env : env) (tok : CST.limit) =
  (* pattern [lL][iI][mM][iI][tT] *) token env tok

let map_join (env : env) (tok : CST.join) =
  (* pattern [jJ][oO][iI][nN] *) token env tok

let map_on (env : env) (tok : CST.on) =
  (* pattern [oO][nN] *) token env tok

let map_action (env : env) (tok : CST.action) =
  (* pattern [aA][cC][tT][iI][oO][nN] *) token env tok

let map_except (env : env) (tok : CST.except) =
  (* pattern [eE][xX][cC][eE][pP][tT] *) token env tok

let map_else_ (env : env) (tok : CST.else_) =
  (* pattern [eE][lL][sS][eE] *) token env tok

let map_do_ (env : env) (tok : CST.do_) =
  (* pattern [dD][oO] *) token env tok

let map_after (env : env) (tok : CST.after) =
  (* pattern [aA][fF][tT][eE][rR] *) token env tok

let map_notnull (env : env) (tok : CST.notnull) =
  (* pattern [nN][oO][tT][nN][uU][lL][lL] *) token env tok

let map_autoincrement (env : env) (tok : CST.autoincrement) =
  (* pattern [aA][uU][tT][oO][iI][nN][cC][rR][eE][mM][eE][nN][tT] *) token env tok

let map_asc (env : env) (tok : CST.asc) =
  (* pattern [aA][sS][cC] *) token env tok

let map_from (env : env) (tok : CST.from) =
  (* pattern [fF][rR][oO][mM] *) token env tok

let map_initially (env : env) (tok : CST.initially) =
  (* pattern [iI][nN][iI][tT][iI][aA][lL][lL][yY] *) token env tok

let map_recursive (env : env) (tok : CST.recursive) =
  (* pattern [rR][eE][cC][uU][rR][sS][iI][vV][eE] *) token env tok

let map_collate (env : env) (tok : CST.collate) =
  (* pattern [cC][oO][lL][lL][aA][tT][eE] *) token env tok

let map_each (env : env) (tok : CST.each) =
  (* pattern [eE][aA][cC][hH] *) token env tok

let map_no (env : env) (tok : CST.no) =
  (* pattern [nN][oO] *) token env tok

let map_temporary (env : env) (tok : CST.temporary) =
  (* pattern [tT][eE][mM][pP][oO][rR][aA][rR][yY] *) token env tok

let map_current_date (env : env) (tok : CST.current_date) =
  (* pattern [cC][uU][rR][rR][eE][nN][tT][__][dD][aA][tT][eE] *) token env tok

let map_plan (env : env) (tok : CST.plan) =
  (* pattern [pP][lL][aA][nN] *) token env tok

let map_left (env : env) (tok : CST.left) =
  (* pattern [lL][eE][fF][tT] *) token env tok

let map_row (env : env) (tok : CST.row) =
  (* pattern [rR][oO][wW] *) token env tok

let map_explain (env : env) (tok : CST.explain) =
  (* pattern [eE][xX][pP][lL][aA][iI][nN] *) token env tok

let map_having (env : env) (tok : CST.having) =
  (* pattern [hH][aA][vV][iI][nN][gG] *) token env tok

let map_begin_ (env : env) (tok : CST.begin_) =
  (* pattern [bB][eE][gG][iI][nN] *) token env tok

let map_group (env : env) (tok : CST.group) =
  (* pattern [gG][rR][oO][uU][pP] *) token env tok

let map_in_ (env : env) (tok : CST.in_) =
  (* pattern [iI][nN] *) token env tok

let map_values (env : env) (tok : CST.values) =
  (* pattern [vV][aA][lL][uU][eE][sS] *) token env tok

let map_trigger (env : env) (tok : CST.trigger) =
  (* pattern [tT][rR][iI][gG][gG][eE][rR] *) token env tok

let map_null (env : env) (tok : CST.null) =
  (* pattern [nN][uU][lL][lL] *) token env tok

let map_deferred (env : env) (tok : CST.deferred) =
  (* pattern [dD][eE][fF][eE][rR][rR][eE][dD] *) token env tok

let map_window (env : env) (tok : CST.window) =
  (* pattern [wW][iI][nN][dD][oO][wW] *) token env tok

let map_restrict (env : env) (tok : CST.restrict) =
  (* pattern [rR][eE][sS][tT][rR][iI][cC][tT] *) token env tok

let map_select (env : env) (tok : CST.select) =
  (* pattern [sS][eE][lL][eE][cC][tT] *) token env tok

let map_indexed (env : env) (tok : CST.indexed) =
  (* pattern [iI][nN][dD][eE][xX][eE][dD] *) token env tok

let map_partition (env : env) (tok : CST.partition) =
  (* pattern [pP][aA][rR][tT][iI][tT][iI][oO][nN] *) token env tok

let map_using (env : env) (tok : CST.using) =
  (* pattern [uU][sS][iI][nN][gG] *) token env tok

let map_others (env : env) (tok : CST.others) =
  (* pattern [oO][tT][hH][eE][rR][sS] *) token env tok

let map_cascade (env : env) (tok : CST.cascade) =
  (* pattern [cC][aA][sS][cC][aA][dD][eE] *) token env tok

let map_following (env : env) (tok : CST.following) =
  (* pattern [fF][oO][lL][lL][oO][wW][iI][nN][gG] *) token env tok

let map_release (env : env) (tok : CST.release) =
  (* pattern [rR][eE][lL][eE][aA][sS][eE] *) token env tok

let map_ties (env : env) (tok : CST.ties) =
  (* pattern [tT][iI][eE][sS] *) token env tok

let map_detach (env : env) (tok : CST.detach) =
  (* pattern [dD][eE][tT][aA][cC][hH] *) token env tok

let map_cross (env : env) (tok : CST.cross) =
  (* pattern [cC][rR][oO][sS][sS] *) token env tok

let map_isnull (env : env) (tok : CST.isnull) =
  (* pattern [iI][sS][nN][uU][lL][lL] *) token env tok

let map_not (env : env) (tok : CST.not) =
  (* pattern [nN][oO][tT] *) token env tok

let map_inner (env : env) (tok : CST.inner) =
  (* pattern [iI][nN][nN][eE][rR] *) token env tok

let map_update (env : env) (tok : CST.update) =
  (* pattern [uU][pP][dD][aA][tT][eE] *) token env tok

let map_database (env : env) (tok : CST.database) =
  (* pattern [dD][aA][tT][aA][bB][aA][sS][eE] *) token env tok

let map_pat_4fd4a56 (env : env) (tok : CST.pat_4fd4a56) =
  (* pattern .* *) token env tok

let map_current_timestamp (env : env) (tok : CST.current_timestamp) =
  (* pattern [cC][uU][rR][rR][eE][nN][tT][__][tT][iI][mM][eE][sS][tT][aA][mM][pP] *) token env tok

let map_set (env : env) (tok : CST.set) =
  (* pattern [sS][eE][tT] *) token env tok

let map_primary (env : env) (tok : CST.primary) =
  (* pattern [pP][rR][iI][mM][aA][rR][yY] *) token env tok

let map_temp (env : env) (tok : CST.temp) =
  (* pattern [tT][eE][mM][pP] *) token env tok

let map_pat_f79575e (env : env) (tok : CST.pat_f79575e) =
  (* pattern "(''|[^'])*" *) token env tok

let map_order (env : env) (tok : CST.order) =
  (* pattern [oO][rR][dD][eE][rR] *) token env tok

let map_outer (env : env) (tok : CST.outer) =
  (* pattern [oO][uU][tT][eE][rR] *) token env tok

let map_is (env : env) (tok : CST.is) =
  (* pattern [iI][sS] *) token env tok

let map_numeric_literal (env : env) (tok : CST.numeric_literal) =
  (* numeric_literal *) token env tok

let map_pat_f154b4a (env : env) (tok : CST.pat_f154b4a) =
  (* pattern [^\]]* *) token env tok

let map_pat_73398bc (env : env) (tok : CST.pat_73398bc) =
  (* pattern "(\"\"|[^\"])*" *) token env tok

let map_pragma (env : env) (tok : CST.pragma) =
  (* pattern [pP][rR][aA][gG][mM][aA] *) token env tok

let map_table (env : env) (tok : CST.table) =
  (* pattern [tT][aA][bB][lL][eE] *) token env tok

let map_column (env : env) (tok : CST.column) =
  (* pattern [cC][oO][lL][uU][mM][nN] *) token env tok

let map_default (env : env) (tok : CST.default) =
  (* pattern [dD][eE][fF][aA][uU][lL][tT] *) token env tok

let map_vacuum (env : env) (tok : CST.vacuum) =
  (* pattern [vV][aA][cC][uU][uU][mM] *) token env tok

let map_false_ (env : env) (tok : CST.false_) =
  (* pattern [fF][aA][lL][sS][eE] *) token env tok

let map_instead (env : env) (tok : CST.instead) =
  (* pattern [iI][nN][sS][tT][eE][aA][dD] *) token env tok

let map_of_ (env : env) (tok : CST.of_) =
  (* pattern [oO][fF] *) token env tok

let map_current_time (env : env) (tok : CST.current_time) =
  (* pattern [cC][uU][rR][rR][eE][nN][tT][__][tT][iI][mM][eE] *) token env tok

let map_exists (env : env) (tok : CST.exists) =
  (* pattern [eE][xX][iI][sS][tT][sS] *) token env tok

let map_reindex (env : env) (tok : CST.reindex) =
  (* pattern [rR][eE][iI][nN][dD][eE][xX] *) token env tok

let map_rows (env : env) (tok : CST.rows) =
  (* pattern [rR][oO][wW][sS] *) token env tok

let map_cast (env : env) (tok : CST.cast) =
  (* pattern [cC][aA][sS][tT] *) token env tok

let map_like (env : env) (tok : CST.like) =
  (* pattern [lL][iI][kK][eE] *) token env tok

let map_constraint_ (env : env) (tok : CST.constraint_) =
  (* pattern [cC][oO][nN][sS][tT][rR][aA][iI][nN][tT] *) token env tok

let map_regexp (env : env) (tok : CST.regexp) =
  (* pattern [rR][eE][gG][eE][xX][pP] *) token env tok

let map_materialized (env : env) (tok : CST.materialized) =
  (* pattern [mM][aA][tT][eE][rR][iI][aA][lL][iI][zZ][eE][dD] *) token env tok

let map_offset (env : env) (tok : CST.offset) =
  (* pattern [oO][fF][fF][sS][eE][tT] *) token env tok

let map_pat_05bf793 (env : env) (tok : CST.pat_05bf793) =
  (* pattern [^*]*\*+([^/*][^*]*\*+)* *) token env tok

let map_drop (env : env) (tok : CST.drop) =
  (* pattern [dD][rR][oO][pP] *) token env tok

let map_attach (env : env) (tok : CST.attach) =
  (* pattern [aA][tT][tT][aA][cC][hH] *) token env tok

let map_view (env : env) (tok : CST.view) =
  (* pattern [vV][iI][eE][wW] *) token env tok

let map_without (env : env) (tok : CST.without) =
  (* pattern [wW][iI][tT][hH][oO][uU][tT] *) token env tok

let map_between (env : env) (tok : CST.between) =
  (* pattern [bB][eE][tT][wW][eE][eE][nN] *) token env tok

let map_pat_213dc3e (env : env) (tok : CST.pat_213dc3e) =
  (* pattern [0-9] *) token env tok

let map_stored (env : env) (tok : CST.stored) =
  (* pattern [sS][tT][oO][rR][eE][dD] *) token env tok

let map_where (env : env) (tok : CST.where) =
  (* pattern [wW][hH][eE][rR][eE] *) token env tok

let map_alter (env : env) (tok : CST.alter) =
  (* pattern [aA][lL][tT][eE][rR] *) token env tok

let map_for_ (env : env) (tok : CST.for_) =
  (* pattern [fF][oO][rR] *) token env tok

let map_key (env : env) (tok : CST.key) =
  (* pattern [kK][eE][yY] *) token env tok

let map_savepoint (env : env) (tok : CST.savepoint) =
  (* pattern [sS][aA][vV][eE][pP][oO][iI][nN][tT] *) token env tok

let map_glob (env : env) (tok : CST.glob) =
  (* pattern [gG][lL][oO][bB] *) token env tok

let map_query (env : env) (tok : CST.query) =
  (* pattern [qQ][uU][eE][rR][yY] *) token env tok

let map_groups (env : env) (tok : CST.groups) =
  (* pattern [gG][rR][oO][uU][pP][sS] *) token env tok

let map_intersect (env : env) (tok : CST.intersect) =
  (* pattern [iI][nN][tT][eE][rR][sS][eE][cC][tT] *) token env tok

let map_case (env : env) (tok : CST.case) =
  (* pattern [cC][aA][sS][eE] *) token env tok

let map_delete (env : env) (tok : CST.delete) =
  (* pattern [dD][eE][lL][eE][tT][eE] *) token env tok

let map_replace (env : env) (tok : CST.replace) =
  (* pattern [rR][eE][pP][lL][aA][cC][eE] *) token env tok

let map_pat_29ffae5 (env : env) (tok : CST.pat_29ffae5) =
  (* pattern [_a-zA-Z\x80-\xFF][$_0-9a-zA-Z\x80-\xFF]* *) token env tok

let map_then_ (env : env) (tok : CST.then_) =
  (* pattern [tT][hH][eE][nN] *) token env tok

let map_foreign (env : env) (tok : CST.foreign) =
  (* pattern [fF][oO][rR][eE][iI][gG][nN] *) token env tok

let map_into (env : env) (tok : CST.into) =
  (* pattern [iI][nN][tT][oO] *) token env tok

let map_and_ (env : env) (tok : CST.and_) =
  (* pattern [aA][nN][dD] *) token env tok

let map_abort (env : env) (tok : CST.abort) =
  (* pattern [aA][bB][oO][rR][tT] *) token env tok

let map_nulls (env : env) (tok : CST.nulls) =
  (* pattern [nN][uU][lL][lL][sS] *) token env tok

let map_conflict (env : env) (tok : CST.conflict) =
  (* pattern [cC][oO][nN][fF][lL][iI][cC][tT] *) token env tok

let map_pat_bb5937d (env : env) (tok : CST.pat_bb5937d) =
  (* pattern (``|[^`])* *) token env tok

let map_returning (env : env) (tok : CST.returning) =
  (* pattern [rR][eE][tT][uU][rR][nN][iI][nN][gG] *) token env tok

let map_desc (env : env) (tok : CST.desc) =
  (* pattern [dD][eE][sS][cC] *) token env tok

let map_exclusive (env : env) (tok : CST.exclusive) =
  (* pattern [eE][xX][cC][lL][uU][sS][iI][vV][eE] *) token env tok

let map_unique (env : env) (tok : CST.unique) =
  (* pattern [uU][nN][iI][qQ][uU][eE] *) token env tok

let map_always (env : env) (tok : CST.always) =
  (* pattern [aA][lL][wW][aA][yY][sS] *) token env tok

let map_filter (env : env) (tok : CST.filter) =
  (* pattern [fF][iI][lL][tT][eE][rR] *) token env tok

let map_check (env : env) (tok : CST.check) =
  (* pattern [cC][hH][eE][cC][kK] *) token env tok

let map_rename (env : env) (tok : CST.rename) =
  (* pattern [rR][eE][nN][aA][mM][eE] *) token env tok

let map_match_ (env : env) (tok : CST.match_) =
  (* pattern [mM][aA][tT][cC][hH] *) token env tok

let map_rollback (env : env) (tok : CST.rollback) =
  (* pattern [rR][oO][lL][lL][bB][aA][cC][kK] *) token env tok

let map_commit (env : env) (tok : CST.commit) =
  (* pattern [cC][oO][mM][mM][iI][tT] *) token env tok

let map_fail (env : env) (tok : CST.fail) =
  (* pattern [fF][aA][iI][lL] *) token env tok

let map_add (env : env) (tok : CST.add) =
  (* pattern [aA][dD][dD] *) token env tok

let map_as_ (env : env) (tok : CST.as_) =
  (* pattern [aA][sS] *) token env tok

let map_anon_choice_temp_716f4ac (env : env) (x : CST.anon_choice_temp_716f4ac) =
  (match x with
  | `Temp_3d801aa tok -> R.Case ("Temp_3d801aa",
      (* pattern [tT][eE][mM][pP] *) token env tok
    )
  | `Temp_d5197d9 tok -> R.Case ("Temp_d5197d9",
      (* pattern [tT][eE][mM][pP][oO][rR][aA][rR][yY] *) token env tok
    )
  )

let map_string_ (env : env) ((v1, v2, v3) : CST.string_) =
  let v1 = (* "'" *) token env v1 in
  let v2 = map_pat_f79575e env v2 in
  let v3 = (* "'" *) token env v3 in
  R.Tuple [v1; v2; v3]

let map_join_operator (env : env) (x : CST.join_operator) =
  (match x with
  | `COMMA tok -> R.Case ("COMMA",
      (* "," *) token env tok
    )
  | `Opt_natu_opt_choice_left_opt_outer_join (v1, v2, v3) -> R.Case ("Opt_natu_opt_choice_left_opt_outer_join",
      let v1 =
        (match v1 with
        | Some tok -> R.Option (Some (
            (* pattern [nN][aA][tT][uU][rR][aA][lL] *) token env tok
          ))
        | None -> R.Option None)
      in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            (match x with
            | `Left_opt_outer (v1, v2) -> R.Case ("Left_opt_outer",
                let v1 = (* pattern [lL][eE][fF][tT] *) token env v1 in
                let v2 =
                  (match v2 with
                  | Some tok -> R.Option (Some (
                      (* pattern [oO][uU][tT][eE][rR] *) token env tok
                    ))
                  | None -> R.Option None)
                in
                R.Tuple [v1; v2]
              )
            | `Inner tok -> R.Case ("Inner",
                (* pattern [iI][nN][nN][eE][rR] *) token env tok
              )
            | `Cross tok -> R.Case ("Cross",
                (* pattern [cC][rR][oO][sS][sS] *) token env tok
              )
            )
          ))
        | None -> R.Option None)
      in
      let v3 = (* pattern [jJ][oO][iI][nN] *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

let map_signed_number (env : env) ((v1, v2) : CST.signed_number) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_anon_choice_PLUS_da42005 env x
      ))
    | None -> R.Option None)
  in
  let v2 = (* numeric_literal *) token env v2 in
  R.Tuple [v1; v2]

let map_bind_parameter (env : env) (x : CST.bind_parameter) =
  (match x with
  | `QMARK_rep_pat_213dc3e (v1, v2) -> R.Case ("QMARK_rep_pat_213dc3e",
      let v1 = (* "?" *) token env v1 in
      let v2 =
        R.List (List.map (fun x ->
          map_pat_213dc3e env x
        ) v2)
      in
      R.Tuple [v1; v2]
    )
  | `Choice_AT_pat_93c883a (v1, v2) -> R.Case ("Choice_AT_pat_93c883a",
      let v1 =
        (match v1 with
        | `AT tok -> R.Case ("AT",
            (* "@" *) token env tok
          )
        | `DOLLAR tok -> R.Case ("DOLLAR",
            (* "$" *) token env tok
          )
        | `COLON tok -> R.Case ("COLON",
            (* ":" *) token env tok
          )
        | `HASH tok -> R.Case ("HASH",
            (* "#" *) token env tok
          )
        )
      in
      let v2 = map_pat_93c883a env v2 in
      R.Tuple [v1; v2]
    )
  )

let map_compound_operator (env : env) (x : CST.compound_operator) =
  (match x with
  | `Union tok -> R.Case ("Union",
      (* pattern [uU][nN][iI][oO][nN] *) token env tok
    )
  | `Union_all (v1, v2) -> R.Case ("Union_all",
      let v1 = (* pattern [uU][nN][iI][oO][nN] *) token env v1 in
      let v2 = (* pattern [aA][lL][lL] *) token env v2 in
      R.Tuple [v1; v2]
    )
  | `Inte tok -> R.Case ("Inte",
      (* pattern [iI][nN][tT][eE][rR][sS][eE][cC][tT] *) token env tok
    )
  | `Except tok -> R.Case ("Except",
      (* pattern [eE][xX][cC][eE][pP][tT] *) token env tok
    )
  )

let map_identifier (env : env) (x : CST.identifier) =
  (match x with
  | `Pat_29ffae5 x -> R.Case ("Pat_29ffae5",
      map_pat_29ffae5 env x
    )
  | `DQUOT_pat_73398bc_DQUOT (v1, v2, v3) -> R.Case ("DQUOT_pat_73398bc_DQUOT",
      let v1 = (* "\"" *) token env v1 in
      let v2 = map_pat_73398bc env v2 in
      let v3 = (* "\"" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `BQUOT_pat_bb5937d_BQUOT (v1, v2, v3) -> R.Case ("BQUOT_pat_bb5937d_BQUOT",
      let v1 = (* "`" *) token env v1 in
      let v2 = map_pat_bb5937d env v2 in
      let v3 = (* "`" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `LBRACK_pat_f154b4a_RBRACK (v1, v2, v3) -> R.Case ("LBRACK_pat_f154b4a_RBRACK",
      let v1 = (* "[" *) token env v1 in
      let v2 = map_pat_f154b4a env v2 in
      let v3 = (* "]" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

let map_anon_choice_asc_fa4fd8b (env : env) (x : CST.anon_choice_asc_fa4fd8b) =
  (match x with
  | `Asc tok -> R.Case ("Asc",
      (* pattern [aA][sS][cC] *) token env tok
    )
  | `Desc tok -> R.Case ("Desc",
      (* pattern [dD][eE][sS][cC] *) token env tok
    )
  )

let map_anon_choice_abort_500a898 (env : env) (x : CST.anon_choice_abort_500a898) =
  (match x with
  | `Abort tok -> R.Case ("Abort",
      (* pattern [aA][bB][oO][rR][tT] *) token env tok
    )
  | `Fail tok -> R.Case ("Fail",
      (* pattern [fF][aA][iI][lL] *) token env tok
    )
  | `Ignore tok -> R.Case ("Ignore",
      (* pattern [iI][gG][nN][oO][rR][eE] *) token env tok
    )
  | `Repl tok -> R.Case ("Repl",
      (* pattern [rR][eE][pP][lL][aA][cC][eE] *) token env tok
    )
  | `Roll tok -> R.Case ("Roll",
      (* pattern [rR][oO][lL][lL][bB][aA][cC][kK] *) token env tok
    )
  )

let map_conflict_clause (env : env) ((v1, v2, v3) : CST.conflict_clause) =
  let v1 = (* pattern [oO][nN] *) token env v1 in
  let v2 =
    (* pattern [cC][oO][nN][fF][lL][iI][cC][tT] *) token env v2
  in
  let v3 =
    (match v3 with
    | `Roll tok -> R.Case ("Roll",
        (* pattern [rR][oO][lL][lL][bB][aA][cC][kK] *) token env tok
      )
    | `Abort tok -> R.Case ("Abort",
        (* pattern [aA][bB][oO][rR][tT] *) token env tok
      )
    | `Fail tok -> R.Case ("Fail",
        (* pattern [fF][aA][iI][lL] *) token env tok
      )
    | `Ignore tok -> R.Case ("Ignore",
        (* pattern [iI][gG][nN][oO][rR][eE] *) token env tok
      )
    | `Repl tok -> R.Case ("Repl",
        (* pattern [rR][eE][pP][lL][aA][cC][eE] *) token env tok
      )
    )
  in
  R.Tuple [v1; v2; v3]

let map_string_literal (env : env) (x : CST.string_literal) =
  map_string_ env x

let map_function_name (env : env) (x : CST.function_name) =
  map_identifier env x

let map_name (env : env) (x : CST.name) =
  (match x with
  | `Str_lit x -> R.Case ("Str_lit",
      map_string_literal env x
    )
  | `Id x -> R.Case ("Id",
      map_function_name env x
    )
  )

let map_collation_name (env : env) (x : CST.collation_name) =
  (match x with
  | `Str_lit x -> R.Case ("Str_lit",
      map_string_literal env x
    )
  | `Id x -> R.Case ("Id",
      map_function_name env x
    )
  )

let map_literal_value (env : env) (x : CST.literal_value) =
  (match x with
  | `Nume_lit tok -> R.Case ("Nume_lit",
      (* numeric_literal *) token env tok
    )
  | `Str_lit x -> R.Case ("Str_lit",
      map_string_literal env x
    )
  | `Blob_lit (v1, v2) -> R.Case ("Blob_lit",
      let v1 =
        (match v1 with
        | `X_9dd4e46 tok -> R.Case ("X_9dd4e46",
            (* "x" *) token env tok
          )
        | `X_02129bb tok -> R.Case ("X_02129bb",
            (* "X" *) token env tok
          )
        )
      in
      let v2 = map_string_literal env v2 in
      R.Tuple [v1; v2]
    )
  | `Null tok -> R.Case ("Null",
      (* pattern [nN][uU][lL][lL] *) token env tok
    )
  | `True tok -> R.Case ("True",
      (* pattern [tT][rR][uU][eE] *) token env tok
    )
  | `False tok -> R.Case ("False",
      (* pattern [fF][aA][lL][sS][eE] *) token env tok
    )
  | `Curr_time_95f1cf7 tok -> R.Case ("Curr_time_95f1cf7",
      (* pattern [cC][uU][rR][rR][eE][nN][tT][__][tT][iI][mM][eE] *) token env tok
    )
  | `Curr_date tok -> R.Case ("Curr_date",
      (* pattern [cC][uU][rR][rR][eE][nN][tT][__][dD][aA][tT][eE] *) token env tok
    )
  | `Curr_time_72fc600 tok -> R.Case ("Curr_time_72fc600",
      (* pattern [cC][uU][rR][rR][eE][nN][tT][__][tT][iI][mM][eE][sS][tT][aA][mM][pP] *) token env tok
    )
  )

let map_error_message (env : env) (x : CST.error_message) =
  map_name env x

let map_anon_opt_opt_as_error_mess_6e99511 (env : env) (opt : CST.anon_opt_opt_as_error_mess_6e99511) =
  (match opt with
  | Some (v1, v2) -> R.Option (Some (
      let v1 =
        (match v1 with
        | Some tok -> R.Option (Some (
            (* pattern [aA][sS] *) token env tok
          ))
        | None -> R.Option None)
      in
      let v2 = map_error_message env v2 in
      R.Tuple [v1; v2]
    ))
  | None -> R.Option None)

let map_name2 (env : env) ((v1, v2) : CST.name2) =
  let v1 =
    (match v1 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = map_error_message env v1 in
        let v2 = (* "." *) token env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v2 = map_error_message env v2 in
  R.Tuple [v1; v2]

let map_savepoint_stmt (env : env) ((v1, v2) : CST.savepoint_stmt) =
  let v1 =
    (* pattern [sS][aA][vV][eE][pP][oO][iI][nN][tT] *) token env v1
  in
  let v2 = map_error_message env v2 in
  R.Tuple [v1; v2]

let map_type_name (env : env) ((v1, v2) : CST.type_name) =
  let v1 = R.List (List.map (map_error_message env) v1) in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        (match x with
        | `LPAR_signed_num_RPAR (v1, v2, v3) -> R.Case ("LPAR_signed_num_RPAR",
            let v1 = (* "(" *) token env v1 in
            let v2 = map_signed_number env v2 in
            let v3 = (* ")" *) token env v3 in
            R.Tuple [v1; v2; v3]
          )
        | `LPAR_signed_num_COMMA_signed_num_RPAR (v1, v2, v3, v4, v5) -> R.Case ("LPAR_signed_num_COMMA_signed_num_RPAR",
            let v1 = (* "(" *) token env v1 in
            let v2 = map_signed_number env v2 in
            let v3 = (* "," *) token env v3 in
            let v4 = map_signed_number env v4 in
            let v5 = (* ")" *) token env v5 in
            R.Tuple [v1; v2; v3; v4; v5]
          )
        )
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

let map_pragma_value (env : env) (x : CST.pragma_value) =
  (match x with
  | `Signed_num x -> R.Case ("Signed_num",
      map_signed_number env x
    )
  | `Name x -> R.Case ("Name",
      map_error_message env x
    )
  )

let map_anon_choice_inde_by_error_mess_0500888 (env : env) (x : CST.anon_choice_inde_by_error_mess_0500888) =
  (match x with
  | `Inde_by_name (v1, v2, v3) -> R.Case ("Inde_by_name",
      let v1 =
        (* pattern [iI][nN][dD][eE][xX][eE][dD] *) token env v1
      in
      let v2 = (* pattern [bB][yY] *) token env v2 in
      let v3 = map_error_message env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Not_inde (v1, v2) -> R.Case ("Not_inde",
      let v1 = (* pattern [nN][oO][tT] *) token env v1 in
      let v2 =
        (* pattern [iI][nN][dD][eE][xX][eE][dD] *) token env v2
      in
      R.Tuple [v1; v2]
    )
  )

let map_detach_stmt (env : env) ((v1, v2, v3) : CST.detach_stmt) =
  let v1 =
    (* pattern [dD][eE][tT][aA][cC][hH] *) token env v1
  in
  let v2 =
    (match v2 with
    | Some tok -> R.Option (Some (
        (* pattern [dD][aA][tT][aA][bB][aA][sS][eE] *) token env tok
      ))
    | None -> R.Option None)
  in
  let v3 = map_error_message env v3 in
  R.Tuple [v1; v2; v3]

let map_anon_opt_tran_opt_error_mess_65fc71e (env : env) (opt : CST.anon_opt_tran_opt_error_mess_65fc71e) =
  (match opt with
  | Some (v1, v2) -> R.Option (Some (
      let v1 =
        (* pattern [tT][rR][aA][nN][sS][aA][cC][tT][iI][oO][nN] *) token env v1
      in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_error_message env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2]
    ))
  | None -> R.Option None)

let map_release_stmt (env : env) ((v1, v2, v3) : CST.release_stmt) =
  let v1 =
    (* pattern [rR][eE][lL][eE][aA][sS][eE] *) token env v1
  in
  let v2 =
    (match v2 with
    | Some tok -> R.Option (Some (
        (* pattern [sS][aA][vV][eE][pP][oO][iI][nN][tT] *) token env tok
      ))
    | None -> R.Option None)
  in
  let v3 = map_error_message env v3 in
  R.Tuple [v1; v2; v3]

let map_column_name_list (env : env) ((v1, v2, v3, v4) : CST.column_name_list) =
  let v1 = (* "(" *) token env v1 in
  let v2 = map_error_message env v2 in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_error_message env v2 in
      R.Tuple [v1; v2]
    ) v3)
  in
  let v4 = (* ")" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

let map_analyze_stmt (env : env) ((v1, v2) : CST.analyze_stmt) =
  let v1 =
    (* pattern [aA][nN][aA][lL][yY][zZ][eE] *) token env v1
  in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_name2 env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

let map_reindex_stmt (env : env) ((v1, v2) : CST.reindex_stmt) =
  let v1 =
    (* pattern [rR][eE][iI][nN][dD][eE][xX] *) token env v1
  in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_name2 env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

let map_drop_trigger_stmt (env : env) ((v1, v2, v3, v4) : CST.drop_trigger_stmt) =
  let v1 = (* pattern [dD][rR][oO][pP] *) token env v1 in
  let v2 =
    (* pattern [tT][rR][iI][gG][gG][eE][rR] *) token env v2
  in
  let v3 =
    (match v3 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = (* pattern [iI][fF] *) token env v1 in
        let v2 =
          (* pattern [eE][xX][iI][sS][tT][sS] *) token env v2
        in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v4 = map_name2 env v4 in
  R.Tuple [v1; v2; v3; v4]

let map_drop_view_stmt (env : env) ((v1, v2, v3, v4) : CST.drop_view_stmt) =
  let v1 = (* pattern [dD][rR][oO][pP] *) token env v1 in
  let v2 = (* pattern [vV][iI][eE][wW] *) token env v2 in
  let v3 =
    (match v3 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = (* pattern [iI][fF] *) token env v1 in
        let v2 =
          (* pattern [eE][xX][iI][sS][tT][sS] *) token env v2
        in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v4 = map_name2 env v4 in
  R.Tuple [v1; v2; v3; v4]

let map_drop_table_stmt (env : env) ((v1, v2, v3, v4) : CST.drop_table_stmt) =
  let v1 = (* pattern [dD][rR][oO][pP] *) token env v1 in
  let v2 = (* pattern [tT][aA][bB][lL][eE] *) token env v2 in
  let v3 =
    (match v3 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = (* pattern [iI][fF] *) token env v1 in
        let v2 =
          (* pattern [eE][xX][iI][sS][tT][sS] *) token env v2
        in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v4 = map_name2 env v4 in
  R.Tuple [v1; v2; v3; v4]

let map_drop_index_stmt (env : env) ((v1, v2, v3, v4) : CST.drop_index_stmt) =
  let v1 = (* pattern [dD][rR][oO][pP] *) token env v1 in
  let v2 = (* pattern [iI][nN][dD][eE][xX] *) token env v2 in
  let v3 =
    (match v3 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = (* pattern [iI][fF] *) token env v1 in
        let v2 =
          (* pattern [eE][xX][iI][sS][tT][sS] *) token env v2
        in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v4 = map_name2 env v4 in
  R.Tuple [v1; v2; v3; v4]

let map_pragma_stmt (env : env) ((v1, v2, v3) : CST.pragma_stmt) =
  let v1 =
    (* pattern [pP][rR][aA][gG][mM][aA] *) token env v1
  in
  let v2 = map_name2 env v2 in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        (match x with
        | `EQ_pragma_value (v1, v2) -> R.Case ("EQ_pragma_value",
            let v1 = (* "=" *) token env v1 in
            let v2 = map_pragma_value env v2 in
            R.Tuple [v1; v2]
          )
        | `LPAR_pragma_value_RPAR (v1, v2, v3) -> R.Case ("LPAR_pragma_value_RPAR",
            let v1 = (* "(" *) token env v1 in
            let v2 = map_pragma_value env v2 in
            let v3 = (* ")" *) token env v3 in
            R.Tuple [v1; v2; v3]
          )
        )
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

let map_qualified_table_name (env : env) ((v1, v2, v3) : CST.qualified_table_name) =
  let v1 = map_name2 env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = (* pattern [aA][sS] *) token env v1 in
        let v2 = map_error_message env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_anon_choice_inde_by_error_mess_0500888 env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

let map_rollback_stmt (env : env) ((v1, v2, v3) : CST.rollback_stmt) =
  let v1 =
    (* pattern [rR][oO][lL][lL][bB][aA][cC][kK] *) token env v1
  in
  let v2 = map_anon_opt_tran_opt_error_mess_65fc71e env v2 in
  let v3 =
    (match v3 with
    | Some (v1, v2, v3) -> R.Option (Some (
        let v1 = (* pattern [tT][oO] *) token env v1 in
        let v2 =
          (match v2 with
          | Some tok -> R.Option (Some (
              (* pattern [sS][aA][vV][eE][pP][oO][iI][nN][tT] *) token env tok
            ))
          | None -> R.Option None)
        in
        let v3 = map_error_message env v3 in
        R.Tuple [v1; v2; v3]
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

let map_begin_stmt (env : env) ((v1, v2, v3) : CST.begin_stmt) =
  let v1 = (* pattern [bB][eE][gG][iI][nN] *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        (match x with
        | `Defe tok -> R.Case ("Defe",
            (* pattern [dD][eE][fF][eE][rR][rR][eE][dD] *) token env tok
          )
        | `Imme tok -> R.Case ("Imme",
            (* pattern [iI][mM][mM][eE][dD][iI][aA][tT][eE] *) token env tok
          )
        | `Excl tok -> R.Case ("Excl",
            (* pattern [eE][xX][cC][lL][uU][sS][iI][vV][eE] *) token env tok
          )
        )
      ))
    | None -> R.Option None)
  in
  let v3 = map_anon_opt_tran_opt_error_mess_65fc71e env v3 in
  R.Tuple [v1; v2; v3]

let map_commit_stmt (env : env) ((v1, v2) : CST.commit_stmt) =
  let v1 =
    (match v1 with
    | `Commit tok -> R.Case ("Commit",
        (* pattern [cC][oO][mM][mM][iI][tT] *) token env tok
      )
    | `End tok -> R.Case ("End",
        (* pattern [eE][nN][dD] *) token env tok
      )
    )
  in
  let v2 = map_anon_opt_tran_opt_error_mess_65fc71e env v2 in
  R.Tuple [v1; v2]

let map_anon_choice_error_mess_facbc16 (env : env) (x : CST.anon_choice_error_mess_facbc16) =
  (match x with
  | `Name x -> R.Case ("Name",
      map_error_message env x
    )
  | `Column_name_list x -> R.Case ("Column_name_list",
      map_column_name_list env x
    )
  )

let rec map_anon_COMMA_LPAR_file_rep_COMMA_file_RPAR_a378f47 (env : env) ((v1, v2, v3, v4, v5) : CST.anon_COMMA_LPAR_file_rep_COMMA_file_RPAR_a378f47) =
  let v1 = (* "," *) token env v1 in
  let v2 = (* "(" *) token env v2 in
  let v3 = map_filename env v3 in
  let v4 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_filename env v2 in
      R.Tuple [v1; v2]
    ) v4)
  in
  let v5 = (* ")" *) token env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_anon_file_rep_COMMA_file_3ba9398 (env : env) ((v1, v2) : CST.anon_file_rep_COMMA_file_3ba9398) =
  let v1 = map_filename env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_filename env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  R.Tuple [v1; v2]

and map_common_table_expression (env : env) (x : CST.common_table_expression) =
  (match x with
  | `Rectype (v1, v2, v3, v4, v5, v6, v7) -> R.Case ("Rectype",
      let v1 = map_error_message env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_column_name_list env x
          ))
        | None -> R.Option None)
      in
      let v3 = (* pattern [aA][sS] *) token env v3 in
      let v4 =
        (match v4 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 =
              (match v1 with
              | Some tok -> R.Option (Some (
                  (* pattern [nN][oO][tT] *) token env tok
                ))
              | None -> R.Option None)
            in
            let v2 =
              (* pattern [mM][aA][tT][eE][rR][iI][aA][lL][iI][zZ][eE][dD] *) token env v2
            in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      let v5 = (* "(" *) token env v5 in
      let v6 = map_select_stmt env v6 in
      let v7 = (* ")" *) token env v7 in
      R.Tuple [v1; v2; v3; v4; v5; v6; v7]
    )
  )

and map_expr (env : env) (x : CST.expr) =
  (match x with
  | `Lit_value x -> R.Case ("Lit_value",
      map_literal_value env x
    )
  | `Bind_param x -> R.Case ("Bind_param",
      map_bind_parameter env x
    )
  | `Name x -> R.Case ("Name",
      map_error_message env x
    )
  | `Name_DOT_name (v1, v2, v3) -> R.Case ("Name_DOT_name",
      let v1 = map_error_message env v1 in
      let v2 = (* "." *) token env v2 in
      let v3 = map_error_message env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Name_DOT_name_DOT_name (v1, v2, v3, v4, v5) -> R.Case ("Name_DOT_name_DOT_name",
      let v1 = map_error_message env v1 in
      let v2 = (* "." *) token env v2 in
      let v3 = map_error_message env v3 in
      let v4 = (* "." *) token env v4 in
      let v5 = map_error_message env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `TILDE_expr (v1, v2) -> R.Case ("TILDE_expr",
      let v1 = (* "~" *) token env v1 in
      let v2 = map_filename env v2 in
      R.Tuple [v1; v2]
    )
  | `Choice_DASH_expr (v1, v2) -> R.Case ("Choice_DASH_expr",
      let v1 =
        (match v1 with
        | `DASH tok -> R.Case ("DASH",
            (* "-" *) token env tok
          )
        | `PLUS tok -> R.Case ("PLUS",
            (* "+" *) token env tok
          )
        )
      in
      let v2 = map_filename env v2 in
      R.Tuple [v1; v2]
    )
  | `Not_expr (v1, v2) -> R.Case ("Not_expr",
      let v1 = (* pattern [nN][oO][tT] *) token env v1 in
      let v2 = map_filename env v2 in
      R.Tuple [v1; v2]
    )
  | `Expr_BARBAR_expr (v1, v2, v3) -> R.Case ("Expr_BARBAR_expr",
      let v1 = map_filename env v1 in
      let v2 = (* "||" *) token env v2 in
      let v3 = map_filename env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Expr_choice_STAR_expr (v1, v2, v3) -> R.Case ("Expr_choice_STAR_expr",
      let v1 = map_filename env v1 in
      let v2 =
        (match v2 with
        | `STAR tok -> R.Case ("STAR",
            (* "*" *) token env tok
          )
        | `SLASH tok -> R.Case ("SLASH",
            (* "/" *) token env tok
          )
        | `PERC tok -> R.Case ("PERC",
            (* "%" *) token env tok
          )
        )
      in
      let v3 = map_filename env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Expr_choice_PLUS_expr (v1, v2, v3) -> R.Case ("Expr_choice_PLUS_expr",
      let v1 = map_filename env v1 in
      let v2 = map_anon_choice_PLUS_da42005 env v2 in
      let v3 = map_filename env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Expr_choice_LTLT_expr (v1, v2, v3) -> R.Case ("Expr_choice_LTLT_expr",
      let v1 = map_filename env v1 in
      let v2 =
        (match v2 with
        | `LTLT tok -> R.Case ("LTLT",
            (* "<<" *) token env tok
          )
        | `GTGT tok -> R.Case ("GTGT",
            (* ">>" *) token env tok
          )
        | `AMP tok -> R.Case ("AMP",
            (* "&" *) token env tok
          )
        | `BAR tok -> R.Case ("BAR",
            (* "|" *) token env tok
          )
        )
      in
      let v3 = map_filename env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Expr_choice_LT_expr (v1, v2, v3) -> R.Case ("Expr_choice_LT_expr",
      let v1 = map_filename env v1 in
      let v2 =
        (match v2 with
        | `LT tok -> R.Case ("LT",
            (* "<" *) token env tok
          )
        | `LTEQ tok -> R.Case ("LTEQ",
            (* "<=" *) token env tok
          )
        | `GT tok -> R.Case ("GT",
            (* ">" *) token env tok
          )
        | `GTEQ tok -> R.Case ("GTEQ",
            (* ">=" *) token env tok
          )
        )
      in
      let v3 = map_filename env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Expr_choice_EQ_expr (v1, v2, v3) -> R.Case ("Expr_choice_EQ_expr",
      let v1 = map_filename env v1 in
      let v2 =
        (match v2 with
        | `EQ tok -> R.Case ("EQ",
            (* "=" *) token env tok
          )
        | `EQEQ tok -> R.Case ("EQEQ",
            (* "==" *) token env tok
          )
        | `BANGEQ tok -> R.Case ("BANGEQ",
            (* "!=" *) token env tok
          )
        | `LTGT tok -> R.Case ("LTGT",
            (* "<>" *) token env tok
          )
        )
      in
      let v3 = map_filename env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Expr_opt_not_in_choice_LPAR_opt_choice_select_stmt_RPAR (v1, v2, v3, v4) -> R.Case ("Expr_opt_not_in_choice_LPAR_opt_choice_select_stmt_RPAR",
      let v1 = map_filename env v1 in
      let v2 =
        (match v2 with
        | Some tok -> R.Option (Some (
            (* pattern [nN][oO][tT] *) token env tok
          ))
        | None -> R.Option None)
      in
      let v3 = (* pattern [iI][nN] *) token env v3 in
      let v4 =
        (match v4 with
        | `LPAR_opt_choice_select_stmt_RPAR (v1, v2, v3) -> R.Case ("LPAR_opt_choice_select_stmt_RPAR",
            let v1 = (* "(" *) token env v1 in
            let v2 =
              (match v2 with
              | Some x -> R.Option (Some (
                  (match x with
                  | `Select_stmt x -> R.Case ("Select_stmt",
                      map_select_stmt env x
                    )
                  | `Expr_rep_COMMA_expr x -> R.Case ("Expr_rep_COMMA_expr",
                      map_anon_file_rep_COMMA_file_3ba9398 env x
                    )
                  )
                ))
              | None -> R.Option None)
            in
            let v3 = (* ")" *) token env v3 in
            R.Tuple [v1; v2; v3]
          )
        | `Name2_opt_LPAR_opt_expr_rep_COMMA_expr_RPAR (v1, v2) -> R.Case ("Name2_opt_LPAR_opt_expr_rep_COMMA_expr_RPAR",
            let v1 = map_name2 env v1 in
            let v2 =
              (match v2 with
              | Some (v1, v2, v3) -> R.Option (Some (
                  let v1 = (* "(" *) token env v1 in
                  let v2 =
                    (match v2 with
                    | Some x -> R.Option (Some (
                        map_anon_file_rep_COMMA_file_3ba9398 env x
                      ))
                    | None -> R.Option None)
                  in
                  let v3 = (* ")" *) token env v3 in
                  R.Tuple [v1; v2; v3]
                ))
              | None -> R.Option None)
            in
            R.Tuple [v1; v2]
          )
        )
      in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Expr_and_expr (v1, v2, v3) -> R.Case ("Expr_and_expr",
      let v1 = map_filename env v1 in
      let v2 = (* pattern [aA][nN][dD] *) token env v2 in
      let v3 = map_filename env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Expr_or_expr (v1, v2, v3) -> R.Case ("Expr_or_expr",
      let v1 = map_filename env v1 in
      let v2 = (* pattern [oO][rR] *) token env v2 in
      let v3 = map_filename env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Func_name_LPAR_opt_choice_opt_dist_expr_rep_COMMA_expr_RPAR_opt_filter_clause_opt_over_clause (v1, v2, v3, v4, v5, v6) -> R.Case ("Func_name_LPAR_opt_choice_opt_dist_expr_rep_COMMA_expr_RPAR_opt_filter_clause_opt_over_clause",
      let v1 = map_function_name env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            (match x with
            | `Opt_dist_expr_rep_COMMA_expr (v1, v2, v3) -> R.Case ("Opt_dist_expr_rep_COMMA_expr",
                let v1 =
                  (match v1 with
                  | Some tok -> R.Option (Some (
                      (* pattern [dD][iI][sS][tT][iI][nN][cC][tT] *) token env tok
                    ))
                  | None -> R.Option None)
                in
                let v2 = map_filename env v2 in
                let v3 =
                  R.List (List.map (fun (v1, v2) ->
                    let v1 = (* "," *) token env v1 in
                    let v2 = map_filename env v2 in
                    R.Tuple [v1; v2]
                  ) v3)
                in
                R.Tuple [v1; v2; v3]
              )
            | `STAR tok -> R.Case ("STAR",
                (* "*" *) token env tok
              )
            )
          ))
        | None -> R.Option None)
      in
      let v4 = (* ")" *) token env v4 in
      let v5 =
        (match v5 with
        | Some x -> R.Option (Some (
            map_filter_clause env x
          ))
        | None -> R.Option None)
      in
      let v6 =
        (match v6 with
        | Some x -> R.Option (Some (
            map_over_clause env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4; v5; v6]
    )
  | `LPAR_expr_rep_COMMA_expr_RPAR (v1, v2, v3, v4) -> R.Case ("LPAR_expr_rep_COMMA_expr_RPAR",
      let v1 = (* "(" *) token env v1 in
      let v2 = map_filename env v2 in
      let v3 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = (* "," *) token env v1 in
          let v2 = map_filename env v2 in
          R.Tuple [v1; v2]
        ) v3)
      in
      let v4 = (* ")" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Cast_LPAR_expr_as_type_name_RPAR (v1, v2, v3, v4, v5, v6) -> R.Case ("Cast_LPAR_expr_as_type_name_RPAR",
      let v1 = (* pattern [cC][aA][sS][tT] *) token env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 = map_filename env v3 in
      let v4 = (* pattern [aA][sS] *) token env v4 in
      let v5 = map_type_name env v5 in
      let v6 = (* ")" *) token env v6 in
      R.Tuple [v1; v2; v3; v4; v5; v6]
    )
  | `Expr_coll_coll_name (v1, v2, v3) -> R.Case ("Expr_coll_coll_name",
      let v1 = map_filename env v1 in
      let v2 =
        (* pattern [cC][oO][lL][lL][aA][tT][eE] *) token env v2
      in
      let v3 = map_collation_name env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Expr_opt_not_choice_like_expr_opt_esc_expr (v1, v2, v3, v4, v5) -> R.Case ("Expr_opt_not_choice_like_expr_opt_esc_expr",
      let v1 = map_filename env v1 in
      let v2 =
        (match v2 with
        | Some tok -> R.Option (Some (
            (* pattern [nN][oO][tT] *) token env tok
          ))
        | None -> R.Option None)
      in
      let v3 =
        (match v3 with
        | `Like tok -> R.Case ("Like",
            (* pattern [lL][iI][kK][eE] *) token env tok
          )
        | `Glob tok -> R.Case ("Glob",
            (* pattern [gG][lL][oO][bB] *) token env tok
          )
        | `Regex tok -> R.Case ("Regex",
            (* pattern [rR][eE][gG][eE][xX][pP] *) token env tok
          )
        | `Match tok -> R.Case ("Match",
            (* pattern [mM][aA][tT][cC][hH] *) token env tok
          )
        )
      in
      let v4 = map_filename env v4 in
      let v5 =
        (match v5 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 =
              (* pattern [eE][sS][cC][aA][pP][eE] *) token env v1
            in
            let v2 = map_filename env v2 in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Expr_choice_isnull (v1, v2) -> R.Case ("Expr_choice_isnull",
      let v1 = map_filename env v1 in
      let v2 =
        (match v2 with
        | `Isnull tok -> R.Case ("Isnull",
            (* pattern [iI][sS][nN][uU][lL][lL] *) token env tok
          )
        | `Notn tok -> R.Case ("Notn",
            (* pattern [nN][oO][tT][nN][uU][lL][lL] *) token env tok
          )
        | `Not_null (v1, v2) -> R.Case ("Not_null",
            let v1 = (* pattern [nN][oO][tT] *) token env v1 in
            let v2 = (* pattern [nN][uU][lL][lL] *) token env v2 in
            R.Tuple [v1; v2]
          )
        )
      in
      R.Tuple [v1; v2]
    )
  | `Expr_is_opt_not_expr (v1, v2, v3, v4) -> R.Case ("Expr_is_opt_not_expr",
      let v1 = map_filename env v1 in
      let v2 = (* pattern [iI][sS] *) token env v2 in
      let v3 =
        (match v3 with
        | Some tok -> R.Option (Some (
            (* pattern [nN][oO][tT] *) token env tok
          ))
        | None -> R.Option None)
      in
      let v4 = map_filename env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Expr_opt_not_betw_expr_and_expr (v1, v2, v3, v4, v5, v6) -> R.Case ("Expr_opt_not_betw_expr_and_expr",
      let v1 = map_filename env v1 in
      let v2 =
        (match v2 with
        | Some tok -> R.Option (Some (
            (* pattern [nN][oO][tT] *) token env tok
          ))
        | None -> R.Option None)
      in
      let v3 =
        (* pattern [bB][eE][tT][wW][eE][eE][nN] *) token env v3
      in
      let v4 = map_filename env v4 in
      let v5 = (* pattern [aA][nN][dD] *) token env v5 in
      let v6 = map_filename env v6 in
      R.Tuple [v1; v2; v3; v4; v5; v6]
    )
  | `LPAR_select_stmt_RPAR (v1, v2, v3) -> R.Case ("LPAR_select_stmt_RPAR",
      let v1 = (* "(" *) token env v1 in
      let v2 = map_select_stmt env v2 in
      let v3 = (* ")" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exists_LPAR_select_stmt_RPAR (v1, v2, v3, v4) -> R.Case ("Exists_LPAR_select_stmt_RPAR",
      let v1 =
        (* pattern [eE][xX][iI][sS][tT][sS] *) token env v1
      in
      let v2 = (* "(" *) token env v2 in
      let v3 = map_select_stmt env v3 in
      let v4 = (* ")" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Not_exists_LPAR_select_stmt_RPAR (v1, v2, v3, v4, v5) -> R.Case ("Not_exists_LPAR_select_stmt_RPAR",
      let v1 = (* pattern [nN][oO][tT] *) token env v1 in
      let v2 =
        (* pattern [eE][xX][iI][sS][tT][sS] *) token env v2
      in
      let v3 = (* "(" *) token env v3 in
      let v4 = map_select_stmt env v4 in
      let v5 = (* ")" *) token env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Case_opt_expr_rep1_when_expr_then_expr_opt_else_expr_end (v1, v2, v3, v4, v5) -> R.Case ("Case_opt_expr_rep1_when_expr_then_expr_opt_else_expr_end",
      let v1 = (* pattern [cC][aA][sS][eE] *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_filename env x
          ))
        | None -> R.Option None)
      in
      let v3 =
        R.List (List.map (fun (v1, v2, v3, v4) ->
          let v1 = (* pattern [wW][hH][eE][nN] *) token env v1 in
          let v2 = map_filename env v2 in
          let v3 = (* pattern [tT][hH][eE][nN] *) token env v3 in
          let v4 = map_filename env v4 in
          R.Tuple [v1; v2; v3; v4]
        ) v3)
      in
      let v4 =
        (match v4 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = (* pattern [eE][lL][sS][eE] *) token env v1 in
            let v2 = map_filename env v2 in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      let v5 = (* pattern [eE][nN][dD] *) token env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Raise_func (v1, v2, v3, v4) -> R.Case ("Raise_func",
      let v1 = (* pattern [rR][aA][iI][sS][eE] *) token env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 =
        (match v3 with
        | `Ignore tok -> R.Case ("Ignore",
            (* pattern [iI][gG][nN][oO][rR][eE] *) token env tok
          )
        | `Choice_roll_COMMA_error_mess (v1, v2, v3) -> R.Case ("Choice_roll_COMMA_error_mess",
            let v1 =
              (match v1 with
              | `Roll tok -> R.Case ("Roll",
                  (* pattern [rR][oO][lL][lL][bB][aA][cC][kK] *) token env tok
                )
              | `Abort tok -> R.Case ("Abort",
                  (* pattern [aA][bB][oO][rR][tT] *) token env tok
                )
              | `Fail tok -> R.Case ("Fail",
                  (* pattern [fF][aA][iI][lL] *) token env tok
                )
              )
            in
            let v2 = (* "," *) token env v2 in
            let v3 = map_error_message env v3 in
            R.Tuple [v1; v2; v3]
          )
        )
      in
      let v4 = (* ")" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  )

and map_filename (env : env) (x : CST.filename) =
  map_expr env x

and map_filter_clause (env : env) ((v1, v2, v3, v4, v5) : CST.filter_clause) =
  let v1 =
    (* pattern [fF][iI][lL][tT][eE][rR] *) token env v1
  in
  let v2 = (* "(" *) token env v2 in
  let v3 = (* pattern [wW][hH][eE][rR][eE] *) token env v3 in
  let v4 = map_filename env v4 in
  let v5 = (* ")" *) token env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_frame_spec (env : env) ((v1, v2, v3) : CST.frame_spec) =
  let v1 =
    (match v1 with
    | `Range tok -> R.Case ("Range",
        (* pattern [rR][aA][nN][gG][eE] *) token env tok
      )
    | `Rows tok -> R.Case ("Rows",
        (* pattern [rR][oO][wW][sS] *) token env tok
      )
    | `Groups tok -> R.Case ("Groups",
        (* pattern [gG][rR][oO][uU][pP][sS] *) token env tok
      )
    )
  in
  let v2 =
    (match v2 with
    | `Betw_choice_unbo_prec_and_choice_expr_prec (v1, v2, v3, v4) -> R.Case ("Betw_choice_unbo_prec_and_choice_expr_prec",
        let v1 =
          (* pattern [bB][eE][tT][wW][eE][eE][nN] *) token env v1
        in
        let v2 =
          (match v2 with
          | `Unbo_prec (v1, v2) -> R.Case ("Unbo_prec",
              let v1 =
                (* pattern [uU][nN][bB][oO][uU][nN][dD][eE][dD] *) token env v1
              in
              let v2 =
                (* pattern [pP][rR][eE][cC][eE][dD][iI][nN][gG] *) token env v2
              in
              R.Tuple [v1; v2]
            )
          | `Expr_prec (v1, v2) -> R.Case ("Expr_prec",
              let v1 = map_filename env v1 in
              let v2 =
                (* pattern [pP][rR][eE][cC][eE][dD][iI][nN][gG] *) token env v2
              in
              R.Tuple [v1; v2]
            )
          | `Curr_row (v1, v2) -> R.Case ("Curr_row",
              let v1 =
                (* pattern [cC][uU][rR][rR][eE][nN][tT] *) token env v1
              in
              let v2 = (* pattern [rR][oO][wW] *) token env v2 in
              R.Tuple [v1; v2]
            )
          | `Expr_foll (v1, v2) -> R.Case ("Expr_foll",
              let v1 = map_filename env v1 in
              let v2 =
                (* pattern [fF][oO][lL][lL][oO][wW][iI][nN][gG] *) token env v2
              in
              R.Tuple [v1; v2]
            )
          )
        in
        let v3 = (* pattern [aA][nN][dD] *) token env v3 in
        let v4 =
          (match v4 with
          | `Expr_prec (v1, v2) -> R.Case ("Expr_prec",
              let v1 = map_filename env v1 in
              let v2 =
                (* pattern [pP][rR][eE][cC][eE][dD][iI][nN][gG] *) token env v2
              in
              R.Tuple [v1; v2]
            )
          | `Curr_row (v1, v2) -> R.Case ("Curr_row",
              let v1 =
                (* pattern [cC][uU][rR][rR][eE][nN][tT] *) token env v1
              in
              let v2 = (* pattern [rR][oO][wW] *) token env v2 in
              R.Tuple [v1; v2]
            )
          | `Expr_foll (v1, v2) -> R.Case ("Expr_foll",
              let v1 = map_filename env v1 in
              let v2 =
                (* pattern [fF][oO][lL][lL][oO][wW][iI][nN][gG] *) token env v2
              in
              R.Tuple [v1; v2]
            )
          | `Unbo_foll (v1, v2) -> R.Case ("Unbo_foll",
              let v1 =
                (* pattern [uU][nN][bB][oO][uU][nN][dD][eE][dD] *) token env v1
              in
              let v2 =
                (* pattern [fF][oO][lL][lL][oO][wW][iI][nN][gG] *) token env v2
              in
              R.Tuple [v1; v2]
            )
          )
        in
        R.Tuple [v1; v2; v3; v4]
      )
    | `Unbo_prec (v1, v2) -> R.Case ("Unbo_prec",
        let v1 =
          (* pattern [uU][nN][bB][oO][uU][nN][dD][eE][dD] *) token env v1
        in
        let v2 =
          (* pattern [pP][rR][eE][cC][eE][dD][iI][nN][gG] *) token env v2
        in
        R.Tuple [v1; v2]
      )
    | `Expr_prec (v1, v2) -> R.Case ("Expr_prec",
        let v1 = map_filename env v1 in
        let v2 =
          (* pattern [pP][rR][eE][cC][eE][dD][iI][nN][gG] *) token env v2
        in
        R.Tuple [v1; v2]
      )
    | `Curr_row (v1, v2) -> R.Case ("Curr_row",
        let v1 =
          (* pattern [cC][uU][rR][rR][eE][nN][tT] *) token env v1
        in
        let v2 = (* pattern [rR][oO][wW] *) token env v2 in
        R.Tuple [v1; v2]
      )
    )
  in
  let v3 =
    (match v3 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 =
          (* pattern [eE][xX][cC][lL][uU][dD][eE] *) token env v1
        in
        let v2 =
          (match v2 with
          | `Not_others (v1, v2) -> R.Case ("Not_others",
              let v1 = (* pattern [nN][oO][tT] *) token env v1 in
              let v2 =
                (* pattern [oO][tT][hH][eE][rR][sS] *) token env v2
              in
              R.Tuple [v1; v2]
            )
          | `Curr_row (v1, v2) -> R.Case ("Curr_row",
              let v1 =
                (* pattern [cC][uU][rR][rR][eE][nN][tT] *) token env v1
              in
              let v2 = (* pattern [rR][oO][wW] *) token env v2 in
              R.Tuple [v1; v2]
            )
          | `Group tok -> R.Case ("Group",
              (* pattern [gG][rR][oO][uU][pP] *) token env tok
            )
          | `Ties tok -> R.Case ("Ties",
              (* pattern [tT][iI][eE][sS] *) token env tok
            )
          )
        in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

and map_from_clause (env : env) ((v1, v2) : CST.from_clause) =
  let v1 = (* pattern [fF][rR][oO][mM] *) token env v1 in
  let v2 = map_join_clause env v2 in
  R.Tuple [v1; v2]

and map_group_by_clause (env : env) ((v1, v2, v3, v4, v5) : CST.group_by_clause) =
  let v1 = (* pattern [gG][rR][oO][uU][pP] *) token env v1 in
  let v2 = (* pattern [bB][yY] *) token env v2 in
  let v3 = map_filename env v3 in
  let v4 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_filename env v2 in
      R.Tuple [v1; v2]
    ) v4)
  in
  let v5 =
    (match v5 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 =
          (* pattern [hH][aA][vV][iI][nN][gG] *) token env v1
        in
        let v2 = map_filename env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4; v5]

and map_join_clause (env : env) ((v1, v2) : CST.join_clause) =
  let v1 = map_table_or_subquery env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2, v3) ->
      let v1 = map_join_operator env v1 in
      let v2 = map_table_or_subquery env v2 in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_join_constraint env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3]
    ) v2)
  in
  R.Tuple [v1; v2]

and map_join_constraint (env : env) (x : CST.join_constraint) =
  (match x with
  | `On_expr (v1, v2) -> R.Case ("On_expr",
      let v1 = (* pattern [oO][nN] *) token env v1 in
      let v2 = map_filename env v2 in
      R.Tuple [v1; v2]
    )
  | `Using_LPAR_name_rep_COMMA_name_RPAR (v1, v2, v3, v4, v5) -> R.Case ("Using_LPAR_name_rep_COMMA_name_RPAR",
      let v1 = (* pattern [uU][sS][iI][nN][gG] *) token env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 = map_error_message env v3 in
      let v4 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = (* "," *) token env v1 in
          let v2 = map_error_message env v2 in
          R.Tuple [v1; v2]
        ) v4)
      in
      let v5 = (* ")" *) token env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  )

and map_limit_clause (env : env) ((v1, v2, v3) : CST.limit_clause) =
  let v1 = (* pattern [lL][iI][mM][iI][tT] *) token env v1 in
  let v2 = map_filename env v2 in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        (match x with
        | `Offset_expr (v1, v2) -> R.Case ("Offset_expr",
            let v1 =
              (* pattern [oO][fF][fF][sS][eE][tT] *) token env v1
            in
            let v2 = map_filename env v2 in
            R.Tuple [v1; v2]
          )
        | `COMMA_expr (v1, v2) -> R.Case ("COMMA_expr",
            let v1 = (* "," *) token env v1 in
            let v2 = map_filename env v2 in
            R.Tuple [v1; v2]
          )
        )
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

and map_order_by_clause (env : env) ((v1, v2, v3, v4) : CST.order_by_clause) =
  let v1 = (* pattern [oO][rR][dD][eE][rR] *) token env v1 in
  let v2 = (* pattern [bB][yY] *) token env v2 in
  let v3 = map_ordering_term env v3 in
  let v4 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_ordering_term env v2 in
      R.Tuple [v1; v2]
    ) v4)
  in
  R.Tuple [v1; v2; v3; v4]

and map_ordering_term (env : env) ((v1, v2, v3) : CST.ordering_term) =
  let v1 = map_filename env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_anon_choice_asc_fa4fd8b env x
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = (* pattern [nN][uU][lL][lL][sS] *) token env v1 in
        let v2 =
          (match v2 with
          | `First tok -> R.Case ("First",
              (* pattern [fF][iI][rR][sS][tT] *) token env tok
            )
          | `Last tok -> R.Case ("Last",
              (* pattern [lL][aA][sS][tT] *) token env tok
            )
          )
        in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

and map_over_clause (env : env) ((v1, v2) : CST.over_clause) =
  let v1 = (* pattern [oO][vV][eE][rR] *) token env v1 in
  let v2 =
    (match v2 with
    | `Name x -> R.Case ("Name",
        map_error_message env x
      )
    | `LPAR_opt_name_opt_part_by_expr_rep_COMMA_expr_opt_order_by_orde_term_rep_COMMA_orde_term_opt_frame_spec_RPAR x -> R.Case ("LPAR_opt_name_opt_part_by_expr_rep_COMMA_expr_opt_order_by_orde_term_rep_COMMA_orde_term_opt_frame_spec_RPAR",
        map_window_defn env x
      )
    )
  in
  R.Tuple [v1; v2]

and map_result_column (env : env) (x : CST.result_column) =
  (match x with
  | `Name_DOT_STAR (v1, v2, v3) -> R.Case ("Name_DOT_STAR",
      let v1 = map_error_message env v1 in
      let v2 = (* "." *) token env v2 in
      let v3 = (* "*" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `STAR tok -> R.Case ("STAR",
      (* "*" *) token env tok
    )
  | `Expr_opt_opt_as_name (v1, v2) -> R.Case ("Expr_opt_opt_as_name",
      let v1 = map_filename env v1 in
      let v2 = map_anon_opt_opt_as_error_mess_6e99511 env v2 in
      R.Tuple [v1; v2]
    )
  )

and map_select_core (env : env) (x : CST.select_core) =
  (match x with
  | `Select_opt_choice_dist_result_column_rep_COMMA_result_column_opt_from_clause_opt_where_clause_opt_group_by_clause_opt_window_clause (v1, v2, v3, v4, v5, v6, v7, v8) -> R.Case ("Select_opt_choice_dist_result_column_rep_COMMA_result_column_opt_from_clause_opt_where_clause_opt_group_by_clause_opt_window_clause",
      let v1 =
        (* pattern [sS][eE][lL][eE][cC][tT] *) token env v1
      in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            (match x with
            | `Dist tok -> R.Case ("Dist",
                (* pattern [dD][iI][sS][tT][iI][nN][cC][tT] *) token env tok
              )
            | `All tok -> R.Case ("All",
                (* pattern [aA][lL][lL] *) token env tok
              )
            )
          ))
        | None -> R.Option None)
      in
      let v3 = map_result_column env v3 in
      let v4 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = (* "," *) token env v1 in
          let v2 = map_result_column env v2 in
          R.Tuple [v1; v2]
        ) v4)
      in
      let v5 =
        (match v5 with
        | Some x -> R.Option (Some (
            map_from_clause env x
          ))
        | None -> R.Option None)
      in
      let v6 =
        (match v6 with
        | Some x -> R.Option (Some (
            map_where_clause env x
          ))
        | None -> R.Option None)
      in
      let v7 =
        (match v7 with
        | Some x -> R.Option (Some (
            map_group_by_clause env x
          ))
        | None -> R.Option None)
      in
      let v8 =
        (match v8 with
        | Some x -> R.Option (Some (
            map_window_clause env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4; v5; v6; v7; v8]
    )
  | `Values_LPAR_expr_rep_COMMA_expr_RPAR_rep_COMMA_LPAR_expr_rep_COMMA_expr_RPAR (v1, v2, v3, v4, v5, v6) -> R.Case ("Values_LPAR_expr_rep_COMMA_expr_RPAR_rep_COMMA_LPAR_expr_rep_COMMA_expr_RPAR",
      let v1 =
        (* pattern [vV][aA][lL][uU][eE][sS] *) token env v1
      in
      let v2 = (* "(" *) token env v2 in
      let v3 = map_filename env v3 in
      let v4 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = (* "," *) token env v1 in
          let v2 = map_filename env v2 in
          R.Tuple [v1; v2]
        ) v4)
      in
      let v5 = (* ")" *) token env v5 in
      let v6 =
        R.List (List.map (map_anon_COMMA_LPAR_file_rep_COMMA_file_RPAR_a378f47 env) v6)
      in
      R.Tuple [v1; v2; v3; v4; v5; v6]
    )
  )

and map_select_stmt (env : env) (x : CST.select_stmt) =
  (match x with
  | `Rectype (v1, v2, v3, v4, v5) -> R.Case ("Rectype",
      let v1 =
        (match v1 with
        | Some x -> R.Option (Some (
            map_with_clause env x
          ))
        | None -> R.Option None)
      in
      let v2 = map_select_core env v2 in
      let v3 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = map_compound_operator env v1 in
          let v2 = map_select_core env v2 in
          R.Tuple [v1; v2]
        ) v3)
      in
      let v4 =
        (match v4 with
        | Some x -> R.Option (Some (
            map_order_by_clause env x
          ))
        | None -> R.Option None)
      in
      let v5 =
        (match v5 with
        | Some x -> R.Option (Some (
            map_limit_clause env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  )

and map_table_or_subquery (env : env) (x : CST.table_or_subquery) =
  (match x with
  | `Name2_opt_opt_as_name_opt_choice_inde_by_name (v1, v2, v3) -> R.Case ("Name2_opt_opt_as_name_opt_choice_inde_by_name",
      let v1 = map_name2 env v1 in
      let v2 = map_anon_opt_opt_as_error_mess_6e99511 env v2 in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_anon_choice_inde_by_error_mess_0500888 env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3]
    )
  | `Name2_LPAR_expr_rep_COMMA_expr_RPAR_opt_opt_as_name (v1, v2, v3, v4, v5, v6) -> R.Case ("Name2_LPAR_expr_rep_COMMA_expr_RPAR_opt_opt_as_name",
      let v1 = map_name2 env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 = map_filename env v3 in
      let v4 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = (* "," *) token env v1 in
          let v2 = map_filename env v2 in
          R.Tuple [v1; v2]
        ) v4)
      in
      let v5 = (* ")" *) token env v5 in
      let v6 = map_anon_opt_opt_as_error_mess_6e99511 env v6 in
      R.Tuple [v1; v2; v3; v4; v5; v6]
    )
  | `LPAR_select_stmt_RPAR_opt_opt_as_name (v1, v2, v3, v4) -> R.Case ("LPAR_select_stmt_RPAR_opt_opt_as_name",
      let v1 = (* "(" *) token env v1 in
      let v2 = map_select_stmt env v2 in
      let v3 = (* ")" *) token env v3 in
      let v4 = map_anon_opt_opt_as_error_mess_6e99511 env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `LPAR_join_clause_RPAR (v1, v2, v3) -> R.Case ("LPAR_join_clause_RPAR",
      let v1 = (* "(" *) token env v1 in
      let v2 = map_join_clause env v2 in
      let v3 = (* ")" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_where_clause (env : env) ((v1, v2) : CST.where_clause) =
  let v1 = (* pattern [wW][hH][eE][rR][eE] *) token env v1 in
  let v2 = map_filename env v2 in
  R.Tuple [v1; v2]

and map_window_clause (env : env) ((v1, v2, v3, v4, v5) : CST.window_clause) =
  let v1 =
    (* pattern [wW][iI][nN][dD][oO][wW] *) token env v1
  in
  let v2 = map_error_message env v2 in
  let v3 = (* pattern [aA][sS] *) token env v3 in
  let v4 = map_window_defn env v4 in
  let v5 =
    R.List (List.map (fun (v1, v2, v3, v4) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_error_message env v2 in
      let v3 = (* pattern [aA][sS] *) token env v3 in
      let v4 = map_window_defn env v4 in
      R.Tuple [v1; v2; v3; v4]
    ) v5)
  in
  R.Tuple [v1; v2; v3; v4; v5]

and map_window_defn (env : env) ((v1, v2, v3, v4, v5, v6) : CST.window_defn) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_error_message env x
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | Some (v1, v2, v3, v4) -> R.Option (Some (
        let v1 =
          (* pattern [pP][aA][rR][tT][iI][tT][iI][oO][nN] *) token env v1
        in
        let v2 = (* pattern [bB][yY] *) token env v2 in
        let v3 = map_filename env v3 in
        let v4 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_filename env v2 in
            R.Tuple [v1; v2]
          ) v4)
        in
        R.Tuple [v1; v2; v3; v4]
      ))
    | None -> R.Option None)
  in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_order_by_clause env x
      ))
    | None -> R.Option None)
  in
  let v5 =
    (match v5 with
    | Some x -> R.Option (Some (
        map_frame_spec env x
      ))
    | None -> R.Option None)
  in
  let v6 = (* ")" *) token env v6 in
  R.Tuple [v1; v2; v3; v4; v5; v6]

and map_with_clause (env : env) (x : CST.with_clause) =
  (match x with
  | `Rectype (v1, v2, v3, v4) -> R.Case ("Rectype",
      let v1 = (* pattern [wW][iI][tT][hH] *) token env v1 in
      let v2 =
        (match v2 with
        | Some tok -> R.Option (Some (
            (* pattern [rR][eE][cC][uU][rR][sS][iI][vV][eE] *) token env tok
          ))
        | None -> R.Option None)
      in
      let v3 = map_common_table_expression env v3 in
      let v4 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = (* "," *) token env v1 in
          let v2 = map_common_table_expression env v2 in
          R.Tuple [v1; v2]
        ) v4)
      in
      R.Tuple [v1; v2; v3; v4]
    )
  )

let map_foreign_key_clause (env : env) ((v1, v2, v3, v4, v5) : CST.foreign_key_clause) =
  let v1 =
    (* pattern [rR][eE][fF][eE][rR][eE][nN][cC][eE][sS] *) token env v1
  in
  let v2 = map_error_message env v2 in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_column_name_list env x
      ))
    | None -> R.Option None)
  in
  let v4 =
    R.List (List.map (fun x ->
      (match x with
      | `On_choice_delete_choice_set_null (v1, v2, v3) -> R.Case ("On_choice_delete_choice_set_null",
          let v1 = (* pattern [oO][nN] *) token env v1 in
          let v2 =
            (match v2 with
            | `Delete tok -> R.Case ("Delete",
                (* pattern [dD][eE][lL][eE][tT][eE] *) token env tok
              )
            | `Update tok -> R.Case ("Update",
                (* pattern [uU][pP][dD][aA][tT][eE] *) token env tok
              )
            )
          in
          let v3 =
            (match v3 with
            | `Set_null (v1, v2) -> R.Case ("Set_null",
                let v1 = (* pattern [sS][eE][tT] *) token env v1 in
                let v2 = (* pattern [nN][uU][lL][lL] *) token env v2 in
                R.Tuple [v1; v2]
              )
            | `Set_defa (v1, v2) -> R.Case ("Set_defa",
                let v1 = (* pattern [sS][eE][tT] *) token env v1 in
                let v2 =
                  (* pattern [dD][eE][fF][aA][uU][lL][tT] *) token env v2
                in
                R.Tuple [v1; v2]
              )
            | `Casc tok -> R.Case ("Casc",
                (* pattern [cC][aA][sS][cC][aA][dD][eE] *) token env tok
              )
            | `Rest tok -> R.Case ("Rest",
                (* pattern [rR][eE][sS][tT][rR][iI][cC][tT] *) token env tok
              )
            | `No_action (v1, v2) -> R.Case ("No_action",
                let v1 = (* pattern [nN][oO] *) token env v1 in
                let v2 =
                  (* pattern [aA][cC][tT][iI][oO][nN] *) token env v2
                in
                R.Tuple [v1; v2]
              )
            )
          in
          R.Tuple [v1; v2; v3]
        )
      | `Match_name (v1, v2) -> R.Case ("Match_name",
          let v1 = (* pattern [mM][aA][tT][cC][hH] *) token env v1 in
          let v2 = map_error_message env v2 in
          R.Tuple [v1; v2]
        )
      )
    ) v4)
  in
  let v5 =
    (match v5 with
    | Some (v1, v2, v3) -> R.Option (Some (
        let v1 =
          (match v1 with
          | Some tok -> R.Option (Some (
              (* pattern [nN][oO][tT] *) token env tok
            ))
          | None -> R.Option None)
        in
        let v2 =
          (* pattern [dD][eE][fF][eE][rR][rR][aA][bB][lL][eE] *) token env v2
        in
        let v3 =
          (match v3 with
          | Some x -> R.Option (Some (
              (match x with
              | `Init_defe (v1, v2) -> R.Case ("Init_defe",
                  let v1 =
                    (* pattern [iI][nN][iI][tT][iI][aA][lL][lL][yY] *) token env v1
                  in
                  let v2 =
                    (* pattern [dD][eE][fF][eE][rR][rR][eE][dD] *) token env v2
                  in
                  R.Tuple [v1; v2]
                )
              | `Init_imme (v1, v2) -> R.Case ("Init_imme",
                  let v1 =
                    (* pattern [iI][nN][iI][tT][iI][aA][lL][lL][yY] *) token env v1
                  in
                  let v2 =
                    (* pattern [iI][mM][mM][eE][dD][iI][aA][tT][eE] *) token env v2
                  in
                  R.Tuple [v1; v2]
                )
              )
            ))
          | None -> R.Option None)
        in
        R.Tuple [v1; v2; v3]
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4; v5]

let map_indexed_column (env : env) ((v1, v2) : CST.indexed_column) =
  let v1 = map_filename env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_anon_choice_asc_fa4fd8b env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

let map_attach_stmt (env : env) ((v1, v2, v3, v4, v5) : CST.attach_stmt) =
  let v1 =
    (* pattern [aA][tT][tT][aA][cC][hH] *) token env v1
  in
  let v2 =
    (match v2 with
    | Some tok -> R.Option (Some (
        (* pattern [dD][aA][tT][aA][bB][aA][sS][eE] *) token env tok
      ))
    | None -> R.Option None)
  in
  let v3 = map_filename env v3 in
  let v4 = (* pattern [aA][sS] *) token env v4 in
  let v5 = map_error_message env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

let map_vacuum_stmt (env : env) ((v1, v2, v3) : CST.vacuum_stmt) =
  let v1 =
    (* pattern [vV][aA][cC][uU][uU][mM] *) token env v1
  in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_error_message env x
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = (* pattern [iI][nN][tT][oO] *) token env v1 in
        let v2 = map_filename env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

let map_returning_clause (env : env) ((v1, v2, v3) : CST.returning_clause) =
  let v1 =
    (* pattern [rR][eE][tT][uU][rR][nN][iI][nN][gG] *) token env v1
  in
  let v2 = map_result_column env v2 in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_result_column env v2 in
      R.Tuple [v1; v2]
    ) v3)
  in
  R.Tuple [v1; v2; v3]

let map_create_view_stmt (env : env) ((v1, v2, v3, v4, v5, v6, v7, v8) : CST.create_view_stmt) =
  let v1 =
    (* pattern [cC][rR][eE][aA][tT][eE] *) token env v1
  in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_anon_choice_temp_716f4ac env x
      ))
    | None -> R.Option None)
  in
  let v3 = (* pattern [vV][iI][eE][wW] *) token env v3 in
  let v4 =
    (match v4 with
    | Some (v1, v2, v3) -> R.Option (Some (
        let v1 = (* pattern [iI][fF] *) token env v1 in
        let v2 = (* pattern [nN][oO][tT] *) token env v2 in
        let v3 =
          (* pattern [eE][xX][iI][sS][tT][sS] *) token env v3
        in
        R.Tuple [v1; v2; v3]
      ))
    | None -> R.Option None)
  in
  let v5 = map_name2 env v5 in
  let v6 =
    (match v6 with
    | Some x -> R.Option (Some (
        map_column_name_list env x
      ))
    | None -> R.Option None)
  in
  let v7 = (* pattern [aA][sS] *) token env v7 in
  let v8 = map_select_stmt env v8 in
  R.Tuple [v1; v2; v3; v4; v5; v6; v7; v8]

let map_column_constraint (env : env) ((v1, v2) : CST.column_constraint) =
  let v1 =
    (match v1 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 =
          (* pattern [cC][oO][nN][sS][tT][rR][aA][iI][nN][tT] *) token env v1
        in
        let v2 = map_error_message env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v2 =
    (match v2 with
    | `Prim_key_opt_choice_asc_opt_conf_clause_opt_auto (v1, v2, v3, v4, v5) -> R.Case ("Prim_key_opt_choice_asc_opt_conf_clause_opt_auto",
        let v1 =
          (* pattern [pP][rR][iI][mM][aA][rR][yY] *) token env v1
        in
        let v2 = (* pattern [kK][eE][yY] *) token env v2 in
        let v3 =
          (match v3 with
          | Some x -> R.Option (Some (
              map_anon_choice_asc_fa4fd8b env x
            ))
          | None -> R.Option None)
        in
        let v4 =
          (match v4 with
          | Some x -> R.Option (Some (
              map_conflict_clause env x
            ))
          | None -> R.Option None)
        in
        let v5 =
          (match v5 with
          | Some tok -> R.Option (Some (
              (* pattern [aA][uU][tT][oO][iI][nN][cC][rR][eE][mM][eE][nN][tT] *) token env tok
            ))
          | None -> R.Option None)
        in
        R.Tuple [v1; v2; v3; v4; v5]
      )
    | `Opt_not_null_opt_conf_clause (v1, v2, v3) -> R.Case ("Opt_not_null_opt_conf_clause",
        let v1 =
          (match v1 with
          | Some tok -> R.Option (Some (
              (* pattern [nN][oO][tT] *) token env tok
            ))
          | None -> R.Option None)
        in
        let v2 = (* pattern [nN][uU][lL][lL] *) token env v2 in
        let v3 =
          (match v3 with
          | Some x -> R.Option (Some (
              map_conflict_clause env x
            ))
          | None -> R.Option None)
        in
        R.Tuple [v1; v2; v3]
      )
    | `Unique_opt_conf_clause (v1, v2) -> R.Case ("Unique_opt_conf_clause",
        let v1 =
          (* pattern [uU][nN][iI][qQ][uU][eE] *) token env v1
        in
        let v2 =
          (match v2 with
          | Some x -> R.Option (Some (
              map_conflict_clause env x
            ))
          | None -> R.Option None)
        in
        R.Tuple [v1; v2]
      )
    | `Check_LPAR_expr_RPAR (v1, v2, v3, v4) -> R.Case ("Check_LPAR_expr_RPAR",
        let v1 = (* pattern [cC][hH][eE][cC][kK] *) token env v1 in
        let v2 = (* "(" *) token env v2 in
        let v3 = map_filename env v3 in
        let v4 = (* ")" *) token env v4 in
        R.Tuple [v1; v2; v3; v4]
      )
    | `Defa_choice_LPAR_expr_RPAR (v1, v2) -> R.Case ("Defa_choice_LPAR_expr_RPAR",
        let v1 =
          (* pattern [dD][eE][fF][aA][uU][lL][tT] *) token env v1
        in
        let v2 =
          (match v2 with
          | `LPAR_expr_RPAR (v1, v2, v3) -> R.Case ("LPAR_expr_RPAR",
              let v1 = (* "(" *) token env v1 in
              let v2 = map_filename env v2 in
              let v3 = (* ")" *) token env v3 in
              R.Tuple [v1; v2; v3]
            )
          | `Lit_value x -> R.Case ("Lit_value",
              map_literal_value env x
            )
          | `Signed_num x -> R.Case ("Signed_num",
              map_signed_number env x
            )
          )
        in
        R.Tuple [v1; v2]
      )
    | `Coll_coll_name (v1, v2) -> R.Case ("Coll_coll_name",
        let v1 =
          (* pattern [cC][oO][lL][lL][aA][tT][eE] *) token env v1
        in
        let v2 = map_collation_name env v2 in
        R.Tuple [v1; v2]
      )
    | `Fore_key_clause x -> R.Case ("Fore_key_clause",
        map_foreign_key_clause env x
      )
    | `Opt_gene_always_as_LPAR_expr_RPAR_opt_choice_stored (v1, v2, v3, v4, v5, v6) -> R.Case ("Opt_gene_always_as_LPAR_expr_RPAR_opt_choice_stored",
        let v1 =
          (match v1 with
          | Some (v1, v2) -> R.Option (Some (
              let v1 =
                (* pattern [gG][eE][nN][eE][rR][aA][tT][eE][dD] *) token env v1
              in
              let v2 =
                (* pattern [aA][lL][wW][aA][yY][sS] *) token env v2
              in
              R.Tuple [v1; v2]
            ))
          | None -> R.Option None)
        in
        let v2 = (* pattern [aA][sS] *) token env v2 in
        let v3 = (* "(" *) token env v3 in
        let v4 = map_filename env v4 in
        let v5 = (* ")" *) token env v5 in
        let v6 =
          (match v6 with
          | Some x -> R.Option (Some (
              (match x with
              | `Stored tok -> R.Case ("Stored",
                  (* pattern [sS][tT][oO][rR][eE][dD] *) token env tok
                )
              | `Virt tok -> R.Case ("Virt",
                  (* pattern [vV][iI][rR][tT][uU][aA][lL] *) token env tok
                )
              )
            ))
          | None -> R.Option None)
        in
        R.Tuple [v1; v2; v3; v4; v5; v6]
      )
    )
  in
  R.Tuple [v1; v2]

let map_create_index_stmt (env : env) ((v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12) : CST.create_index_stmt) =
  let v1 =
    (* pattern [cC][rR][eE][aA][tT][eE] *) token env v1
  in
  let v2 =
    (match v2 with
    | Some tok -> R.Option (Some (
        (* pattern [uU][nN][iI][qQ][uU][eE] *) token env tok
      ))
    | None -> R.Option None)
  in
  let v3 = (* pattern [iI][nN][dD][eE][xX] *) token env v3 in
  let v4 =
    (match v4 with
    | Some (v1, v2, v3) -> R.Option (Some (
        let v1 = (* pattern [iI][fF] *) token env v1 in
        let v2 = (* pattern [nN][oO][tT] *) token env v2 in
        let v3 =
          (* pattern [eE][xX][iI][sS][tT][sS] *) token env v3
        in
        R.Tuple [v1; v2; v3]
      ))
    | None -> R.Option None)
  in
  let v5 = map_name2 env v5 in
  let v6 = (* pattern [oO][nN] *) token env v6 in
  let v7 = map_error_message env v7 in
  let v8 = (* "(" *) token env v8 in
  let v9 = map_indexed_column env v9 in
  let v10 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_indexed_column env v2 in
      R.Tuple [v1; v2]
    ) v10)
  in
  let v11 = (* ")" *) token env v11 in
  let v12 =
    (match v12 with
    | Some x -> R.Option (Some (
        map_where_clause env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4; v5; v6; v7; v8; v9; v10; v11; v12]

let map_table_constraint (env : env) ((v1, v2) : CST.table_constraint) =
  let v1 =
    (match v1 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 =
          (* pattern [cC][oO][nN][sS][tT][rR][aA][iI][nN][tT] *) token env v1
        in
        let v2 = map_error_message env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v2 =
    (match v2 with
    | `Choice_prim_key_LPAR_inde_column_rep_COMMA_inde_column_RPAR_opt_conf_clause (v1, v2, v3, v4, v5, v6) -> R.Case ("Choice_prim_key_LPAR_inde_column_rep_COMMA_inde_column_RPAR_opt_conf_clause",
        let v1 =
          (match v1 with
          | `Prim_key (v1, v2) -> R.Case ("Prim_key",
              let v1 =
                (* pattern [pP][rR][iI][mM][aA][rR][yY] *) token env v1
              in
              let v2 = (* pattern [kK][eE][yY] *) token env v2 in
              R.Tuple [v1; v2]
            )
          | `Unique tok -> R.Case ("Unique",
              (* pattern [uU][nN][iI][qQ][uU][eE] *) token env tok
            )
          )
        in
        let v2 = (* "(" *) token env v2 in
        let v3 = map_indexed_column env v3 in
        let v4 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_indexed_column env v2 in
            R.Tuple [v1; v2]
          ) v4)
        in
        let v5 = (* ")" *) token env v5 in
        let v6 =
          (match v6 with
          | Some x -> R.Option (Some (
              map_conflict_clause env x
            ))
          | None -> R.Option None)
        in
        R.Tuple [v1; v2; v3; v4; v5; v6]
      )
    | `Check_LPAR_expr_RPAR (v1, v2, v3, v4) -> R.Case ("Check_LPAR_expr_RPAR",
        let v1 = (* pattern [cC][hH][eE][cC][kK] *) token env v1 in
        let v2 = (* "(" *) token env v2 in
        let v3 = map_filename env v3 in
        let v4 = (* ")" *) token env v4 in
        R.Tuple [v1; v2; v3; v4]
      )
    | `Fore_key_LPAR_name_rep_COMMA_name_RPAR_fore_key_clause (v1, v2, v3, v4, v5, v6, v7) -> R.Case ("Fore_key_LPAR_name_rep_COMMA_name_RPAR_fore_key_clause",
        let v1 =
          (* pattern [fF][oO][rR][eE][iI][gG][nN] *) token env v1
        in
        let v2 = (* pattern [kK][eE][yY] *) token env v2 in
        let v3 = (* "(" *) token env v3 in
        let v4 = map_error_message env v4 in
        let v5 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_error_message env v2 in
            R.Tuple [v1; v2]
          ) v5)
        in
        let v6 = (* ")" *) token env v6 in
        let v7 = map_foreign_key_clause env v7 in
        R.Tuple [v1; v2; v3; v4; v5; v6; v7]
      )
    )
  in
  R.Tuple [v1; v2]

let map_upsert_clause (env : env) ((v1, v2, v3, v4, v5) : CST.upsert_clause) =
  let v1 = (* pattern [oO][nN] *) token env v1 in
  let v2 =
    (* pattern [cC][oO][nN][fF][lL][iI][cC][tT] *) token env v2
  in
  let v3 =
    (match v3 with
    | Some (v1, v2, v3, v4, v5) -> R.Option (Some (
        let v1 = (* "(" *) token env v1 in
        let v2 = map_indexed_column env v2 in
        let v3 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_indexed_column env v2 in
            R.Tuple [v1; v2]
          ) v3)
        in
        let v4 = (* ")" *) token env v4 in
        let v5 =
          (match v5 with
          | Some x -> R.Option (Some (
              map_where_clause env x
            ))
          | None -> R.Option None)
        in
        R.Tuple [v1; v2; v3; v4; v5]
      ))
    | None -> R.Option None)
  in
  let v4 = (* pattern [dD][oO] *) token env v4 in
  let v5 =
    (match v5 with
    | `Noth tok -> R.Case ("Noth",
        (* pattern [nN][oO][tT][hH][iI][nN][gG] *) token env tok
      )
    | `Update_set_choice_name_EQ_expr_rep_COMMA_choice_name_EQ_expr_opt_where_clause (v1, v2, v3, v4, v5, v6, v7) -> R.Case ("Update_set_choice_name_EQ_expr_rep_COMMA_choice_name_EQ_expr_opt_where_clause",
        let v1 =
          (* pattern [uU][pP][dD][aA][tT][eE] *) token env v1
        in
        let v2 = (* pattern [sS][eE][tT] *) token env v2 in
        let v3 = map_anon_choice_error_mess_facbc16 env v3 in
        let v4 = (* "=" *) token env v4 in
        let v5 = map_filename env v5 in
        let v6 =
          R.List (List.map (fun (v1, v2, v3, v4) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_anon_choice_error_mess_facbc16 env v2 in
            let v3 = (* "=" *) token env v3 in
            let v4 = map_filename env v4 in
            R.Tuple [v1; v2; v3; v4]
          ) v6)
        in
        let v7 =
          (match v7 with
          | Some x -> R.Option (Some (
              map_where_clause env x
            ))
          | None -> R.Option None)
        in
        R.Tuple [v1; v2; v3; v4; v5; v6; v7]
      )
    )
  in
  R.Tuple [v1; v2; v3; v4; v5]

let map_update_stmt (env : env) ((v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14) : CST.update_stmt) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_with_clause env x
      ))
    | None -> R.Option None)
  in
  let v2 =
    (* pattern [uU][pP][dD][aA][tT][eE] *) token env v2
  in
  let v3 =
    (match v3 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = (* pattern [oO][rR] *) token env v1 in
        let v2 = map_anon_choice_abort_500a898 env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v4 = map_qualified_table_name env v4 in
  let v5 = (* pattern [sS][eE][tT] *) token env v5 in
  let v6 = map_anon_choice_error_mess_facbc16 env v6 in
  let v7 = (* "=" *) token env v7 in
  let v8 = map_filename env v8 in
  let v9 =
    R.List (List.map (fun (v1, v2, v3, v4) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_anon_choice_error_mess_facbc16 env v2 in
      let v3 = (* "=" *) token env v3 in
      let v4 = map_filename env v4 in
      R.Tuple [v1; v2; v3; v4]
    ) v9)
  in
  let v10 =
    (match v10 with
    | Some x -> R.Option (Some (
        map_from_clause env x
      ))
    | None -> R.Option None)
  in
  let v11 =
    (match v11 with
    | Some x -> R.Option (Some (
        map_where_clause env x
      ))
    | None -> R.Option None)
  in
  let v12 =
    (match v12 with
    | Some x -> R.Option (Some (
        map_returning_clause env x
      ))
    | None -> R.Option None)
  in
  let v13 =
    (match v13 with
    | Some x -> R.Option (Some (
        map_order_by_clause env x
      ))
    | None -> R.Option None)
  in
  let v14 =
    (match v14 with
    | Some x -> R.Option (Some (
        map_limit_clause env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4; v5; v6; v7; v8; v9; v10; v11; v12; v13; v14]

let map_delete_stmt (env : env) ((v1, v2, v3, v4, v5, v6, v7, v8) : CST.delete_stmt) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_with_clause env x
      ))
    | None -> R.Option None)
  in
  let v2 =
    (* pattern [dD][eE][lL][eE][tT][eE] *) token env v2
  in
  let v3 = (* pattern [fF][rR][oO][mM] *) token env v3 in
  let v4 = map_qualified_table_name env v4 in
  let v5 =
    (match v5 with
    | Some x -> R.Option (Some (
        map_where_clause env x
      ))
    | None -> R.Option None)
  in
  let v6 =
    (match v6 with
    | Some x -> R.Option (Some (
        map_returning_clause env x
      ))
    | None -> R.Option None)
  in
  let v7 =
    (match v7 with
    | Some x -> R.Option (Some (
        map_order_by_clause env x
      ))
    | None -> R.Option None)
  in
  let v8 =
    (match v8 with
    | Some x -> R.Option (Some (
        map_limit_clause env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4; v5; v6; v7; v8]

let map_column_def (env : env) ((v1, v2, v3) : CST.column_def) =
  let v1 = map_error_message env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_type_name env x
      ))
    | None -> R.Option None)
  in
  let v3 = R.List (List.map (map_column_constraint env) v3) in
  R.Tuple [v1; v2; v3]

let map_insert_stmt (env : env) ((v1, v2, v3, v4, v5, v6, v7, v8) : CST.insert_stmt) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_with_clause env x
      ))
    | None -> R.Option None)
  in
  let v2 =
    (match v2 with
    | `Repl tok -> R.Case ("Repl",
        (* pattern [rR][eE][pP][lL][aA][cC][eE] *) token env tok
      )
    | `Insert_opt_or_choice_abort (v1, v2) -> R.Case ("Insert_opt_or_choice_abort",
        let v1 =
          (* pattern [iI][nN][sS][eE][rR][tT] *) token env v1
        in
        let v2 =
          (match v2 with
          | Some (v1, v2) -> R.Option (Some (
              let v1 = (* pattern [oO][rR] *) token env v1 in
              let v2 = map_anon_choice_abort_500a898 env v2 in
              R.Tuple [v1; v2]
            ))
          | None -> R.Option None)
        in
        R.Tuple [v1; v2]
      )
    )
  in
  let v3 = (* pattern [iI][nN][tT][oO] *) token env v3 in
  let v4 = map_name2 env v4 in
  let v5 =
    (match v5 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = (* pattern [aA][sS] *) token env v1 in
        let v2 = map_error_message env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v6 =
    (match v6 with
    | Some x -> R.Option (Some (
        map_column_name_list env x
      ))
    | None -> R.Option None)
  in
  let v7 =
    (match v7 with
    | `Values_LPAR_expr_rep_COMMA_expr_RPAR_rep_COMMA_LPAR_expr_rep_COMMA_expr_RPAR_opt_upsert_clause (v1, v2, v3, v4, v5, v6, v7) -> R.Case ("Values_LPAR_expr_rep_COMMA_expr_RPAR_rep_COMMA_LPAR_expr_rep_COMMA_expr_RPAR_opt_upsert_clause",
        let v1 =
          (* pattern [vV][aA][lL][uU][eE][sS] *) token env v1
        in
        let v2 = (* "(" *) token env v2 in
        let v3 = map_filename env v3 in
        let v4 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_filename env v2 in
            R.Tuple [v1; v2]
          ) v4)
        in
        let v5 = (* ")" *) token env v5 in
        let v6 =
          R.List (List.map (map_anon_COMMA_LPAR_file_rep_COMMA_file_RPAR_a378f47 env) v6)
        in
        let v7 =
          (match v7 with
          | Some x -> R.Option (Some (
              map_upsert_clause env x
            ))
          | None -> R.Option None)
        in
        R.Tuple [v1; v2; v3; v4; v5; v6; v7]
      )
    | `Select_stmt_opt_upsert_clause (v1, v2) -> R.Case ("Select_stmt_opt_upsert_clause",
        let v1 = map_select_stmt env v1 in
        let v2 =
          (match v2 with
          | Some x -> R.Option (Some (
              map_upsert_clause env x
            ))
          | None -> R.Option None)
        in
        R.Tuple [v1; v2]
      )
    | `Defa_values (v1, v2) -> R.Case ("Defa_values",
        let v1 =
          (* pattern [dD][eE][fF][aA][uU][lL][tT] *) token env v1
        in
        let v2 =
          (* pattern [vV][aA][lL][uU][eE][sS] *) token env v2
        in
        R.Tuple [v1; v2]
      )
    )
  in
  let v8 =
    (match v8 with
    | Some x -> R.Option (Some (
        map_returning_clause env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4; v5; v6; v7; v8]

let map_create_virtual_table_stmt (env : env) ((v1, v2, v3, v4, v5, v6, v7, v8) : CST.create_virtual_table_stmt) =
  let v1 =
    (* pattern [cC][rR][eE][aA][tT][eE] *) token env v1
  in
  let v2 =
    (* pattern [vV][iI][rR][tT][uU][aA][lL] *) token env v2
  in
  let v3 = (* pattern [tT][aA][bB][lL][eE] *) token env v3 in
  let v4 =
    (match v4 with
    | Some (v1, v2, v3) -> R.Option (Some (
        let v1 = (* pattern [iI][fF] *) token env v1 in
        let v2 = (* pattern [nN][oO][tT] *) token env v2 in
        let v3 =
          (* pattern [eE][xX][iI][sS][tT][sS] *) token env v3
        in
        R.Tuple [v1; v2; v3]
      ))
    | None -> R.Option None)
  in
  let v5 = map_name2 env v5 in
  let v6 = (* pattern [uU][sS][iI][nN][gG] *) token env v6 in
  let v7 = map_error_message env v7 in
  let v8 =
    (match v8 with
    | Some (v1, v2, v3, v4, v5) -> R.Option (Some (
        let v1 = (* "(" *) token env v1 in
        let v2 = map_column_def env v2 in
        let v3 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_column_def env v2 in
            R.Tuple [v1; v2]
          ) v3)
        in
        let v4 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_table_constraint env v2 in
            R.Tuple [v1; v2]
          ) v4)
        in
        let v5 = (* ")" *) token env v5 in
        R.Tuple [v1; v2; v3; v4; v5]
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4; v5; v6; v7; v8]

let map_create_table_stmt (env : env) ((v1, v2, v3, v4, v5, v6) : CST.create_table_stmt) =
  let v1 =
    (* pattern [cC][rR][eE][aA][tT][eE] *) token env v1
  in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_anon_choice_temp_716f4ac env x
      ))
    | None -> R.Option None)
  in
  let v3 = (* pattern [tT][aA][bB][lL][eE] *) token env v3 in
  let v4 =
    (match v4 with
    | Some (v1, v2, v3) -> R.Option (Some (
        let v1 = (* pattern [iI][fF] *) token env v1 in
        let v2 = (* pattern [nN][oO][tT] *) token env v2 in
        let v3 =
          (* pattern [eE][xX][iI][sS][tT][sS] *) token env v3
        in
        R.Tuple [v1; v2; v3]
      ))
    | None -> R.Option None)
  in
  let v5 = map_name2 env v5 in
  let v6 =
    (match v6 with
    | `As_select_stmt (v1, v2) -> R.Case ("As_select_stmt",
        let v1 = (* pattern [aA][sS] *) token env v1 in
        let v2 = map_select_stmt env v2 in
        R.Tuple [v1; v2]
      )
    | `LPAR_column_def_rep_COMMA_column_def_rep_COMMA_table_cons_RPAR_opt_with_rowid (v1, v2, v3, v4, v5, v6) -> R.Case ("LPAR_column_def_rep_COMMA_column_def_rep_COMMA_table_cons_RPAR_opt_with_rowid",
        let v1 = (* "(" *) token env v1 in
        let v2 = map_column_def env v2 in
        let v3 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_column_def env v2 in
            R.Tuple [v1; v2]
          ) v3)
        in
        let v4 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_table_constraint env v2 in
            R.Tuple [v1; v2]
          ) v4)
        in
        let v5 = (* ")" *) token env v5 in
        let v6 =
          (match v6 with
          | Some (v1, v2) -> R.Option (Some (
              let v1 =
                (* pattern [wW][iI][tT][hH][oO][uU][tT] *) token env v1
              in
              let v2 = (* pattern [rR][oO][wW][iI][dD] *) token env v2 in
              R.Tuple [v1; v2]
            ))
          | None -> R.Option None)
        in
        R.Tuple [v1; v2; v3; v4; v5; v6]
      )
    )
  in
  R.Tuple [v1; v2; v3; v4; v5; v6]

let map_alter_table_stmt (env : env) ((v1, v2, v3, v4) : CST.alter_table_stmt) =
  let v1 = (* pattern [aA][lL][tT][eE][rR] *) token env v1 in
  let v2 = (* pattern [tT][aA][bB][lL][eE] *) token env v2 in
  let v3 = map_name2 env v3 in
  let v4 =
    (match v4 with
    | `Rename_to_name (v1, v2, v3) -> R.Case ("Rename_to_name",
        let v1 =
          (* pattern [rR][eE][nN][aA][mM][eE] *) token env v1
        in
        let v2 = (* pattern [tT][oO] *) token env v2 in
        let v3 = map_error_message env v3 in
        R.Tuple [v1; v2; v3]
      )
    | `Rename_opt_column_name_to_name (v1, v2, v3, v4, v5) -> R.Case ("Rename_opt_column_name_to_name",
        let v1 =
          (* pattern [rR][eE][nN][aA][mM][eE] *) token env v1
        in
        let v2 =
          (match v2 with
          | Some tok -> R.Option (Some (
              (* pattern [cC][oO][lL][uU][mM][nN] *) token env tok
            ))
          | None -> R.Option None)
        in
        let v3 = map_error_message env v3 in
        let v4 = (* pattern [tT][oO] *) token env v4 in
        let v5 = map_error_message env v5 in
        R.Tuple [v1; v2; v3; v4; v5]
      )
    | `Add_opt_column_column_def (v1, v2, v3) -> R.Case ("Add_opt_column_column_def",
        let v1 = (* pattern [aA][dD][dD] *) token env v1 in
        let v2 =
          (match v2 with
          | Some tok -> R.Option (Some (
              (* pattern [cC][oO][lL][uU][mM][nN] *) token env tok
            ))
          | None -> R.Option None)
        in
        let v3 = map_column_def env v3 in
        R.Tuple [v1; v2; v3]
      )
    | `Drop_opt_column_name (v1, v2, v3) -> R.Case ("Drop_opt_column_name",
        let v1 = (* pattern [dD][rR][oO][pP] *) token env v1 in
        let v2 =
          (match v2 with
          | Some tok -> R.Option (Some (
              (* pattern [cC][oO][lL][uU][mM][nN] *) token env tok
            ))
          | None -> R.Option None)
        in
        let v3 = map_error_message env v3 in
        R.Tuple [v1; v2; v3]
      )
    )
  in
  R.Tuple [v1; v2; v3; v4]

let map_create_trigger_stmt (env : env) ((v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13) : CST.create_trigger_stmt) =
  let v1 =
    (* pattern [cC][rR][eE][aA][tT][eE] *) token env v1
  in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_anon_choice_temp_716f4ac env x
      ))
    | None -> R.Option None)
  in
  let v3 =
    (* pattern [tT][rR][iI][gG][gG][eE][rR] *) token env v3
  in
  let v4 =
    (match v4 with
    | Some (v1, v2, v3) -> R.Option (Some (
        let v1 = (* pattern [iI][fF] *) token env v1 in
        let v2 = (* pattern [nN][oO][tT] *) token env v2 in
        let v3 =
          (* pattern [eE][xX][iI][sS][tT][sS] *) token env v3
        in
        R.Tuple [v1; v2; v3]
      ))
    | None -> R.Option None)
  in
  let v5 = map_name2 env v5 in
  let v6 =
    (match v6 with
    | Some x -> R.Option (Some (
        (match x with
        | `Before tok -> R.Case ("Before",
            (* pattern [bB][eE][fF][oO][rR][eE] *) token env tok
          )
        | `After tok -> R.Case ("After",
            (* pattern [aA][fF][tT][eE][rR] *) token env tok
          )
        | `Inst_of (v1, v2) -> R.Case ("Inst_of",
            let v1 =
              (* pattern [iI][nN][sS][tT][eE][aA][dD] *) token env v1
            in
            let v2 = (* pattern [oO][fF] *) token env v2 in
            R.Tuple [v1; v2]
          )
        )
      ))
    | None -> R.Option None)
  in
  let v7 =
    (match v7 with
    | `Delete tok -> R.Case ("Delete",
        (* pattern [dD][eE][lL][eE][tT][eE] *) token env tok
      )
    | `Insert tok -> R.Case ("Insert",
        (* pattern [iI][nN][sS][eE][rR][tT] *) token env tok
      )
    | `Update_opt_of_name_rep_COMMA_name (v1, v2) -> R.Case ("Update_opt_of_name_rep_COMMA_name",
        let v1 =
          (* pattern [uU][pP][dD][aA][tT][eE] *) token env v1
        in
        let v2 =
          (match v2 with
          | Some (v1, v2, v3) -> R.Option (Some (
              let v1 = (* pattern [oO][fF] *) token env v1 in
              let v2 = map_error_message env v2 in
              let v3 =
                R.List (List.map (fun (v1, v2) ->
                  let v1 = (* "," *) token env v1 in
                  let v2 = map_error_message env v2 in
                  R.Tuple [v1; v2]
                ) v3)
              in
              R.Tuple [v1; v2; v3]
            ))
          | None -> R.Option None)
        in
        R.Tuple [v1; v2]
      )
    )
  in
  let v8 = (* pattern [oO][nN] *) token env v8 in
  let v9 = map_error_message env v9 in
  let v10 =
    (match v10 with
    | Some x -> R.Option (Some (
        (match x with
        | `For_each_row_opt_when_expr (v1, v2, v3, v4) -> R.Case ("For_each_row_opt_when_expr",
            let v1 = (* pattern [fF][oO][rR] *) token env v1 in
            let v2 = (* pattern [eE][aA][cC][hH] *) token env v2 in
            let v3 = (* pattern [rR][oO][wW] *) token env v3 in
            let v4 =
              (match v4 with
              | Some (v1, v2) -> R.Option (Some (
                  let v1 = (* pattern [wW][hH][eE][nN] *) token env v1 in
                  let v2 = map_filename env v2 in
                  R.Tuple [v1; v2]
                ))
              | None -> R.Option None)
            in
            R.Tuple [v1; v2; v3; v4]
          )
        | `When_expr (v1, v2) -> R.Case ("When_expr",
            let v1 = (* pattern [wW][hH][eE][nN] *) token env v1 in
            let v2 = map_filename env v2 in
            R.Tuple [v1; v2]
          )
        )
      ))
    | None -> R.Option None)
  in
  let v11 =
    (* pattern [bB][eE][gG][iI][nN] *) token env v11
  in
  let v12 =
    R.List (List.map (fun (v1, v2) ->
      let v1 =
        (match v1 with
        | `Update_stmt x -> R.Case ("Update_stmt",
            map_update_stmt env x
          )
        | `Insert_stmt x -> R.Case ("Insert_stmt",
            map_insert_stmt env x
          )
        | `Delete_stmt x -> R.Case ("Delete_stmt",
            map_delete_stmt env x
          )
        | `Select_stmt x -> R.Case ("Select_stmt",
            map_select_stmt env x
          )
        )
      in
      let v2 = (* ";" *) token env v2 in
      R.Tuple [v1; v2]
    ) v12)
  in
  let v13 = (* pattern [eE][nN][dD] *) token env v13 in
  R.Tuple [v1; v2; v3; v4; v5; v6; v7; v8; v9; v10; v11; v12; v13]

let map_sql_stmt (env : env) ((v1, v2) : CST.sql_stmt) =
  let v1 =
    (match v1 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 =
          (* pattern [eE][xX][pP][lL][aA][iI][nN] *) token env v1
        in
        let v2 =
          (match v2 with
          | Some (v1, v2) -> R.Option (Some (
              let v1 = (* pattern [qQ][uU][eE][rR][yY] *) token env v1 in
              let v2 = (* pattern [pP][lL][aA][nN] *) token env v2 in
              R.Tuple [v1; v2]
            ))
          | None -> R.Option None)
        in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v2 =
    (match v2 with
    | `Alter_table_stmt x -> R.Case ("Alter_table_stmt",
        map_alter_table_stmt env x
      )
    | `Anal_stmt x -> R.Case ("Anal_stmt",
        map_analyze_stmt env x
      )
    | `Attach_stmt x -> R.Case ("Attach_stmt",
        map_attach_stmt env x
      )
    | `Begin_stmt x -> R.Case ("Begin_stmt",
        map_begin_stmt env x
      )
    | `Commit_stmt x -> R.Case ("Commit_stmt",
        map_commit_stmt env x
      )
    | `Create_index_stmt x -> R.Case ("Create_index_stmt",
        map_create_index_stmt env x
      )
    | `Create_table_stmt x -> R.Case ("Create_table_stmt",
        map_create_table_stmt env x
      )
    | `Create_trig_stmt x -> R.Case ("Create_trig_stmt",
        map_create_trigger_stmt env x
      )
    | `Create_view_stmt x -> R.Case ("Create_view_stmt",
        map_create_view_stmt env x
      )
    | `Create_virt_table_stmt x -> R.Case ("Create_virt_table_stmt",
        map_create_virtual_table_stmt env x
      )
    | `Delete_stmt x -> R.Case ("Delete_stmt",
        map_delete_stmt env x
      )
    | `Detach_stmt x -> R.Case ("Detach_stmt",
        map_detach_stmt env x
      )
    | `Drop_index_stmt x -> R.Case ("Drop_index_stmt",
        map_drop_index_stmt env x
      )
    | `Drop_table_stmt x -> R.Case ("Drop_table_stmt",
        map_drop_table_stmt env x
      )
    | `Drop_trig_stmt x -> R.Case ("Drop_trig_stmt",
        map_drop_trigger_stmt env x
      )
    | `Drop_view_stmt x -> R.Case ("Drop_view_stmt",
        map_drop_view_stmt env x
      )
    | `Insert_stmt x -> R.Case ("Insert_stmt",
        map_insert_stmt env x
      )
    | `Pragma_stmt x -> R.Case ("Pragma_stmt",
        map_pragma_stmt env x
      )
    | `Rein_stmt x -> R.Case ("Rein_stmt",
        map_reindex_stmt env x
      )
    | `Rele_stmt x -> R.Case ("Rele_stmt",
        map_release_stmt env x
      )
    | `Roll_stmt x -> R.Case ("Roll_stmt",
        map_rollback_stmt env x
      )
    | `Save_stmt x -> R.Case ("Save_stmt",
        map_savepoint_stmt env x
      )
    | `Select_stmt x -> R.Case ("Select_stmt",
        map_select_stmt env x
      )
    | `Update_stmt x -> R.Case ("Update_stmt",
        map_update_stmt env x
      )
    | `Vacuum_stmt x -> R.Case ("Vacuum_stmt",
        map_vacuum_stmt env x
      )
    )
  in
  R.Tuple [v1; v2]

let map_sql_stmt_list (env : env) ((v1, v2) : CST.sql_stmt_list) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_sql_stmt env x
      ))
    | None -> R.Option None)
  in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* ";" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_sql_stmt env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2]
    ) v2)
  in
  R.Tuple [v1; v2]

let map_whitespace (env : env) (tok : CST.whitespace) =
  (* pattern [ \t\n\f\r]+ *) token env tok

let map_comment (env : env) (x : CST.comment) =
  (match x with
  | `DASHDASH_pat_4fd4a56 (v1, v2) -> R.Case ("DASHDASH_pat_4fd4a56",
      let v1 = (* "--" *) token env v1 in
      let v2 = map_pat_4fd4a56 env v2 in
      R.Tuple [v1; v2]
    )
  | `SLASHSTAR_pat_05bf793_SLASH (v1, v2, v3) -> R.Case ("SLASHSTAR_pat_05bf793_SLASH",
      let v1 = (* "/*" *) token env v1 in
      let v2 = map_pat_05bf793 env v2 in
      let v3 = (* "/" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

let dump_tree root =
  map_sql_stmt_list () root
  |> Tree_sitter_run.Raw_tree.to_channel stdout

let map_extra (env : env) (x : CST.extra) =
  match x with
  | Whitespace (_loc, x) -> ("whitespace", "whitespace", map_whitespace env x)
  | Comment (_loc, x) -> ("comment", "comment", map_comment env x)

let dump_extras (extras : CST.extras) =
  List.iter (fun extra ->
    let ts_rule_name, ocaml_type_name, raw_tree = map_extra () extra in
    let details =
      if ocaml_type_name <> ts_rule_name then
        Printf.sprintf " (OCaml type '%s')" ocaml_type_name
      else
        ""
    in
    Printf.printf "%s%s:\n" ts_rule_name details;
    Tree_sitter_run.Raw_tree.to_channel stdout raw_tree
  ) extras
