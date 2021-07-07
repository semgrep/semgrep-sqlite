(**
   Boilerplate to be used as a template when mapping the sqlite CST
   to another type of tree.
*)

(* Disable warnings against unused variables *)
[@@@warning "-26-27"]

(* Disable warning against unused 'rec' *)
[@@@warning "-39"]

type env = unit

let token (env : env) (_tok : Tree_sitter_run.Token.t) =
  failwith "not implemented"

let blank (env : env) () =
  failwith "not implemented"

let todo (env : env) _ =
   failwith "not implemented"

let map_rowid (env : env) (tok : CST.rowid) =
  token env tok (* pattern [rR][oO][wW][iI][dD] *)

let map_escape (env : env) (tok : CST.escape) =
  token env tok (* pattern [eE][sS][cC][aA][pP][eE] *)

let map_range (env : env) (tok : CST.range) =
  token env tok (* pattern [rR][aA][nN][gG][eE] *)

let map_immediate (env : env) (tok : CST.immediate) =
  token env tok (* pattern [iI][mM][mM][eE][dD][iI][aA][tT][eE] *)

let map_natural (env : env) (tok : CST.natural) =
  token env tok (* pattern [nN][aA][tT][uU][rR][aA][lL] *)

let map_before (env : env) (tok : CST.before) =
  token env tok (* pattern [bB][eE][fF][oO][rR][eE] *)

let map_create (env : env) (tok : CST.create) =
  token env tok (* pattern [cC][rR][eE][aA][tT][eE] *)

let map_all (env : env) (tok : CST.all) =
  token env tok (* pattern [aA][lL][lL] *)

let map_to_ (env : env) (tok : CST.to_) =
  token env tok (* pattern [tT][oO] *)

let map_deferrable (env : env) (tok : CST.deferrable) =
  token env tok (* pattern [dD][eE][fF][eE][rR][rR][aA][bB][lL][eE] *)

let map_ignore (env : env) (tok : CST.ignore) =
  token env tok (* pattern [iI][gG][nN][oO][rR][eE] *)

let map_if_ (env : env) (tok : CST.if_) =
  token env tok (* pattern [iI][fF] *)

let map_index (env : env) (tok : CST.index) =
  token env tok (* pattern [iI][nN][dD][eE][xX] *)

let map_union (env : env) (tok : CST.union) =
  token env tok (* pattern [uU][nN][iI][oO][nN] *)

let map_insert (env : env) (tok : CST.insert) =
  token env tok (* pattern [iI][nN][sS][eE][rR][tT] *)

let map_over (env : env) (tok : CST.over) =
  token env tok (* pattern [oO][vV][eE][rR] *)

let map_exclude (env : env) (tok : CST.exclude) =
  token env tok (* pattern [eE][xX][cC][lL][uU][dD][eE] *)

let map_nothing (env : env) (tok : CST.nothing) =
  token env tok (* pattern [nN][oO][tT][hH][iI][nN][gG] *)

let map_first (env : env) (tok : CST.first) =
  token env tok (* pattern [fF][iI][rR][sS][tT] *)

let map_with_ (env : env) (tok : CST.with_) =
  token env tok (* pattern [wW][iI][tT][hH] *)

let map_current (env : env) (tok : CST.current) =
  token env tok (* pattern [cC][uU][rR][rR][eE][nN][tT] *)

let map_when_ (env : env) (tok : CST.when_) =
  token env tok (* pattern [wW][hH][eE][nN] *)

let map_raise (env : env) (tok : CST.raise) =
  token env tok (* pattern [rR][aA][iI][sS][eE] *)

let map_by (env : env) (tok : CST.by) =
  token env tok (* pattern [bB][yY] *)

let map_last (env : env) (tok : CST.last) =
  token env tok (* pattern [lL][aA][sS][tT] *)

let map_anon_choice_PLUS_da42005 (env : env) (x : CST.anon_choice_PLUS_da42005) =
  (match x with
  | `PLUS tok -> token env tok (* "+" *)
  | `DASH tok -> token env tok (* "-" *)
  )

let map_or_ (env : env) (tok : CST.or_) =
  token env tok (* pattern [oO][rR] *)

let map_virtual_ (env : env) (tok : CST.virtual_) =
  token env tok (* pattern [vV][iI][rR][tT][uU][aA][lL] *)

let map_references (env : env) (tok : CST.references) =
  token env tok (* pattern [rR][eE][fF][eE][rR][eE][nN][cC][eE][sS] *)

let map_distinct (env : env) (tok : CST.distinct) =
  token env tok (* pattern [dD][iI][sS][tT][iI][nN][cC][tT] *)

let map_pat_ae33c7c (env : env) (tok : CST.pat_ae33c7c) =
  token env tok (* pattern [^*]*\*+([^\/*][^*]*\*+)* *)

let map_unbounded (env : env) (tok : CST.unbounded) =
  token env tok (* pattern [uU][nN][bB][oO][uU][nN][dD][eE][dD] *)

let map_analyze (env : env) (tok : CST.analyze) =
  token env tok (* pattern [aA][nN][aA][lL][yY][zZ][eE] *)

let map_preceding (env : env) (tok : CST.preceding) =
  token env tok (* pattern [pP][rR][eE][cC][eE][dD][iI][nN][gG] *)

let map_transaction (env : env) (tok : CST.transaction) =
  token env tok (* pattern [tT][rR][aA][nN][sS][aA][cC][tT][iI][oO][nN] *)

let map_true_ (env : env) (tok : CST.true_) =
  token env tok (* pattern [tT][rR][uU][eE] *)

let map_generated (env : env) (tok : CST.generated) =
  token env tok (* pattern [gG][eE][nN][eE][rR][aA][tT][eE][dD] *)

let map_end_ (env : env) (tok : CST.end_) =
  token env tok (* pattern [eE][nN][dD] *)

let map_limit (env : env) (tok : CST.limit) =
  token env tok (* pattern [lL][iI][mM][iI][tT] *)

let map_join (env : env) (tok : CST.join) =
  token env tok (* pattern [jJ][oO][iI][nN] *)

let map_on (env : env) (tok : CST.on) =
  token env tok (* pattern [oO][nN] *)

let map_action (env : env) (tok : CST.action) =
  token env tok (* pattern [aA][cC][tT][iI][oO][nN] *)

let map_except (env : env) (tok : CST.except) =
  token env tok (* pattern [eE][xX][cC][eE][pP][tT] *)

let map_else_ (env : env) (tok : CST.else_) =
  token env tok (* pattern [eE][lL][sS][eE] *)

let map_do_ (env : env) (tok : CST.do_) =
  token env tok (* pattern [dD][oO] *)

let map_after (env : env) (tok : CST.after) =
  token env tok (* pattern [aA][fF][tT][eE][rR] *)

let map_notnull (env : env) (tok : CST.notnull) =
  token env tok (* pattern [nN][oO][tT][nN][uU][lL][lL] *)

let map_autoincrement (env : env) (tok : CST.autoincrement) =
  token env tok (* pattern [aA][uU][tT][oO][iI][nN][cC][rR][eE][mM][eE][nN][tT] *)

let map_asc (env : env) (tok : CST.asc) =
  token env tok (* pattern [aA][sS][cC] *)

let map_from (env : env) (tok : CST.from) =
  token env tok (* pattern [fF][rR][oO][mM] *)

let map_initially (env : env) (tok : CST.initially) =
  token env tok (* pattern [iI][nN][iI][tT][iI][aA][lL][lL][yY] *)

let map_recursive (env : env) (tok : CST.recursive) =
  token env tok (* pattern [rR][eE][cC][uU][rR][sS][iI][vV][eE] *)

let map_collate (env : env) (tok : CST.collate) =
  token env tok (* pattern [cC][oO][lL][lL][aA][tT][eE] *)

let map_each (env : env) (tok : CST.each) =
  token env tok (* pattern [eE][aA][cC][hH] *)

let map_no (env : env) (tok : CST.no) =
  token env tok (* pattern [nN][oO] *)

let map_temporary (env : env) (tok : CST.temporary) =
  token env tok (* pattern [tT][eE][mM][pP][oO][rR][aA][rR][yY] *)

let map_current_date (env : env) (tok : CST.current_date) =
  token env tok (* pattern [cC][uU][rR][rR][eE][nN][tT][__][dD][aA][tT][eE] *)

let map_plan (env : env) (tok : CST.plan) =
  token env tok (* pattern [pP][lL][aA][nN] *)

let map_left (env : env) (tok : CST.left) =
  token env tok (* pattern [lL][eE][fF][tT] *)

let map_row (env : env) (tok : CST.row) =
  token env tok (* pattern [rR][oO][wW] *)

let map_explain (env : env) (tok : CST.explain) =
  token env tok (* pattern [eE][xX][pP][lL][aA][iI][nN] *)

let map_having (env : env) (tok : CST.having) =
  token env tok (* pattern [hH][aA][vV][iI][nN][gG] *)

let map_begin_ (env : env) (tok : CST.begin_) =
  token env tok (* pattern [bB][eE][gG][iI][nN] *)

let map_group (env : env) (tok : CST.group) =
  token env tok (* pattern [gG][rR][oO][uU][pP] *)

let map_in_ (env : env) (tok : CST.in_) =
  token env tok (* pattern [iI][nN] *)

let map_values (env : env) (tok : CST.values) =
  token env tok (* pattern [vV][aA][lL][uU][eE][sS] *)

let map_trigger (env : env) (tok : CST.trigger) =
  token env tok (* pattern [tT][rR][iI][gG][gG][eE][rR] *)

let map_null (env : env) (tok : CST.null) =
  token env tok (* pattern [nN][uU][lL][lL] *)

let map_deferred (env : env) (tok : CST.deferred) =
  token env tok (* pattern [dD][eE][fF][eE][rR][rR][eE][dD] *)

let map_window (env : env) (tok : CST.window) =
  token env tok (* pattern [wW][iI][nN][dD][oO][wW] *)

let map_restrict (env : env) (tok : CST.restrict) =
  token env tok (* pattern [rR][eE][sS][tT][rR][iI][cC][tT] *)

let map_select (env : env) (tok : CST.select) =
  token env tok (* pattern [sS][eE][lL][eE][cC][tT] *)

let map_indexed (env : env) (tok : CST.indexed) =
  token env tok (* pattern [iI][nN][dD][eE][xX][eE][dD] *)

let map_partition (env : env) (tok : CST.partition) =
  token env tok (* pattern [pP][aA][rR][tT][iI][tT][iI][oO][nN] *)

let map_using (env : env) (tok : CST.using) =
  token env tok (* pattern [uU][sS][iI][nN][gG] *)

let map_others (env : env) (tok : CST.others) =
  token env tok (* pattern [oO][tT][hH][eE][rR][sS] *)

let map_cascade (env : env) (tok : CST.cascade) =
  token env tok (* pattern [cC][aA][sS][cC][aA][dD][eE] *)

let map_following (env : env) (tok : CST.following) =
  token env tok (* pattern [fF][oO][lL][lL][oO][wW][iI][nN][gG] *)

let map_release (env : env) (tok : CST.release) =
  token env tok (* pattern [rR][eE][lL][eE][aA][sS][eE] *)

let map_ties (env : env) (tok : CST.ties) =
  token env tok (* pattern [tT][iI][eE][sS] *)

let map_detach (env : env) (tok : CST.detach) =
  token env tok (* pattern [dD][eE][tT][aA][cC][hH] *)

let map_cross (env : env) (tok : CST.cross) =
  token env tok (* pattern [cC][rR][oO][sS][sS] *)

let map_isnull (env : env) (tok : CST.isnull) =
  token env tok (* pattern [iI][sS][nN][uU][lL][lL] *)

let map_not (env : env) (tok : CST.not) =
  token env tok (* pattern [nN][oO][tT] *)

let map_inner (env : env) (tok : CST.inner) =
  token env tok (* pattern [iI][nN][nN][eE][rR] *)

let map_update (env : env) (tok : CST.update) =
  token env tok (* pattern [uU][pP][dD][aA][tT][eE] *)

let map_database (env : env) (tok : CST.database) =
  token env tok (* pattern [dD][aA][tT][aA][bB][aA][sS][eE] *)

let map_pat_73398bc (env : env) (tok : CST.pat_73398bc) =
  token env tok (* pattern "(\"\"|[^\"])*" *)

let map_current_timestamp (env : env) (tok : CST.current_timestamp) =
  token env tok (* pattern [cC][uU][rR][rR][eE][nN][tT][__][tT][iI][mM][eE][sS][tT][aA][mM][pP] *)

let map_set (env : env) (tok : CST.set) =
  token env tok (* pattern [sS][eE][tT] *)

let map_primary (env : env) (tok : CST.primary) =
  token env tok (* pattern [pP][rR][iI][mM][aA][rR][yY] *)

let map_temp (env : env) (tok : CST.temp) =
  token env tok (* pattern [tT][eE][mM][pP] *)

let map_pat_f79575e (env : env) (tok : CST.pat_f79575e) =
  token env tok (* pattern "(''|[^'])*" *)

let map_order (env : env) (tok : CST.order) =
  token env tok (* pattern [oO][rR][dD][eE][rR] *)

let map_outer (env : env) (tok : CST.outer) =
  token env tok (* pattern [oO][uU][tT][eE][rR] *)

let map_is (env : env) (tok : CST.is) =
  token env tok (* pattern [iI][sS] *)

let map_numeric_literal (env : env) (tok : CST.numeric_literal) =
  token env tok (* numeric_literal *)

let map_pat_f154b4a (env : env) (tok : CST.pat_f154b4a) =
  token env tok (* pattern [^\]]* *)

let map_pat_93c883a (env : env) (tok : CST.pat_93c883a) =
  token env tok (* pattern [$_0-9a-zA-Z\x80-\xFF]+ *)

let map_pragma (env : env) (tok : CST.pragma) =
  token env tok (* pattern [pP][rR][aA][gG][mM][aA] *)

let map_table (env : env) (tok : CST.table) =
  token env tok (* pattern [tT][aA][bB][lL][eE] *)

let map_column (env : env) (tok : CST.column) =
  token env tok (* pattern [cC][oO][lL][uU][mM][nN] *)

let map_default (env : env) (tok : CST.default) =
  token env tok (* pattern [dD][eE][fF][aA][uU][lL][tT] *)

let map_vacuum (env : env) (tok : CST.vacuum) =
  token env tok (* pattern [vV][aA][cC][uU][uU][mM] *)

let map_false_ (env : env) (tok : CST.false_) =
  token env tok (* pattern [fF][aA][lL][sS][eE] *)

let map_instead (env : env) (tok : CST.instead) =
  token env tok (* pattern [iI][nN][sS][tT][eE][aA][dD] *)

let map_of_ (env : env) (tok : CST.of_) =
  token env tok (* pattern [oO][fF] *)

let map_current_time (env : env) (tok : CST.current_time) =
  token env tok (* pattern [cC][uU][rR][rR][eE][nN][tT][__][tT][iI][mM][eE] *)

let map_exists (env : env) (tok : CST.exists) =
  token env tok (* pattern [eE][xX][iI][sS][tT][sS] *)

let map_reindex (env : env) (tok : CST.reindex) =
  token env tok (* pattern [rR][eE][iI][nN][dD][eE][xX] *)

let map_rows (env : env) (tok : CST.rows) =
  token env tok (* pattern [rR][oO][wW][sS] *)

let map_cast (env : env) (tok : CST.cast) =
  token env tok (* pattern [cC][aA][sS][tT] *)

let map_like (env : env) (tok : CST.like) =
  token env tok (* pattern [lL][iI][kK][eE] *)

let map_constraint_ (env : env) (tok : CST.constraint_) =
  token env tok (* pattern [cC][oO][nN][sS][tT][rR][aA][iI][nN][tT] *)

let map_regexp (env : env) (tok : CST.regexp) =
  token env tok (* pattern [rR][eE][gG][eE][xX][pP] *)

let map_materialized (env : env) (tok : CST.materialized) =
  token env tok (* pattern [mM][aA][tT][eE][rR][iI][aA][lL][iI][zZ][eE][dD] *)

let map_offset (env : env) (tok : CST.offset) =
  token env tok (* pattern [oO][fF][fF][sS][eE][tT] *)

let map_pat_213dc3e (env : env) (tok : CST.pat_213dc3e) =
  token env tok (* pattern [0-9] *)

let map_drop (env : env) (tok : CST.drop) =
  token env tok (* pattern [dD][rR][oO][pP] *)

let map_attach (env : env) (tok : CST.attach) =
  token env tok (* pattern [aA][tT][tT][aA][cC][hH] *)

let map_view (env : env) (tok : CST.view) =
  token env tok (* pattern [vV][iI][eE][wW] *)

let map_without (env : env) (tok : CST.without) =
  token env tok (* pattern [wW][iI][tT][hH][oO][uU][tT] *)

let map_between (env : env) (tok : CST.between) =
  token env tok (* pattern [bB][eE][tT][wW][eE][eE][nN] *)

let map_pat_29ffae5 (env : env) (tok : CST.pat_29ffae5) =
  token env tok (* pattern [_a-zA-Z\x80-\xFF][$_0-9a-zA-Z\x80-\xFF]* *)

let map_stored (env : env) (tok : CST.stored) =
  token env tok (* pattern [sS][tT][oO][rR][eE][dD] *)

let map_where (env : env) (tok : CST.where) =
  token env tok (* pattern [wW][hH][eE][rR][eE] *)

let map_alter (env : env) (tok : CST.alter) =
  token env tok (* pattern [aA][lL][tT][eE][rR] *)

let map_for_ (env : env) (tok : CST.for_) =
  token env tok (* pattern [fF][oO][rR] *)

let map_key (env : env) (tok : CST.key) =
  token env tok (* pattern [kK][eE][yY] *)

let map_savepoint (env : env) (tok : CST.savepoint) =
  token env tok (* pattern [sS][aA][vV][eE][pP][oO][iI][nN][tT] *)

let map_glob (env : env) (tok : CST.glob) =
  token env tok (* pattern [gG][lL][oO][bB] *)

let map_query (env : env) (tok : CST.query) =
  token env tok (* pattern [qQ][uU][eE][rR][yY] *)

let map_groups (env : env) (tok : CST.groups) =
  token env tok (* pattern [gG][rR][oO][uU][pP][sS] *)

let map_intersect (env : env) (tok : CST.intersect) =
  token env tok (* pattern [iI][nN][tT][eE][rR][sS][eE][cC][tT] *)

let map_case (env : env) (tok : CST.case) =
  token env tok (* pattern [cC][aA][sS][eE] *)

let map_delete (env : env) (tok : CST.delete) =
  token env tok (* pattern [dD][eE][lL][eE][tT][eE] *)

let map_replace (env : env) (tok : CST.replace) =
  token env tok (* pattern [rR][eE][pP][lL][aA][cC][eE] *)

let map_pat_4fd4a56 (env : env) (tok : CST.pat_4fd4a56) =
  token env tok (* pattern .* *)

let map_then_ (env : env) (tok : CST.then_) =
  token env tok (* pattern [tT][hH][eE][nN] *)

let map_foreign (env : env) (tok : CST.foreign) =
  token env tok (* pattern [fF][oO][rR][eE][iI][gG][nN] *)

let map_into (env : env) (tok : CST.into) =
  token env tok (* pattern [iI][nN][tT][oO] *)

let map_and_ (env : env) (tok : CST.and_) =
  token env tok (* pattern [aA][nN][dD] *)

let map_abort (env : env) (tok : CST.abort) =
  token env tok (* pattern [aA][bB][oO][rR][tT] *)

let map_nulls (env : env) (tok : CST.nulls) =
  token env tok (* pattern [nN][uU][lL][lL][sS] *)

let map_conflict (env : env) (tok : CST.conflict) =
  token env tok (* pattern [cC][oO][nN][fF][lL][iI][cC][tT] *)

let map_pat_bb5937d (env : env) (tok : CST.pat_bb5937d) =
  token env tok (* pattern (``|[^`])* *)

let map_returning (env : env) (tok : CST.returning) =
  token env tok (* pattern [rR][eE][tT][uU][rR][nN][iI][nN][gG] *)

let map_desc (env : env) (tok : CST.desc) =
  token env tok (* pattern [dD][eE][sS][cC] *)

let map_exclusive (env : env) (tok : CST.exclusive) =
  token env tok (* pattern [eE][xX][cC][lL][uU][sS][iI][vV][eE] *)

let map_unique (env : env) (tok : CST.unique) =
  token env tok (* pattern [uU][nN][iI][qQ][uU][eE] *)

let map_always (env : env) (tok : CST.always) =
  token env tok (* pattern [aA][lL][wW][aA][yY][sS] *)

let map_filter (env : env) (tok : CST.filter) =
  token env tok (* pattern [fF][iI][lL][tT][eE][rR] *)

let map_check (env : env) (tok : CST.check) =
  token env tok (* pattern [cC][hH][eE][cC][kK] *)

let map_rename (env : env) (tok : CST.rename) =
  token env tok (* pattern [rR][eE][nN][aA][mM][eE] *)

let map_match_ (env : env) (tok : CST.match_) =
  token env tok (* pattern [mM][aA][tT][cC][hH] *)

let map_rollback (env : env) (tok : CST.rollback) =
  token env tok (* pattern [rR][oO][lL][lL][bB][aA][cC][kK] *)

let map_commit (env : env) (tok : CST.commit) =
  token env tok (* pattern [cC][oO][mM][mM][iI][tT] *)

let map_fail (env : env) (tok : CST.fail) =
  token env tok (* pattern [fF][aA][iI][lL] *)

let map_add (env : env) (tok : CST.add) =
  token env tok (* pattern [aA][dD][dD] *)

let map_as_ (env : env) (tok : CST.as_) =
  token env tok (* pattern [aA][sS] *)

let map_anon_choice_temp_716f4ac (env : env) (x : CST.anon_choice_temp_716f4ac) =
  (match x with
  | `Temp_3d801aa tok ->
      token env tok (* pattern [tT][eE][mM][pP] *)
  | `Temp_d5197d9 tok ->
      token env tok (* pattern [tT][eE][mM][pP][oO][rR][aA][rR][yY] *)
  )

let map_string_ (env : env) ((v1, v2, v3) : CST.string_) =
  let v1 = token env v1 (* "'" *) in
  let v2 = token env v2 (* pattern "(''|[^'])*" *) in
  let v3 = token env v3 (* "'" *) in
  todo env (v1, v2, v3)

let map_join_operator (env : env) (x : CST.join_operator) =
  (match x with
  | `COMMA tok -> token env tok (* "," *)
  | `Opt_natu_opt_choice_left_opt_outer_join (v1, v2, v3) ->
      let v1 =
        (match v1 with
        | Some tok ->
            token env tok (* pattern [nN][aA][tT][uU][rR][aA][lL] *)
        | None -> todo env ())
      in
      let v2 =
        (match v2 with
        | Some x ->
            (match x with
            | `Left_opt_outer (v1, v2) ->
                let v1 = token env v1 (* pattern [lL][eE][fF][tT] *) in
                let v2 =
                  (match v2 with
                  | Some tok ->
                      token env tok (* pattern [oO][uU][tT][eE][rR] *)
                  | None -> todo env ())
                in
                todo env (v1, v2)
            | `Inner tok ->
                token env tok (* pattern [iI][nN][nN][eE][rR] *)
            | `Cross tok ->
                token env tok (* pattern [cC][rR][oO][sS][sS] *)
            )
        | None -> todo env ())
      in
      let v3 = token env v3 (* pattern [jJ][oO][iI][nN] *) in
      todo env (v1, v2, v3)
  )

let map_signed_number (env : env) ((v1, v2) : CST.signed_number) =
  let v1 =
    (match v1 with
    | Some x -> map_anon_choice_PLUS_da42005 env x
    | None -> todo env ())
  in
  let v2 = token env v2 (* numeric_literal *) in
  todo env (v1, v2)

let map_bind_parameter (env : env) (x : CST.bind_parameter) =
  (match x with
  | `QMARK_rep_pat_213dc3e (v1, v2) ->
      let v1 = token env v1 (* "?" *) in
      let v2 = List.map (token env) (* pattern [0-9] *) v2 in
      todo env (v1, v2)
  | `Choice_AT_pat_93c883a (v1, v2) ->
      let v1 =
        (match v1 with
        | `AT tok -> token env tok (* "@" *)
        | `DOLLAR tok -> token env tok (* "$" *)
        | `COLON tok -> token env tok (* ":" *)
        | `HASH tok -> token env tok (* "#" *)
        )
      in
      let v2 =
        token env v2 (* pattern [$_0-9a-zA-Z\x80-\xFF]+ *)
      in
      todo env (v1, v2)
  )

let map_compound_operator (env : env) (x : CST.compound_operator) =
  (match x with
  | `Union tok ->
      token env tok (* pattern [uU][nN][iI][oO][nN] *)
  | `Union_all (v1, v2) ->
      let v1 = token env v1 (* pattern [uU][nN][iI][oO][nN] *) in
      let v2 = token env v2 (* pattern [aA][lL][lL] *) in
      todo env (v1, v2)
  | `Inte tok ->
      token env tok (* pattern [iI][nN][tT][eE][rR][sS][eE][cC][tT] *)
  | `Except tok ->
      token env tok (* pattern [eE][xX][cC][eE][pP][tT] *)
  )

let map_identifier (env : env) (x : CST.identifier) =
  (match x with
  | `Pat_29ffae5 tok ->
      token env tok (* pattern [_a-zA-Z\x80-\xFF][$_0-9a-zA-Z\x80-\xFF]* *)
  | `DQUOT_pat_73398bc_DQUOT (v1, v2, v3) ->
      let v1 = token env v1 (* "\"" *) in
      let v2 = token env v2 (* pattern "(\"\"|[^\"])*" *) in
      let v3 = token env v3 (* "\"" *) in
      todo env (v1, v2, v3)
  | `BQUOT_pat_bb5937d_BQUOT (v1, v2, v3) ->
      let v1 = token env v1 (* "`" *) in
      let v2 = token env v2 (* pattern (``|[^`])* *) in
      let v3 = token env v3 (* "`" *) in
      todo env (v1, v2, v3)
  | `LBRACK_pat_f154b4a_RBRACK (v1, v2, v3) ->
      let v1 = token env v1 (* "[" *) in
      let v2 = token env v2 (* pattern [^\]]* *) in
      let v3 = token env v3 (* "]" *) in
      todo env (v1, v2, v3)
  )

let map_anon_choice_asc_fa4fd8b (env : env) (x : CST.anon_choice_asc_fa4fd8b) =
  (match x with
  | `Asc tok -> token env tok (* pattern [aA][sS][cC] *)
  | `Desc tok -> token env tok (* pattern [dD][eE][sS][cC] *)
  )

let map_anon_choice_abort_500a898 (env : env) (x : CST.anon_choice_abort_500a898) =
  (match x with
  | `Abort tok ->
      token env tok (* pattern [aA][bB][oO][rR][tT] *)
  | `Fail tok -> token env tok (* pattern [fF][aA][iI][lL] *)
  | `Ignore tok ->
      token env tok (* pattern [iI][gG][nN][oO][rR][eE] *)
  | `Repl tok ->
      token env tok (* pattern [rR][eE][pP][lL][aA][cC][eE] *)
  | `Roll tok ->
      token env tok (* pattern [rR][oO][lL][lL][bB][aA][cC][kK] *)
  )

let map_conflict_clause (env : env) ((v1, v2, v3) : CST.conflict_clause) =
  let v1 = token env v1 (* pattern [oO][nN] *) in
  let v2 =
    token env v2 (* pattern [cC][oO][nN][fF][lL][iI][cC][tT] *)
  in
  let v3 =
    (match v3 with
    | `Roll tok ->
        token env tok (* pattern [rR][oO][lL][lL][bB][aA][cC][kK] *)
    | `Abort tok ->
        token env tok (* pattern [aA][bB][oO][rR][tT] *)
    | `Fail tok -> token env tok (* pattern [fF][aA][iI][lL] *)
    | `Ignore tok ->
        token env tok (* pattern [iI][gG][nN][oO][rR][eE] *)
    | `Repl tok ->
        token env tok (* pattern [rR][eE][pP][lL][aA][cC][eE] *)
    )
  in
  todo env (v1, v2, v3)

let map_string_literal (env : env) (x : CST.string_literal) =
  map_string_ env x

let map_function_name (env : env) (x : CST.function_name) =
  map_identifier env x

let map_name (env : env) (x : CST.name) =
  (match x with
  | `Str_lit x -> map_string_literal env x
  | `Id x -> map_function_name env x
  )

let map_collation_name (env : env) (x : CST.collation_name) =
  (match x with
  | `Str_lit x -> map_string_literal env x
  | `Id x -> map_function_name env x
  )

let map_literal_value (env : env) (x : CST.literal_value) =
  (match x with
  | `Nume_lit tok -> token env tok (* numeric_literal *)
  | `Str_lit x -> map_string_literal env x
  | `Blob_lit (v1, v2) ->
      let v1 =
        (match v1 with
        | `X_9dd4e46 tok -> token env tok (* "x" *)
        | `X_02129bb tok -> token env tok (* "X" *)
        )
      in
      let v2 = map_string_literal env v2 in
      todo env (v1, v2)
  | `Null tok -> token env tok (* pattern [nN][uU][lL][lL] *)
  | `True tok -> token env tok (* pattern [tT][rR][uU][eE] *)
  | `False tok ->
      token env tok (* pattern [fF][aA][lL][sS][eE] *)
  | `Curr_time_95f1cf7 tok ->
      token env tok (* pattern [cC][uU][rR][rR][eE][nN][tT][__][tT][iI][mM][eE] *)
  | `Curr_date tok ->
      token env tok (* pattern [cC][uU][rR][rR][eE][nN][tT][__][dD][aA][tT][eE] *)
  | `Curr_time_72fc600 tok ->
      token env tok (* pattern [cC][uU][rR][rR][eE][nN][tT][__][tT][iI][mM][eE][sS][tT][aA][mM][pP] *)
  )

let map_error_message (env : env) (x : CST.error_message) =
  map_name env x

let map_anon_opt_opt_as_error_mess_6e99511 (env : env) (opt : CST.anon_opt_opt_as_error_mess_6e99511) =
  (match opt with
  | Some (v1, v2) ->
      let v1 =
        (match v1 with
        | Some tok -> token env tok (* pattern [aA][sS] *)
        | None -> todo env ())
      in
      let v2 = map_error_message env v2 in
      todo env (v1, v2)
  | None -> todo env ())

let map_name2 (env : env) ((v1, v2) : CST.name2) =
  let v1 =
    (match v1 with
    | Some (v1, v2) ->
        let v1 = map_error_message env v1 in
        let v2 = token env v2 (* "." *) in
        todo env (v1, v2)
    | None -> todo env ())
  in
  let v2 = map_error_message env v2 in
  todo env (v1, v2)

let map_savepoint_stmt (env : env) ((v1, v2) : CST.savepoint_stmt) =
  let v1 =
    token env v1 (* pattern [sS][aA][vV][eE][pP][oO][iI][nN][tT] *)
  in
  let v2 = map_error_message env v2 in
  todo env (v1, v2)

let map_type_name (env : env) ((v1, v2) : CST.type_name) =
  let v1 = List.map (map_error_message env) v1 in
  let v2 =
    (match v2 with
    | Some x ->
        (match x with
        | `LPAR_signed_num_RPAR (v1, v2, v3) ->
            let v1 = token env v1 (* "(" *) in
            let v2 = map_signed_number env v2 in
            let v3 = token env v3 (* ")" *) in
            todo env (v1, v2, v3)
        | `LPAR_signed_num_COMMA_signed_num_RPAR (v1, v2, v3, v4, v5) ->
            let v1 = token env v1 (* "(" *) in
            let v2 = map_signed_number env v2 in
            let v3 = token env v3 (* "," *) in
            let v4 = map_signed_number env v4 in
            let v5 = token env v5 (* ")" *) in
            todo env (v1, v2, v3, v4, v5)
        )
    | None -> todo env ())
  in
  todo env (v1, v2)

let map_pragma_value (env : env) (x : CST.pragma_value) =
  (match x with
  | `Signed_num x -> map_signed_number env x
  | `Name x -> map_error_message env x
  )

let map_anon_choice_inde_by_error_mess_0500888 (env : env) (x : CST.anon_choice_inde_by_error_mess_0500888) =
  (match x with
  | `Inde_by_name (v1, v2, v3) ->
      let v1 =
        token env v1 (* pattern [iI][nN][dD][eE][xX][eE][dD] *)
      in
      let v2 = token env v2 (* pattern [bB][yY] *) in
      let v3 = map_error_message env v3 in
      todo env (v1, v2, v3)
  | `Not_inde (v1, v2) ->
      let v1 = token env v1 (* pattern [nN][oO][tT] *) in
      let v2 =
        token env v2 (* pattern [iI][nN][dD][eE][xX][eE][dD] *)
      in
      todo env (v1, v2)
  )

let map_detach_stmt (env : env) ((v1, v2, v3) : CST.detach_stmt) =
  let v1 =
    token env v1 (* pattern [dD][eE][tT][aA][cC][hH] *)
  in
  let v2 =
    (match v2 with
    | Some tok ->
        token env tok (* pattern [dD][aA][tT][aA][bB][aA][sS][eE] *)
    | None -> todo env ())
  in
  let v3 = map_error_message env v3 in
  todo env (v1, v2, v3)

let map_anon_opt_tran_opt_error_mess_65fc71e (env : env) (opt : CST.anon_opt_tran_opt_error_mess_65fc71e) =
  (match opt with
  | Some (v1, v2) ->
      let v1 =
        token env v1 (* pattern [tT][rR][aA][nN][sS][aA][cC][tT][iI][oO][nN] *)
      in
      let v2 =
        (match v2 with
        | Some x -> map_error_message env x
        | None -> todo env ())
      in
      todo env (v1, v2)
  | None -> todo env ())

let map_release_stmt (env : env) ((v1, v2, v3) : CST.release_stmt) =
  let v1 =
    token env v1 (* pattern [rR][eE][lL][eE][aA][sS][eE] *)
  in
  let v2 =
    (match v2 with
    | Some tok ->
        token env tok (* pattern [sS][aA][vV][eE][pP][oO][iI][nN][tT] *)
    | None -> todo env ())
  in
  let v3 = map_error_message env v3 in
  todo env (v1, v2, v3)

let map_column_name_list (env : env) ((v1, v2, v3, v4) : CST.column_name_list) =
  let v1 = token env v1 (* "(" *) in
  let v2 = map_error_message env v2 in
  let v3 =
    List.map (fun (v1, v2) ->
      let v1 = token env v1 (* "," *) in
      let v2 = map_error_message env v2 in
      todo env (v1, v2)
    ) v3
  in
  let v4 = token env v4 (* ")" *) in
  todo env (v1, v2, v3, v4)

let map_analyze_stmt (env : env) ((v1, v2) : CST.analyze_stmt) =
  let v1 =
    token env v1 (* pattern [aA][nN][aA][lL][yY][zZ][eE] *)
  in
  let v2 =
    (match v2 with
    | Some x -> map_name2 env x
    | None -> todo env ())
  in
  todo env (v1, v2)

let map_reindex_stmt (env : env) ((v1, v2) : CST.reindex_stmt) =
  let v1 =
    token env v1 (* pattern [rR][eE][iI][nN][dD][eE][xX] *)
  in
  let v2 =
    (match v2 with
    | Some x -> map_name2 env x
    | None -> todo env ())
  in
  todo env (v1, v2)

let map_drop_trigger_stmt (env : env) ((v1, v2, v3, v4) : CST.drop_trigger_stmt) =
  let v1 = token env v1 (* pattern [dD][rR][oO][pP] *) in
  let v2 =
    token env v2 (* pattern [tT][rR][iI][gG][gG][eE][rR] *)
  in
  let v3 =
    (match v3 with
    | Some (v1, v2) ->
        let v1 = token env v1 (* pattern [iI][fF] *) in
        let v2 =
          token env v2 (* pattern [eE][xX][iI][sS][tT][sS] *)
        in
        todo env (v1, v2)
    | None -> todo env ())
  in
  let v4 = map_name2 env v4 in
  todo env (v1, v2, v3, v4)

let map_drop_view_stmt (env : env) ((v1, v2, v3, v4) : CST.drop_view_stmt) =
  let v1 = token env v1 (* pattern [dD][rR][oO][pP] *) in
  let v2 = token env v2 (* pattern [vV][iI][eE][wW] *) in
  let v3 =
    (match v3 with
    | Some (v1, v2) ->
        let v1 = token env v1 (* pattern [iI][fF] *) in
        let v2 =
          token env v2 (* pattern [eE][xX][iI][sS][tT][sS] *)
        in
        todo env (v1, v2)
    | None -> todo env ())
  in
  let v4 = map_name2 env v4 in
  todo env (v1, v2, v3, v4)

let map_drop_table_stmt (env : env) ((v1, v2, v3, v4) : CST.drop_table_stmt) =
  let v1 = token env v1 (* pattern [dD][rR][oO][pP] *) in
  let v2 = token env v2 (* pattern [tT][aA][bB][lL][eE] *) in
  let v3 =
    (match v3 with
    | Some (v1, v2) ->
        let v1 = token env v1 (* pattern [iI][fF] *) in
        let v2 =
          token env v2 (* pattern [eE][xX][iI][sS][tT][sS] *)
        in
        todo env (v1, v2)
    | None -> todo env ())
  in
  let v4 = map_name2 env v4 in
  todo env (v1, v2, v3, v4)

let map_drop_index_stmt (env : env) ((v1, v2, v3, v4) : CST.drop_index_stmt) =
  let v1 = token env v1 (* pattern [dD][rR][oO][pP] *) in
  let v2 = token env v2 (* pattern [iI][nN][dD][eE][xX] *) in
  let v3 =
    (match v3 with
    | Some (v1, v2) ->
        let v1 = token env v1 (* pattern [iI][fF] *) in
        let v2 =
          token env v2 (* pattern [eE][xX][iI][sS][tT][sS] *)
        in
        todo env (v1, v2)
    | None -> todo env ())
  in
  let v4 = map_name2 env v4 in
  todo env (v1, v2, v3, v4)

let map_pragma_stmt (env : env) ((v1, v2, v3) : CST.pragma_stmt) =
  let v1 =
    token env v1 (* pattern [pP][rR][aA][gG][mM][aA] *)
  in
  let v2 = map_name2 env v2 in
  let v3 =
    (match v3 with
    | Some x ->
        (match x with
        | `EQ_pragma_value (v1, v2) ->
            let v1 = token env v1 (* "=" *) in
            let v2 = map_pragma_value env v2 in
            todo env (v1, v2)
        | `LPAR_pragma_value_RPAR (v1, v2, v3) ->
            let v1 = token env v1 (* "(" *) in
            let v2 = map_pragma_value env v2 in
            let v3 = token env v3 (* ")" *) in
            todo env (v1, v2, v3)
        )
    | None -> todo env ())
  in
  todo env (v1, v2, v3)

let map_qualified_table_name (env : env) ((v1, v2, v3) : CST.qualified_table_name) =
  let v1 = map_name2 env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) ->
        let v1 = token env v1 (* pattern [aA][sS] *) in
        let v2 = map_error_message env v2 in
        todo env (v1, v2)
    | None -> todo env ())
  in
  let v3 =
    (match v3 with
    | Some x -> map_anon_choice_inde_by_error_mess_0500888 env x
    | None -> todo env ())
  in
  todo env (v1, v2, v3)

let map_rollback_stmt (env : env) ((v1, v2, v3) : CST.rollback_stmt) =
  let v1 =
    token env v1 (* pattern [rR][oO][lL][lL][bB][aA][cC][kK] *)
  in
  let v2 = map_anon_opt_tran_opt_error_mess_65fc71e env v2 in
  let v3 =
    (match v3 with
    | Some (v1, v2, v3) ->
        let v1 = token env v1 (* pattern [tT][oO] *) in
        let v2 =
          (match v2 with
          | Some tok ->
              token env tok (* pattern [sS][aA][vV][eE][pP][oO][iI][nN][tT] *)
          | None -> todo env ())
        in
        let v3 = map_error_message env v3 in
        todo env (v1, v2, v3)
    | None -> todo env ())
  in
  todo env (v1, v2, v3)

let map_begin_stmt (env : env) ((v1, v2, v3) : CST.begin_stmt) =
  let v1 = token env v1 (* pattern [bB][eE][gG][iI][nN] *) in
  let v2 =
    (match v2 with
    | Some x ->
        (match x with
        | `Defe tok ->
            token env tok (* pattern [dD][eE][fF][eE][rR][rR][eE][dD] *)
        | `Imme tok ->
            token env tok (* pattern [iI][mM][mM][eE][dD][iI][aA][tT][eE] *)
        | `Excl tok ->
            token env tok (* pattern [eE][xX][cC][lL][uU][sS][iI][vV][eE] *)
        )
    | None -> todo env ())
  in
  let v3 = map_anon_opt_tran_opt_error_mess_65fc71e env v3 in
  todo env (v1, v2, v3)

let map_commit_stmt (env : env) ((v1, v2) : CST.commit_stmt) =
  let v1 =
    (match v1 with
    | `Commit tok ->
        token env tok (* pattern [cC][oO][mM][mM][iI][tT] *)
    | `End tok -> token env tok (* pattern [eE][nN][dD] *)
    )
  in
  let v2 = map_anon_opt_tran_opt_error_mess_65fc71e env v2 in
  todo env (v1, v2)

let map_anon_choice_error_mess_facbc16 (env : env) (x : CST.anon_choice_error_mess_facbc16) =
  (match x with
  | `Name x -> map_error_message env x
  | `Column_name_list x -> map_column_name_list env x
  )

let rec map_anon_COMMA_LPAR_file_rep_COMMA_file_RPAR_a378f47 (env : env) ((v1, v2, v3, v4, v5) : CST.anon_COMMA_LPAR_file_rep_COMMA_file_RPAR_a378f47) =
  let v1 = token env v1 (* "," *) in
  let v2 = token env v2 (* "(" *) in
  let v3 = map_filename env v3 in
  let v4 =
    List.map (fun (v1, v2) ->
      let v1 = token env v1 (* "," *) in
      let v2 = map_filename env v2 in
      todo env (v1, v2)
    ) v4
  in
  let v5 = token env v5 (* ")" *) in
  todo env (v1, v2, v3, v4, v5)

and map_anon_file_rep_COMMA_file_3ba9398 (env : env) ((v1, v2) : CST.anon_file_rep_COMMA_file_3ba9398) =
  let v1 = map_filename env v1 in
  let v2 =
    List.map (fun (v1, v2) ->
      let v1 = token env v1 (* "," *) in
      let v2 = map_filename env v2 in
      todo env (v1, v2)
    ) v2
  in
  todo env (v1, v2)

and map_common_table_expression (env : env) (x : CST.common_table_expression) =
  (match x with
  | `Rectype (v1, v2, v3, v4, v5, v6, v7) ->
      let v1 = map_error_message env v1 in
      let v2 =
        (match v2 with
        | Some x -> map_column_name_list env x
        | None -> todo env ())
      in
      let v3 = token env v3 (* pattern [aA][sS] *) in
      let v4 =
        (match v4 with
        | Some (v1, v2) ->
            let v1 =
              (match v1 with
              | Some tok -> token env tok (* pattern [nN][oO][tT] *)
              | None -> todo env ())
            in
            let v2 =
              token env v2 (* pattern [mM][aA][tT][eE][rR][iI][aA][lL][iI][zZ][eE][dD] *)
            in
            todo env (v1, v2)
        | None -> todo env ())
      in
      let v5 = token env v5 (* "(" *) in
      let v6 = map_select_stmt env v6 in
      let v7 = token env v7 (* ")" *) in
      todo env (v1, v2, v3, v4, v5, v6, v7)
  )

and map_expr (env : env) (x : CST.expr) =
  (match x with
  | `Lit_value x -> map_literal_value env x
  | `Bind_param x -> map_bind_parameter env x
  | `Name x -> map_error_message env x
  | `Name_DOT_name (v1, v2, v3) ->
      let v1 = map_error_message env v1 in
      let v2 = token env v2 (* "." *) in
      let v3 = map_error_message env v3 in
      todo env (v1, v2, v3)
  | `Name_DOT_name_DOT_name (v1, v2, v3, v4, v5) ->
      let v1 = map_error_message env v1 in
      let v2 = token env v2 (* "." *) in
      let v3 = map_error_message env v3 in
      let v4 = token env v4 (* "." *) in
      let v5 = map_error_message env v5 in
      todo env (v1, v2, v3, v4, v5)
  | `TILDE_expr (v1, v2) ->
      let v1 = token env v1 (* "~" *) in
      let v2 = map_filename env v2 in
      todo env (v1, v2)
  | `Choice_DASH_expr (v1, v2) ->
      let v1 =
        (match v1 with
        | `DASH tok -> token env tok (* "-" *)
        | `PLUS tok -> token env tok (* "+" *)
        )
      in
      let v2 = map_filename env v2 in
      todo env (v1, v2)
  | `Not_expr (v1, v2) ->
      let v1 = token env v1 (* pattern [nN][oO][tT] *) in
      let v2 = map_filename env v2 in
      todo env (v1, v2)
  | `Expr_BARBAR_expr (v1, v2, v3) ->
      let v1 = map_filename env v1 in
      let v2 = token env v2 (* "||" *) in
      let v3 = map_filename env v3 in
      todo env (v1, v2, v3)
  | `Expr_choice_STAR_expr (v1, v2, v3) ->
      let v1 = map_filename env v1 in
      let v2 =
        (match v2 with
        | `STAR tok -> token env tok (* "*" *)
        | `SLASH tok -> token env tok (* "/" *)
        | `PERC tok -> token env tok (* "%" *)
        )
      in
      let v3 = map_filename env v3 in
      todo env (v1, v2, v3)
  | `Expr_choice_PLUS_expr (v1, v2, v3) ->
      let v1 = map_filename env v1 in
      let v2 = map_anon_choice_PLUS_da42005 env v2 in
      let v3 = map_filename env v3 in
      todo env (v1, v2, v3)
  | `Expr_choice_LTLT_expr (v1, v2, v3) ->
      let v1 = map_filename env v1 in
      let v2 =
        (match v2 with
        | `LTLT tok -> token env tok (* "<<" *)
        | `GTGT tok -> token env tok (* ">>" *)
        | `AMP tok -> token env tok (* "&" *)
        | `BAR tok -> token env tok (* "|" *)
        )
      in
      let v3 = map_filename env v3 in
      todo env (v1, v2, v3)
  | `Expr_choice_LT_expr (v1, v2, v3) ->
      let v1 = map_filename env v1 in
      let v2 =
        (match v2 with
        | `LT tok -> token env tok (* "<" *)
        | `LTEQ tok -> token env tok (* "<=" *)
        | `GT tok -> token env tok (* ">" *)
        | `GTEQ tok -> token env tok (* ">=" *)
        )
      in
      let v3 = map_filename env v3 in
      todo env (v1, v2, v3)
  | `Expr_choice_EQ_expr (v1, v2, v3) ->
      let v1 = map_filename env v1 in
      let v2 =
        (match v2 with
        | `EQ tok -> token env tok (* "=" *)
        | `EQEQ tok -> token env tok (* "==" *)
        | `BANGEQ tok -> token env tok (* "!=" *)
        | `LTGT tok -> token env tok (* "<>" *)
        )
      in
      let v3 = map_filename env v3 in
      todo env (v1, v2, v3)
  | `Expr_opt_not_in_choice_LPAR_opt_choice_select_stmt_RPAR (v1, v2, v3, v4) ->
      let v1 = map_filename env v1 in
      let v2 =
        (match v2 with
        | Some tok -> token env tok (* pattern [nN][oO][tT] *)
        | None -> todo env ())
      in
      let v3 = token env v3 (* pattern [iI][nN] *) in
      let v4 =
        (match v4 with
        | `LPAR_opt_choice_select_stmt_RPAR (v1, v2, v3) ->
            let v1 = token env v1 (* "(" *) in
            let v2 =
              (match v2 with
              | Some x ->
                  (match x with
                  | `Select_stmt x -> map_select_stmt env x
                  | `Expr_rep_COMMA_expr x ->
                      map_anon_file_rep_COMMA_file_3ba9398 env x
                  )
              | None -> todo env ())
            in
            let v3 = token env v3 (* ")" *) in
            todo env (v1, v2, v3)
        | `Name2_opt_LPAR_opt_expr_rep_COMMA_expr_RPAR (v1, v2) ->
            let v1 = map_name2 env v1 in
            let v2 =
              (match v2 with
              | Some (v1, v2, v3) ->
                  let v1 = token env v1 (* "(" *) in
                  let v2 =
                    (match v2 with
                    | Some x -> map_anon_file_rep_COMMA_file_3ba9398 env x
                    | None -> todo env ())
                  in
                  let v3 = token env v3 (* ")" *) in
                  todo env (v1, v2, v3)
              | None -> todo env ())
            in
            todo env (v1, v2)
        )
      in
      todo env (v1, v2, v3, v4)
  | `Expr_and_expr (v1, v2, v3) ->
      let v1 = map_filename env v1 in
      let v2 = token env v2 (* pattern [aA][nN][dD] *) in
      let v3 = map_filename env v3 in
      todo env (v1, v2, v3)
  | `Expr_or_expr (v1, v2, v3) ->
      let v1 = map_filename env v1 in
      let v2 = token env v2 (* pattern [oO][rR] *) in
      let v3 = map_filename env v3 in
      todo env (v1, v2, v3)
  | `Func_name_LPAR_opt_choice_opt_dist_expr_rep_COMMA_expr_RPAR_opt_filter_clause_opt_over_clause (v1, v2, v3, v4, v5, v6) ->
      let v1 = map_function_name env v1 in
      let v2 = token env v2 (* "(" *) in
      let v3 =
        (match v3 with
        | Some x ->
            (match x with
            | `Opt_dist_expr_rep_COMMA_expr (v1, v2, v3) ->
                let v1 =
                  (match v1 with
                  | Some tok ->
                      token env tok (* pattern [dD][iI][sS][tT][iI][nN][cC][tT] *)
                  | None -> todo env ())
                in
                let v2 = map_filename env v2 in
                let v3 =
                  List.map (fun (v1, v2) ->
                    let v1 = token env v1 (* "," *) in
                    let v2 = map_filename env v2 in
                    todo env (v1, v2)
                  ) v3
                in
                todo env (v1, v2, v3)
            | `STAR tok -> token env tok (* "*" *)
            )
        | None -> todo env ())
      in
      let v4 = token env v4 (* ")" *) in
      let v5 =
        (match v5 with
        | Some x -> map_filter_clause env x
        | None -> todo env ())
      in
      let v6 =
        (match v6 with
        | Some x -> map_over_clause env x
        | None -> todo env ())
      in
      todo env (v1, v2, v3, v4, v5, v6)
  | `LPAR_expr_rep_COMMA_expr_RPAR (v1, v2, v3, v4) ->
      let v1 = token env v1 (* "(" *) in
      let v2 = map_filename env v2 in
      let v3 =
        List.map (fun (v1, v2) ->
          let v1 = token env v1 (* "," *) in
          let v2 = map_filename env v2 in
          todo env (v1, v2)
        ) v3
      in
      let v4 = token env v4 (* ")" *) in
      todo env (v1, v2, v3, v4)
  | `Cast_LPAR_expr_as_type_name_RPAR (v1, v2, v3, v4, v5, v6) ->
      let v1 = token env v1 (* pattern [cC][aA][sS][tT] *) in
      let v2 = token env v2 (* "(" *) in
      let v3 = map_filename env v3 in
      let v4 = token env v4 (* pattern [aA][sS] *) in
      let v5 = map_type_name env v5 in
      let v6 = token env v6 (* ")" *) in
      todo env (v1, v2, v3, v4, v5, v6)
  | `Expr_coll_coll_name (v1, v2, v3) ->
      let v1 = map_filename env v1 in
      let v2 =
        token env v2 (* pattern [cC][oO][lL][lL][aA][tT][eE] *)
      in
      let v3 = map_collation_name env v3 in
      todo env (v1, v2, v3)
  | `Expr_opt_not_choice_like_expr_opt_esc_expr (v1, v2, v3, v4, v5) ->
      let v1 = map_filename env v1 in
      let v2 =
        (match v2 with
        | Some tok -> token env tok (* pattern [nN][oO][tT] *)
        | None -> todo env ())
      in
      let v3 =
        (match v3 with
        | `Like tok -> token env tok (* pattern [lL][iI][kK][eE] *)
        | `Glob tok -> token env tok (* pattern [gG][lL][oO][bB] *)
        | `Regex tok ->
            token env tok (* pattern [rR][eE][gG][eE][xX][pP] *)
        | `Match tok ->
            token env tok (* pattern [mM][aA][tT][cC][hH] *)
        )
      in
      let v4 = map_filename env v4 in
      let v5 =
        (match v5 with
        | Some (v1, v2) ->
            let v1 =
              token env v1 (* pattern [eE][sS][cC][aA][pP][eE] *)
            in
            let v2 = map_filename env v2 in
            todo env (v1, v2)
        | None -> todo env ())
      in
      todo env (v1, v2, v3, v4, v5)
  | `Expr_choice_isnull (v1, v2) ->
      let v1 = map_filename env v1 in
      let v2 =
        (match v2 with
        | `Isnull tok ->
            token env tok (* pattern [iI][sS][nN][uU][lL][lL] *)
        | `Notn tok ->
            token env tok (* pattern [nN][oO][tT][nN][uU][lL][lL] *)
        | `Not_null (v1, v2) ->
            let v1 = token env v1 (* pattern [nN][oO][tT] *) in
            let v2 = token env v2 (* pattern [nN][uU][lL][lL] *) in
            todo env (v1, v2)
        )
      in
      todo env (v1, v2)
  | `Expr_is_opt_not_expr (v1, v2, v3, v4) ->
      let v1 = map_filename env v1 in
      let v2 = token env v2 (* pattern [iI][sS] *) in
      let v3 =
        (match v3 with
        | Some tok -> token env tok (* pattern [nN][oO][tT] *)
        | None -> todo env ())
      in
      let v4 = map_filename env v4 in
      todo env (v1, v2, v3, v4)
  | `Expr_opt_not_betw_expr_and_expr (v1, v2, v3, v4, v5, v6) ->
      let v1 = map_filename env v1 in
      let v2 =
        (match v2 with
        | Some tok -> token env tok (* pattern [nN][oO][tT] *)
        | None -> todo env ())
      in
      let v3 =
        token env v3 (* pattern [bB][eE][tT][wW][eE][eE][nN] *)
      in
      let v4 = map_filename env v4 in
      let v5 = token env v5 (* pattern [aA][nN][dD] *) in
      let v6 = map_filename env v6 in
      todo env (v1, v2, v3, v4, v5, v6)
  | `LPAR_select_stmt_RPAR (v1, v2, v3) ->
      let v1 = token env v1 (* "(" *) in
      let v2 = map_select_stmt env v2 in
      let v3 = token env v3 (* ")" *) in
      todo env (v1, v2, v3)
  | `Exists_LPAR_select_stmt_RPAR (v1, v2, v3, v4) ->
      let v1 =
        token env v1 (* pattern [eE][xX][iI][sS][tT][sS] *)
      in
      let v2 = token env v2 (* "(" *) in
      let v3 = map_select_stmt env v3 in
      let v4 = token env v4 (* ")" *) in
      todo env (v1, v2, v3, v4)
  | `Not_exists_LPAR_select_stmt_RPAR (v1, v2, v3, v4, v5) ->
      let v1 = token env v1 (* pattern [nN][oO][tT] *) in
      let v2 =
        token env v2 (* pattern [eE][xX][iI][sS][tT][sS] *)
      in
      let v3 = token env v3 (* "(" *) in
      let v4 = map_select_stmt env v4 in
      let v5 = token env v5 (* ")" *) in
      todo env (v1, v2, v3, v4, v5)
  | `Case_opt_expr_rep1_when_expr_then_expr_opt_else_expr_end (v1, v2, v3, v4, v5) ->
      let v1 = token env v1 (* pattern [cC][aA][sS][eE] *) in
      let v2 =
        (match v2 with
        | Some x -> map_filename env x
        | None -> todo env ())
      in
      let v3 =
        List.map (fun (v1, v2, v3, v4) ->
          let v1 = token env v1 (* pattern [wW][hH][eE][nN] *) in
          let v2 = map_filename env v2 in
          let v3 = token env v3 (* pattern [tT][hH][eE][nN] *) in
          let v4 = map_filename env v4 in
          todo env (v1, v2, v3, v4)
        ) v3
      in
      let v4 =
        (match v4 with
        | Some (v1, v2) ->
            let v1 = token env v1 (* pattern [eE][lL][sS][eE] *) in
            let v2 = map_filename env v2 in
            todo env (v1, v2)
        | None -> todo env ())
      in
      let v5 = token env v5 (* pattern [eE][nN][dD] *) in
      todo env (v1, v2, v3, v4, v5)
  | `Raise_func (v1, v2, v3, v4) ->
      let v1 = token env v1 (* pattern [rR][aA][iI][sS][eE] *) in
      let v2 = token env v2 (* "(" *) in
      let v3 =
        (match v3 with
        | `Ignore tok ->
            token env tok (* pattern [iI][gG][nN][oO][rR][eE] *)
        | `Choice_roll_COMMA_error_mess (v1, v2, v3) ->
            let v1 =
              (match v1 with
              | `Roll tok ->
                  token env tok (* pattern [rR][oO][lL][lL][bB][aA][cC][kK] *)
              | `Abort tok ->
                  token env tok (* pattern [aA][bB][oO][rR][tT] *)
              | `Fail tok -> token env tok (* pattern [fF][aA][iI][lL] *)
              )
            in
            let v2 = token env v2 (* "," *) in
            let v3 = map_error_message env v3 in
            todo env (v1, v2, v3)
        )
      in
      let v4 = token env v4 (* ")" *) in
      todo env (v1, v2, v3, v4)
  )

and map_filename (env : env) (x : CST.filename) =
  map_expr env x

and map_filter_clause (env : env) ((v1, v2, v3, v4, v5) : CST.filter_clause) =
  let v1 =
    token env v1 (* pattern [fF][iI][lL][tT][eE][rR] *)
  in
  let v2 = token env v2 (* "(" *) in
  let v3 = token env v3 (* pattern [wW][hH][eE][rR][eE] *) in
  let v4 = map_filename env v4 in
  let v5 = token env v5 (* ")" *) in
  todo env (v1, v2, v3, v4, v5)

and map_frame_spec (env : env) ((v1, v2, v3) : CST.frame_spec) =
  let v1 =
    (match v1 with
    | `Range tok ->
        token env tok (* pattern [rR][aA][nN][gG][eE] *)
    | `Rows tok -> token env tok (* pattern [rR][oO][wW][sS] *)
    | `Groups tok ->
        token env tok (* pattern [gG][rR][oO][uU][pP][sS] *)
    )
  in
  let v2 =
    (match v2 with
    | `Betw_choice_unbo_prec_and_choice_expr_prec (v1, v2, v3, v4) ->
        let v1 =
          token env v1 (* pattern [bB][eE][tT][wW][eE][eE][nN] *)
        in
        let v2 =
          (match v2 with
          | `Unbo_prec (v1, v2) ->
              let v1 =
                token env v1 (* pattern [uU][nN][bB][oO][uU][nN][dD][eE][dD] *)
              in
              let v2 =
                token env v2 (* pattern [pP][rR][eE][cC][eE][dD][iI][nN][gG] *)
              in
              todo env (v1, v2)
          | `Expr_prec (v1, v2) ->
              let v1 = map_filename env v1 in
              let v2 =
                token env v2 (* pattern [pP][rR][eE][cC][eE][dD][iI][nN][gG] *)
              in
              todo env (v1, v2)
          | `Curr_row (v1, v2) ->
              let v1 =
                token env v1 (* pattern [cC][uU][rR][rR][eE][nN][tT] *)
              in
              let v2 = token env v2 (* pattern [rR][oO][wW] *) in
              todo env (v1, v2)
          | `Expr_foll (v1, v2) ->
              let v1 = map_filename env v1 in
              let v2 =
                token env v2 (* pattern [fF][oO][lL][lL][oO][wW][iI][nN][gG] *)
              in
              todo env (v1, v2)
          )
        in
        let v3 = token env v3 (* pattern [aA][nN][dD] *) in
        let v4 =
          (match v4 with
          | `Expr_prec (v1, v2) ->
              let v1 = map_filename env v1 in
              let v2 =
                token env v2 (* pattern [pP][rR][eE][cC][eE][dD][iI][nN][gG] *)
              in
              todo env (v1, v2)
          | `Curr_row (v1, v2) ->
              let v1 =
                token env v1 (* pattern [cC][uU][rR][rR][eE][nN][tT] *)
              in
              let v2 = token env v2 (* pattern [rR][oO][wW] *) in
              todo env (v1, v2)
          | `Expr_foll (v1, v2) ->
              let v1 = map_filename env v1 in
              let v2 =
                token env v2 (* pattern [fF][oO][lL][lL][oO][wW][iI][nN][gG] *)
              in
              todo env (v1, v2)
          | `Unbo_foll (v1, v2) ->
              let v1 =
                token env v1 (* pattern [uU][nN][bB][oO][uU][nN][dD][eE][dD] *)
              in
              let v2 =
                token env v2 (* pattern [fF][oO][lL][lL][oO][wW][iI][nN][gG] *)
              in
              todo env (v1, v2)
          )
        in
        todo env (v1, v2, v3, v4)
    | `Unbo_prec (v1, v2) ->
        let v1 =
          token env v1 (* pattern [uU][nN][bB][oO][uU][nN][dD][eE][dD] *)
        in
        let v2 =
          token env v2 (* pattern [pP][rR][eE][cC][eE][dD][iI][nN][gG] *)
        in
        todo env (v1, v2)
    | `Expr_prec (v1, v2) ->
        let v1 = map_filename env v1 in
        let v2 =
          token env v2 (* pattern [pP][rR][eE][cC][eE][dD][iI][nN][gG] *)
        in
        todo env (v1, v2)
    | `Curr_row (v1, v2) ->
        let v1 =
          token env v1 (* pattern [cC][uU][rR][rR][eE][nN][tT] *)
        in
        let v2 = token env v2 (* pattern [rR][oO][wW] *) in
        todo env (v1, v2)
    )
  in
  let v3 =
    (match v3 with
    | Some (v1, v2) ->
        let v1 =
          token env v1 (* pattern [eE][xX][cC][lL][uU][dD][eE] *)
        in
        let v2 =
          (match v2 with
          | `Not_others (v1, v2) ->
              let v1 = token env v1 (* pattern [nN][oO][tT] *) in
              let v2 =
                token env v2 (* pattern [oO][tT][hH][eE][rR][sS] *)
              in
              todo env (v1, v2)
          | `Curr_row (v1, v2) ->
              let v1 =
                token env v1 (* pattern [cC][uU][rR][rR][eE][nN][tT] *)
              in
              let v2 = token env v2 (* pattern [rR][oO][wW] *) in
              todo env (v1, v2)
          | `Group tok ->
              token env tok (* pattern [gG][rR][oO][uU][pP] *)
          | `Ties tok -> token env tok (* pattern [tT][iI][eE][sS] *)
          )
        in
        todo env (v1, v2)
    | None -> todo env ())
  in
  todo env (v1, v2, v3)

and map_from_clause (env : env) ((v1, v2) : CST.from_clause) =
  let v1 = token env v1 (* pattern [fF][rR][oO][mM] *) in
  let v2 = map_join_clause env v2 in
  todo env (v1, v2)

and map_group_by_clause (env : env) ((v1, v2, v3, v4, v5) : CST.group_by_clause) =
  let v1 = token env v1 (* pattern [gG][rR][oO][uU][pP] *) in
  let v2 = token env v2 (* pattern [bB][yY] *) in
  let v3 = map_filename env v3 in
  let v4 =
    List.map (fun (v1, v2) ->
      let v1 = token env v1 (* "," *) in
      let v2 = map_filename env v2 in
      todo env (v1, v2)
    ) v4
  in
  let v5 =
    (match v5 with
    | Some (v1, v2) ->
        let v1 =
          token env v1 (* pattern [hH][aA][vV][iI][nN][gG] *)
        in
        let v2 = map_filename env v2 in
        todo env (v1, v2)
    | None -> todo env ())
  in
  todo env (v1, v2, v3, v4, v5)

and map_join_clause (env : env) ((v1, v2) : CST.join_clause) =
  let v1 = map_table_or_subquery env v1 in
  let v2 =
    List.map (fun (v1, v2, v3) ->
      let v1 = map_join_operator env v1 in
      let v2 = map_table_or_subquery env v2 in
      let v3 =
        (match v3 with
        | Some x -> map_join_constraint env x
        | None -> todo env ())
      in
      todo env (v1, v2, v3)
    ) v2
  in
  todo env (v1, v2)

and map_join_constraint (env : env) (x : CST.join_constraint) =
  (match x with
  | `On_expr (v1, v2) ->
      let v1 = token env v1 (* pattern [oO][nN] *) in
      let v2 = map_filename env v2 in
      todo env (v1, v2)
  | `Using_LPAR_name_rep_COMMA_name_RPAR (v1, v2, v3, v4, v5) ->
      let v1 = token env v1 (* pattern [uU][sS][iI][nN][gG] *) in
      let v2 = token env v2 (* "(" *) in
      let v3 = map_error_message env v3 in
      let v4 =
        List.map (fun (v1, v2) ->
          let v1 = token env v1 (* "," *) in
          let v2 = map_error_message env v2 in
          todo env (v1, v2)
        ) v4
      in
      let v5 = token env v5 (* ")" *) in
      todo env (v1, v2, v3, v4, v5)
  )

and map_limit_clause (env : env) ((v1, v2, v3) : CST.limit_clause) =
  let v1 = token env v1 (* pattern [lL][iI][mM][iI][tT] *) in
  let v2 = map_filename env v2 in
  let v3 =
    (match v3 with
    | Some x ->
        (match x with
        | `Offset_expr (v1, v2) ->
            let v1 =
              token env v1 (* pattern [oO][fF][fF][sS][eE][tT] *)
            in
            let v2 = map_filename env v2 in
            todo env (v1, v2)
        | `COMMA_expr (v1, v2) ->
            let v1 = token env v1 (* "," *) in
            let v2 = map_filename env v2 in
            todo env (v1, v2)
        )
    | None -> todo env ())
  in
  todo env (v1, v2, v3)

and map_order_by_clause (env : env) ((v1, v2, v3, v4) : CST.order_by_clause) =
  let v1 = token env v1 (* pattern [oO][rR][dD][eE][rR] *) in
  let v2 = token env v2 (* pattern [bB][yY] *) in
  let v3 = map_ordering_term env v3 in
  let v4 =
    List.map (fun (v1, v2) ->
      let v1 = token env v1 (* "," *) in
      let v2 = map_ordering_term env v2 in
      todo env (v1, v2)
    ) v4
  in
  todo env (v1, v2, v3, v4)

and map_ordering_term (env : env) ((v1, v2, v3) : CST.ordering_term) =
  let v1 = map_filename env v1 in
  let v2 =
    (match v2 with
    | Some x -> map_anon_choice_asc_fa4fd8b env x
    | None -> todo env ())
  in
  let v3 =
    (match v3 with
    | Some (v1, v2) ->
        let v1 = token env v1 (* pattern [nN][uU][lL][lL][sS] *) in
        let v2 =
          (match v2 with
          | `First tok ->
              token env tok (* pattern [fF][iI][rR][sS][tT] *)
          | `Last tok -> token env tok (* pattern [lL][aA][sS][tT] *)
          )
        in
        todo env (v1, v2)
    | None -> todo env ())
  in
  todo env (v1, v2, v3)

and map_over_clause (env : env) ((v1, v2) : CST.over_clause) =
  let v1 = token env v1 (* pattern [oO][vV][eE][rR] *) in
  let v2 =
    (match v2 with
    | `Name x -> map_error_message env x
    | `LPAR_opt_name_opt_part_by_expr_rep_COMMA_expr_opt_order_by_orde_term_rep_COMMA_orde_term_opt_frame_spec_RPAR x ->
        map_window_defn env x
    )
  in
  todo env (v1, v2)

and map_result_column (env : env) (x : CST.result_column) =
  (match x with
  | `Name_DOT_STAR (v1, v2, v3) ->
      let v1 = map_error_message env v1 in
      let v2 = token env v2 (* "." *) in
      let v3 = token env v3 (* "*" *) in
      todo env (v1, v2, v3)
  | `STAR tok -> token env tok (* "*" *)
  | `Expr_opt_opt_as_name (v1, v2) ->
      let v1 = map_filename env v1 in
      let v2 = map_anon_opt_opt_as_error_mess_6e99511 env v2 in
      todo env (v1, v2)
  )

and map_select_core (env : env) (x : CST.select_core) =
  (match x with
  | `Select_opt_choice_dist_result_column_rep_COMMA_result_column_opt_from_clause_opt_where_clause_opt_group_by_clause_opt_window_clause (v1, v2, v3, v4, v5, v6, v7, v8) ->
      let v1 =
        token env v1 (* pattern [sS][eE][lL][eE][cC][tT] *)
      in
      let v2 =
        (match v2 with
        | Some x ->
            (match x with
            | `Dist tok ->
                token env tok (* pattern [dD][iI][sS][tT][iI][nN][cC][tT] *)
            | `All tok -> token env tok (* pattern [aA][lL][lL] *)
            )
        | None -> todo env ())
      in
      let v3 = map_result_column env v3 in
      let v4 =
        List.map (fun (v1, v2) ->
          let v1 = token env v1 (* "," *) in
          let v2 = map_result_column env v2 in
          todo env (v1, v2)
        ) v4
      in
      let v5 =
        (match v5 with
        | Some x -> map_from_clause env x
        | None -> todo env ())
      in
      let v6 =
        (match v6 with
        | Some x -> map_where_clause env x
        | None -> todo env ())
      in
      let v7 =
        (match v7 with
        | Some x -> map_group_by_clause env x
        | None -> todo env ())
      in
      let v8 =
        (match v8 with
        | Some x -> map_window_clause env x
        | None -> todo env ())
      in
      todo env (v1, v2, v3, v4, v5, v6, v7, v8)
  | `Values_LPAR_expr_rep_COMMA_expr_RPAR_rep_COMMA_LPAR_expr_rep_COMMA_expr_RPAR (v1, v2, v3, v4, v5, v6) ->
      let v1 =
        token env v1 (* pattern [vV][aA][lL][uU][eE][sS] *)
      in
      let v2 = token env v2 (* "(" *) in
      let v3 = map_filename env v3 in
      let v4 =
        List.map (fun (v1, v2) ->
          let v1 = token env v1 (* "," *) in
          let v2 = map_filename env v2 in
          todo env (v1, v2)
        ) v4
      in
      let v5 = token env v5 (* ")" *) in
      let v6 =
        List.map (map_anon_COMMA_LPAR_file_rep_COMMA_file_RPAR_a378f47 env) v6
      in
      todo env (v1, v2, v3, v4, v5, v6)
  )

and map_select_stmt (env : env) (x : CST.select_stmt) =
  (match x with
  | `Rectype (v1, v2, v3, v4, v5) ->
      let v1 =
        (match v1 with
        | Some x -> map_with_clause env x
        | None -> todo env ())
      in
      let v2 = map_select_core env v2 in
      let v3 =
        List.map (fun (v1, v2) ->
          let v1 = map_compound_operator env v1 in
          let v2 = map_select_core env v2 in
          todo env (v1, v2)
        ) v3
      in
      let v4 =
        (match v4 with
        | Some x -> map_order_by_clause env x
        | None -> todo env ())
      in
      let v5 =
        (match v5 with
        | Some x -> map_limit_clause env x
        | None -> todo env ())
      in
      todo env (v1, v2, v3, v4, v5)
  )

and map_table_or_subquery (env : env) (x : CST.table_or_subquery) =
  (match x with
  | `Name2_opt_opt_as_name_opt_choice_inde_by_name (v1, v2, v3) ->
      let v1 = map_name2 env v1 in
      let v2 = map_anon_opt_opt_as_error_mess_6e99511 env v2 in
      let v3 =
        (match v3 with
        | Some x -> map_anon_choice_inde_by_error_mess_0500888 env x
        | None -> todo env ())
      in
      todo env (v1, v2, v3)
  | `Name2_LPAR_expr_rep_COMMA_expr_RPAR_opt_opt_as_name (v1, v2, v3, v4, v5, v6) ->
      let v1 = map_name2 env v1 in
      let v2 = token env v2 (* "(" *) in
      let v3 = map_filename env v3 in
      let v4 =
        List.map (fun (v1, v2) ->
          let v1 = token env v1 (* "," *) in
          let v2 = map_filename env v2 in
          todo env (v1, v2)
        ) v4
      in
      let v5 = token env v5 (* ")" *) in
      let v6 = map_anon_opt_opt_as_error_mess_6e99511 env v6 in
      todo env (v1, v2, v3, v4, v5, v6)
  | `LPAR_select_stmt_RPAR_opt_opt_as_name (v1, v2, v3, v4) ->
      let v1 = token env v1 (* "(" *) in
      let v2 = map_select_stmt env v2 in
      let v3 = token env v3 (* ")" *) in
      let v4 = map_anon_opt_opt_as_error_mess_6e99511 env v4 in
      todo env (v1, v2, v3, v4)
  | `LPAR_join_clause_RPAR (v1, v2, v3) ->
      let v1 = token env v1 (* "(" *) in
      let v2 = map_join_clause env v2 in
      let v3 = token env v3 (* ")" *) in
      todo env (v1, v2, v3)
  )

and map_where_clause (env : env) ((v1, v2) : CST.where_clause) =
  let v1 = token env v1 (* pattern [wW][hH][eE][rR][eE] *) in
  let v2 = map_filename env v2 in
  todo env (v1, v2)

and map_window_clause (env : env) ((v1, v2, v3, v4, v5) : CST.window_clause) =
  let v1 =
    token env v1 (* pattern [wW][iI][nN][dD][oO][wW] *)
  in
  let v2 = map_error_message env v2 in
  let v3 = token env v3 (* pattern [aA][sS] *) in
  let v4 = map_window_defn env v4 in
  let v5 =
    List.map (fun (v1, v2, v3, v4) ->
      let v1 = token env v1 (* "," *) in
      let v2 = map_error_message env v2 in
      let v3 = token env v3 (* pattern [aA][sS] *) in
      let v4 = map_window_defn env v4 in
      todo env (v1, v2, v3, v4)
    ) v5
  in
  todo env (v1, v2, v3, v4, v5)

and map_window_defn (env : env) ((v1, v2, v3, v4, v5, v6) : CST.window_defn) =
  let v1 = token env v1 (* "(" *) in
  let v2 =
    (match v2 with
    | Some x -> map_error_message env x
    | None -> todo env ())
  in
  let v3 =
    (match v3 with
    | Some (v1, v2, v3, v4) ->
        let v1 =
          token env v1 (* pattern [pP][aA][rR][tT][iI][tT][iI][oO][nN] *)
        in
        let v2 = token env v2 (* pattern [bB][yY] *) in
        let v3 = map_filename env v3 in
        let v4 =
          List.map (fun (v1, v2) ->
            let v1 = token env v1 (* "," *) in
            let v2 = map_filename env v2 in
            todo env (v1, v2)
          ) v4
        in
        todo env (v1, v2, v3, v4)
    | None -> todo env ())
  in
  let v4 =
    (match v4 with
    | Some x -> map_order_by_clause env x
    | None -> todo env ())
  in
  let v5 =
    (match v5 with
    | Some x -> map_frame_spec env x
    | None -> todo env ())
  in
  let v6 = token env v6 (* ")" *) in
  todo env (v1, v2, v3, v4, v5, v6)

and map_with_clause (env : env) (x : CST.with_clause) =
  (match x with
  | `Rectype (v1, v2, v3, v4) ->
      let v1 = token env v1 (* pattern [wW][iI][tT][hH] *) in
      let v2 =
        (match v2 with
        | Some tok ->
            token env tok (* pattern [rR][eE][cC][uU][rR][sS][iI][vV][eE] *)
        | None -> todo env ())
      in
      let v3 = map_common_table_expression env v3 in
      let v4 =
        List.map (fun (v1, v2) ->
          let v1 = token env v1 (* "," *) in
          let v2 = map_common_table_expression env v2 in
          todo env (v1, v2)
        ) v4
      in
      todo env (v1, v2, v3, v4)
  )

let map_foreign_key_clause (env : env) ((v1, v2, v3, v4, v5) : CST.foreign_key_clause) =
  let v1 =
    token env v1 (* pattern [rR][eE][fF][eE][rR][eE][nN][cC][eE][sS] *)
  in
  let v2 = map_error_message env v2 in
  let v3 =
    (match v3 with
    | Some x -> map_column_name_list env x
    | None -> todo env ())
  in
  let v4 =
    List.map (fun x ->
      (match x with
      | `On_choice_delete_choice_set_null (v1, v2, v3) ->
          let v1 = token env v1 (* pattern [oO][nN] *) in
          let v2 =
            (match v2 with
            | `Delete tok ->
                token env tok (* pattern [dD][eE][lL][eE][tT][eE] *)
            | `Update tok ->
                token env tok (* pattern [uU][pP][dD][aA][tT][eE] *)
            )
          in
          let v3 =
            (match v3 with
            | `Set_null (v1, v2) ->
                let v1 = token env v1 (* pattern [sS][eE][tT] *) in
                let v2 = token env v2 (* pattern [nN][uU][lL][lL] *) in
                todo env (v1, v2)
            | `Set_defa (v1, v2) ->
                let v1 = token env v1 (* pattern [sS][eE][tT] *) in
                let v2 =
                  token env v2 (* pattern [dD][eE][fF][aA][uU][lL][tT] *)
                in
                todo env (v1, v2)
            | `Casc tok ->
                token env tok (* pattern [cC][aA][sS][cC][aA][dD][eE] *)
            | `Rest tok ->
                token env tok (* pattern [rR][eE][sS][tT][rR][iI][cC][tT] *)
            | `No_action (v1, v2) ->
                let v1 = token env v1 (* pattern [nN][oO] *) in
                let v2 =
                  token env v2 (* pattern [aA][cC][tT][iI][oO][nN] *)
                in
                todo env (v1, v2)
            )
          in
          todo env (v1, v2, v3)
      | `Match_name (v1, v2) ->
          let v1 = token env v1 (* pattern [mM][aA][tT][cC][hH] *) in
          let v2 = map_error_message env v2 in
          todo env (v1, v2)
      )
    ) v4
  in
  let v5 =
    (match v5 with
    | Some (v1, v2, v3) ->
        let v1 =
          (match v1 with
          | Some tok -> token env tok (* pattern [nN][oO][tT] *)
          | None -> todo env ())
        in
        let v2 =
          token env v2 (* pattern [dD][eE][fF][eE][rR][rR][aA][bB][lL][eE] *)
        in
        let v3 =
          (match v3 with
          | Some x ->
              (match x with
              | `Init_defe (v1, v2) ->
                  let v1 =
                    token env v1 (* pattern [iI][nN][iI][tT][iI][aA][lL][lL][yY] *)
                  in
                  let v2 =
                    token env v2 (* pattern [dD][eE][fF][eE][rR][rR][eE][dD] *)
                  in
                  todo env (v1, v2)
              | `Init_imme (v1, v2) ->
                  let v1 =
                    token env v1 (* pattern [iI][nN][iI][tT][iI][aA][lL][lL][yY] *)
                  in
                  let v2 =
                    token env v2 (* pattern [iI][mM][mM][eE][dD][iI][aA][tT][eE] *)
                  in
                  todo env (v1, v2)
              )
          | None -> todo env ())
        in
        todo env (v1, v2, v3)
    | None -> todo env ())
  in
  todo env (v1, v2, v3, v4, v5)

let map_indexed_column (env : env) ((v1, v2) : CST.indexed_column) =
  let v1 = map_filename env v1 in
  let v2 =
    (match v2 with
    | Some x -> map_anon_choice_asc_fa4fd8b env x
    | None -> todo env ())
  in
  todo env (v1, v2)

let map_attach_stmt (env : env) ((v1, v2, v3, v4, v5) : CST.attach_stmt) =
  let v1 =
    token env v1 (* pattern [aA][tT][tT][aA][cC][hH] *)
  in
  let v2 =
    (match v2 with
    | Some tok ->
        token env tok (* pattern [dD][aA][tT][aA][bB][aA][sS][eE] *)
    | None -> todo env ())
  in
  let v3 = map_filename env v3 in
  let v4 = token env v4 (* pattern [aA][sS] *) in
  let v5 = map_error_message env v5 in
  todo env (v1, v2, v3, v4, v5)

let map_vacuum_stmt (env : env) ((v1, v2, v3) : CST.vacuum_stmt) =
  let v1 =
    token env v1 (* pattern [vV][aA][cC][uU][uU][mM] *)
  in
  let v2 =
    (match v2 with
    | Some x -> map_error_message env x
    | None -> todo env ())
  in
  let v3 =
    (match v3 with
    | Some (v1, v2) ->
        let v1 = token env v1 (* pattern [iI][nN][tT][oO] *) in
        let v2 = map_filename env v2 in
        todo env (v1, v2)
    | None -> todo env ())
  in
  todo env (v1, v2, v3)

let map_returning_clause (env : env) ((v1, v2, v3) : CST.returning_clause) =
  let v1 =
    token env v1 (* pattern [rR][eE][tT][uU][rR][nN][iI][nN][gG] *)
  in
  let v2 = map_result_column env v2 in
  let v3 =
    List.map (fun (v1, v2) ->
      let v1 = token env v1 (* "," *) in
      let v2 = map_result_column env v2 in
      todo env (v1, v2)
    ) v3
  in
  todo env (v1, v2, v3)

let map_create_view_stmt (env : env) ((v1, v2, v3, v4, v5, v6, v7, v8) : CST.create_view_stmt) =
  let v1 =
    token env v1 (* pattern [cC][rR][eE][aA][tT][eE] *)
  in
  let v2 =
    (match v2 with
    | Some x -> map_anon_choice_temp_716f4ac env x
    | None -> todo env ())
  in
  let v3 = token env v3 (* pattern [vV][iI][eE][wW] *) in
  let v4 =
    (match v4 with
    | Some (v1, v2, v3) ->
        let v1 = token env v1 (* pattern [iI][fF] *) in
        let v2 = token env v2 (* pattern [nN][oO][tT] *) in
        let v3 =
          token env v3 (* pattern [eE][xX][iI][sS][tT][sS] *)
        in
        todo env (v1, v2, v3)
    | None -> todo env ())
  in
  let v5 = map_name2 env v5 in
  let v6 =
    (match v6 with
    | Some x -> map_column_name_list env x
    | None -> todo env ())
  in
  let v7 = token env v7 (* pattern [aA][sS] *) in
  let v8 = map_select_stmt env v8 in
  todo env (v1, v2, v3, v4, v5, v6, v7, v8)

let map_column_constraint (env : env) ((v1, v2) : CST.column_constraint) =
  let v1 =
    (match v1 with
    | Some (v1, v2) ->
        let v1 =
          token env v1 (* pattern [cC][oO][nN][sS][tT][rR][aA][iI][nN][tT] *)
        in
        let v2 = map_error_message env v2 in
        todo env (v1, v2)
    | None -> todo env ())
  in
  let v2 =
    (match v2 with
    | `Prim_key_opt_choice_asc_opt_conf_clause_opt_auto (v1, v2, v3, v4, v5) ->
        let v1 =
          token env v1 (* pattern [pP][rR][iI][mM][aA][rR][yY] *)
        in
        let v2 = token env v2 (* pattern [kK][eE][yY] *) in
        let v3 =
          (match v3 with
          | Some x -> map_anon_choice_asc_fa4fd8b env x
          | None -> todo env ())
        in
        let v4 =
          (match v4 with
          | Some x -> map_conflict_clause env x
          | None -> todo env ())
        in
        let v5 =
          (match v5 with
          | Some tok ->
              token env tok (* pattern [aA][uU][tT][oO][iI][nN][cC][rR][eE][mM][eE][nN][tT] *)
          | None -> todo env ())
        in
        todo env (v1, v2, v3, v4, v5)
    | `Opt_not_null_opt_conf_clause (v1, v2, v3) ->
        let v1 =
          (match v1 with
          | Some tok -> token env tok (* pattern [nN][oO][tT] *)
          | None -> todo env ())
        in
        let v2 = token env v2 (* pattern [nN][uU][lL][lL] *) in
        let v3 =
          (match v3 with
          | Some x -> map_conflict_clause env x
          | None -> todo env ())
        in
        todo env (v1, v2, v3)
    | `Unique_opt_conf_clause (v1, v2) ->
        let v1 =
          token env v1 (* pattern [uU][nN][iI][qQ][uU][eE] *)
        in
        let v2 =
          (match v2 with
          | Some x -> map_conflict_clause env x
          | None -> todo env ())
        in
        todo env (v1, v2)
    | `Check_LPAR_expr_RPAR (v1, v2, v3, v4) ->
        let v1 = token env v1 (* pattern [cC][hH][eE][cC][kK] *) in
        let v2 = token env v2 (* "(" *) in
        let v3 = map_filename env v3 in
        let v4 = token env v4 (* ")" *) in
        todo env (v1, v2, v3, v4)
    | `Defa_choice_LPAR_expr_RPAR (v1, v2) ->
        let v1 =
          token env v1 (* pattern [dD][eE][fF][aA][uU][lL][tT] *)
        in
        let v2 =
          (match v2 with
          | `LPAR_expr_RPAR (v1, v2, v3) ->
              let v1 = token env v1 (* "(" *) in
              let v2 = map_filename env v2 in
              let v3 = token env v3 (* ")" *) in
              todo env (v1, v2, v3)
          | `Lit_value x -> map_literal_value env x
          | `Signed_num x -> map_signed_number env x
          )
        in
        todo env (v1, v2)
    | `Coll_coll_name (v1, v2) ->
        let v1 =
          token env v1 (* pattern [cC][oO][lL][lL][aA][tT][eE] *)
        in
        let v2 = map_collation_name env v2 in
        todo env (v1, v2)
    | `Fore_key_clause x -> map_foreign_key_clause env x
    | `Opt_gene_always_as_LPAR_expr_RPAR_opt_choice_stored (v1, v2, v3, v4, v5, v6) ->
        let v1 =
          (match v1 with
          | Some (v1, v2) ->
              let v1 =
                token env v1 (* pattern [gG][eE][nN][eE][rR][aA][tT][eE][dD] *)
              in
              let v2 =
                token env v2 (* pattern [aA][lL][wW][aA][yY][sS] *)
              in
              todo env (v1, v2)
          | None -> todo env ())
        in
        let v2 = token env v2 (* pattern [aA][sS] *) in
        let v3 = token env v3 (* "(" *) in
        let v4 = map_filename env v4 in
        let v5 = token env v5 (* ")" *) in
        let v6 =
          (match v6 with
          | Some x ->
              (match x with
              | `Stored tok ->
                  token env tok (* pattern [sS][tT][oO][rR][eE][dD] *)
              | `Virt tok ->
                  token env tok (* pattern [vV][iI][rR][tT][uU][aA][lL] *)
              )
          | None -> todo env ())
        in
        todo env (v1, v2, v3, v4, v5, v6)
    )
  in
  todo env (v1, v2)

let map_create_index_stmt (env : env) ((v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12) : CST.create_index_stmt) =
  let v1 =
    token env v1 (* pattern [cC][rR][eE][aA][tT][eE] *)
  in
  let v2 =
    (match v2 with
    | Some tok ->
        token env tok (* pattern [uU][nN][iI][qQ][uU][eE] *)
    | None -> todo env ())
  in
  let v3 = token env v3 (* pattern [iI][nN][dD][eE][xX] *) in
  let v4 =
    (match v4 with
    | Some (v1, v2, v3) ->
        let v1 = token env v1 (* pattern [iI][fF] *) in
        let v2 = token env v2 (* pattern [nN][oO][tT] *) in
        let v3 =
          token env v3 (* pattern [eE][xX][iI][sS][tT][sS] *)
        in
        todo env (v1, v2, v3)
    | None -> todo env ())
  in
  let v5 = map_name2 env v5 in
  let v6 = token env v6 (* pattern [oO][nN] *) in
  let v7 = map_error_message env v7 in
  let v8 = token env v8 (* "(" *) in
  let v9 = map_indexed_column env v9 in
  let v10 =
    List.map (fun (v1, v2) ->
      let v1 = token env v1 (* "," *) in
      let v2 = map_indexed_column env v2 in
      todo env (v1, v2)
    ) v10
  in
  let v11 = token env v11 (* ")" *) in
  let v12 =
    (match v12 with
    | Some x -> map_where_clause env x
    | None -> todo env ())
  in
  todo env (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12)

let map_table_constraint (env : env) ((v1, v2) : CST.table_constraint) =
  let v1 =
    (match v1 with
    | Some (v1, v2) ->
        let v1 =
          token env v1 (* pattern [cC][oO][nN][sS][tT][rR][aA][iI][nN][tT] *)
        in
        let v2 = map_error_message env v2 in
        todo env (v1, v2)
    | None -> todo env ())
  in
  let v2 =
    (match v2 with
    | `Choice_prim_key_LPAR_inde_column_rep_COMMA_inde_column_RPAR_opt_conf_clause (v1, v2, v3, v4, v5, v6) ->
        let v1 =
          (match v1 with
          | `Prim_key (v1, v2) ->
              let v1 =
                token env v1 (* pattern [pP][rR][iI][mM][aA][rR][yY] *)
              in
              let v2 = token env v2 (* pattern [kK][eE][yY] *) in
              todo env (v1, v2)
          | `Unique tok ->
              token env tok (* pattern [uU][nN][iI][qQ][uU][eE] *)
          )
        in
        let v2 = token env v2 (* "(" *) in
        let v3 = map_indexed_column env v3 in
        let v4 =
          List.map (fun (v1, v2) ->
            let v1 = token env v1 (* "," *) in
            let v2 = map_indexed_column env v2 in
            todo env (v1, v2)
          ) v4
        in
        let v5 = token env v5 (* ")" *) in
        let v6 =
          (match v6 with
          | Some x -> map_conflict_clause env x
          | None -> todo env ())
        in
        todo env (v1, v2, v3, v4, v5, v6)
    | `Check_LPAR_expr_RPAR (v1, v2, v3, v4) ->
        let v1 = token env v1 (* pattern [cC][hH][eE][cC][kK] *) in
        let v2 = token env v2 (* "(" *) in
        let v3 = map_filename env v3 in
        let v4 = token env v4 (* ")" *) in
        todo env (v1, v2, v3, v4)
    | `Fore_key_LPAR_name_rep_COMMA_name_RPAR_fore_key_clause (v1, v2, v3, v4, v5, v6, v7) ->
        let v1 =
          token env v1 (* pattern [fF][oO][rR][eE][iI][gG][nN] *)
        in
        let v2 = token env v2 (* pattern [kK][eE][yY] *) in
        let v3 = token env v3 (* "(" *) in
        let v4 = map_error_message env v4 in
        let v5 =
          List.map (fun (v1, v2) ->
            let v1 = token env v1 (* "," *) in
            let v2 = map_error_message env v2 in
            todo env (v1, v2)
          ) v5
        in
        let v6 = token env v6 (* ")" *) in
        let v7 = map_foreign_key_clause env v7 in
        todo env (v1, v2, v3, v4, v5, v6, v7)
    )
  in
  todo env (v1, v2)

let map_upsert_clause (env : env) ((v1, v2, v3, v4, v5) : CST.upsert_clause) =
  let v1 = token env v1 (* pattern [oO][nN] *) in
  let v2 =
    token env v2 (* pattern [cC][oO][nN][fF][lL][iI][cC][tT] *)
  in
  let v3 =
    (match v3 with
    | Some (v1, v2, v3, v4, v5) ->
        let v1 = token env v1 (* "(" *) in
        let v2 = map_indexed_column env v2 in
        let v3 =
          List.map (fun (v1, v2) ->
            let v1 = token env v1 (* "," *) in
            let v2 = map_indexed_column env v2 in
            todo env (v1, v2)
          ) v3
        in
        let v4 = token env v4 (* ")" *) in
        let v5 =
          (match v5 with
          | Some x -> map_where_clause env x
          | None -> todo env ())
        in
        todo env (v1, v2, v3, v4, v5)
    | None -> todo env ())
  in
  let v4 = token env v4 (* pattern [dD][oO] *) in
  let v5 =
    (match v5 with
    | `Noth tok ->
        token env tok (* pattern [nN][oO][tT][hH][iI][nN][gG] *)
    | `Update_set_choice_name_EQ_expr_rep_COMMA_choice_name_EQ_expr_opt_where_clause (v1, v2, v3, v4, v5, v6, v7) ->
        let v1 =
          token env v1 (* pattern [uU][pP][dD][aA][tT][eE] *)
        in
        let v2 = token env v2 (* pattern [sS][eE][tT] *) in
        let v3 = map_anon_choice_error_mess_facbc16 env v3 in
        let v4 = token env v4 (* "=" *) in
        let v5 = map_filename env v5 in
        let v6 =
          List.map (fun (v1, v2, v3, v4) ->
            let v1 = token env v1 (* "," *) in
            let v2 = map_anon_choice_error_mess_facbc16 env v2 in
            let v3 = token env v3 (* "=" *) in
            let v4 = map_filename env v4 in
            todo env (v1, v2, v3, v4)
          ) v6
        in
        let v7 =
          (match v7 with
          | Some x -> map_where_clause env x
          | None -> todo env ())
        in
        todo env (v1, v2, v3, v4, v5, v6, v7)
    )
  in
  todo env (v1, v2, v3, v4, v5)

let map_update_stmt (env : env) ((v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14) : CST.update_stmt) =
  let v1 =
    (match v1 with
    | Some x -> map_with_clause env x
    | None -> todo env ())
  in
  let v2 =
    token env v2 (* pattern [uU][pP][dD][aA][tT][eE] *)
  in
  let v3 =
    (match v3 with
    | Some (v1, v2) ->
        let v1 = token env v1 (* pattern [oO][rR] *) in
        let v2 = map_anon_choice_abort_500a898 env v2 in
        todo env (v1, v2)
    | None -> todo env ())
  in
  let v4 = map_qualified_table_name env v4 in
  let v5 = token env v5 (* pattern [sS][eE][tT] *) in
  let v6 = map_anon_choice_error_mess_facbc16 env v6 in
  let v7 = token env v7 (* "=" *) in
  let v8 = map_filename env v8 in
  let v9 =
    List.map (fun (v1, v2, v3, v4) ->
      let v1 = token env v1 (* "," *) in
      let v2 = map_anon_choice_error_mess_facbc16 env v2 in
      let v3 = token env v3 (* "=" *) in
      let v4 = map_filename env v4 in
      todo env (v1, v2, v3, v4)
    ) v9
  in
  let v10 =
    (match v10 with
    | Some x -> map_from_clause env x
    | None -> todo env ())
  in
  let v11 =
    (match v11 with
    | Some x -> map_where_clause env x
    | None -> todo env ())
  in
  let v12 =
    (match v12 with
    | Some x -> map_returning_clause env x
    | None -> todo env ())
  in
  let v13 =
    (match v13 with
    | Some x -> map_order_by_clause env x
    | None -> todo env ())
  in
  let v14 =
    (match v14 with
    | Some x -> map_limit_clause env x
    | None -> todo env ())
  in
  todo env (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14)

let map_delete_stmt (env : env) ((v1, v2, v3, v4, v5, v6, v7, v8) : CST.delete_stmt) =
  let v1 =
    (match v1 with
    | Some x -> map_with_clause env x
    | None -> todo env ())
  in
  let v2 =
    token env v2 (* pattern [dD][eE][lL][eE][tT][eE] *)
  in
  let v3 = token env v3 (* pattern [fF][rR][oO][mM] *) in
  let v4 = map_qualified_table_name env v4 in
  let v5 =
    (match v5 with
    | Some x -> map_where_clause env x
    | None -> todo env ())
  in
  let v6 =
    (match v6 with
    | Some x -> map_returning_clause env x
    | None -> todo env ())
  in
  let v7 =
    (match v7 with
    | Some x -> map_order_by_clause env x
    | None -> todo env ())
  in
  let v8 =
    (match v8 with
    | Some x -> map_limit_clause env x
    | None -> todo env ())
  in
  todo env (v1, v2, v3, v4, v5, v6, v7, v8)

let map_column_def (env : env) ((v1, v2, v3) : CST.column_def) =
  let v1 = map_error_message env v1 in
  let v2 =
    (match v2 with
    | Some x -> map_type_name env x
    | None -> todo env ())
  in
  let v3 = List.map (map_column_constraint env) v3 in
  todo env (v1, v2, v3)

let map_insert_stmt (env : env) ((v1, v2, v3, v4, v5, v6, v7, v8) : CST.insert_stmt) =
  let v1 =
    (match v1 with
    | Some x -> map_with_clause env x
    | None -> todo env ())
  in
  let v2 =
    (match v2 with
    | `Repl tok ->
        token env tok (* pattern [rR][eE][pP][lL][aA][cC][eE] *)
    | `Insert_opt_or_choice_abort (v1, v2) ->
        let v1 =
          token env v1 (* pattern [iI][nN][sS][eE][rR][tT] *)
        in
        let v2 =
          (match v2 with
          | Some (v1, v2) ->
              let v1 = token env v1 (* pattern [oO][rR] *) in
              let v2 = map_anon_choice_abort_500a898 env v2 in
              todo env (v1, v2)
          | None -> todo env ())
        in
        todo env (v1, v2)
    )
  in
  let v3 = token env v3 (* pattern [iI][nN][tT][oO] *) in
  let v4 = map_name2 env v4 in
  let v5 =
    (match v5 with
    | Some (v1, v2) ->
        let v1 = token env v1 (* pattern [aA][sS] *) in
        let v2 = map_error_message env v2 in
        todo env (v1, v2)
    | None -> todo env ())
  in
  let v6 =
    (match v6 with
    | Some x -> map_column_name_list env x
    | None -> todo env ())
  in
  let v7 =
    (match v7 with
    | `Values_LPAR_expr_rep_COMMA_expr_RPAR_rep_COMMA_LPAR_expr_rep_COMMA_expr_RPAR_opt_upsert_clause (v1, v2, v3, v4, v5, v6, v7) ->
        let v1 =
          token env v1 (* pattern [vV][aA][lL][uU][eE][sS] *)
        in
        let v2 = token env v2 (* "(" *) in
        let v3 = map_filename env v3 in
        let v4 =
          List.map (fun (v1, v2) ->
            let v1 = token env v1 (* "," *) in
            let v2 = map_filename env v2 in
            todo env (v1, v2)
          ) v4
        in
        let v5 = token env v5 (* ")" *) in
        let v6 =
          List.map (map_anon_COMMA_LPAR_file_rep_COMMA_file_RPAR_a378f47 env) v6
        in
        let v7 =
          (match v7 with
          | Some x -> map_upsert_clause env x
          | None -> todo env ())
        in
        todo env (v1, v2, v3, v4, v5, v6, v7)
    | `Select_stmt_opt_upsert_clause (v1, v2) ->
        let v1 = map_select_stmt env v1 in
        let v2 =
          (match v2 with
          | Some x -> map_upsert_clause env x
          | None -> todo env ())
        in
        todo env (v1, v2)
    | `Defa_values (v1, v2) ->
        let v1 =
          token env v1 (* pattern [dD][eE][fF][aA][uU][lL][tT] *)
        in
        let v2 =
          token env v2 (* pattern [vV][aA][lL][uU][eE][sS] *)
        in
        todo env (v1, v2)
    )
  in
  let v8 =
    (match v8 with
    | Some x -> map_returning_clause env x
    | None -> todo env ())
  in
  todo env (v1, v2, v3, v4, v5, v6, v7, v8)

let map_create_virtual_table_stmt (env : env) ((v1, v2, v3, v4, v5, v6, v7, v8) : CST.create_virtual_table_stmt) =
  let v1 =
    token env v1 (* pattern [cC][rR][eE][aA][tT][eE] *)
  in
  let v2 =
    token env v2 (* pattern [vV][iI][rR][tT][uU][aA][lL] *)
  in
  let v3 = token env v3 (* pattern [tT][aA][bB][lL][eE] *) in
  let v4 =
    (match v4 with
    | Some (v1, v2, v3) ->
        let v1 = token env v1 (* pattern [iI][fF] *) in
        let v2 = token env v2 (* pattern [nN][oO][tT] *) in
        let v3 =
          token env v3 (* pattern [eE][xX][iI][sS][tT][sS] *)
        in
        todo env (v1, v2, v3)
    | None -> todo env ())
  in
  let v5 = map_name2 env v5 in
  let v6 = token env v6 (* pattern [uU][sS][iI][nN][gG] *) in
  let v7 = map_error_message env v7 in
  let v8 =
    (match v8 with
    | Some (v1, v2, v3, v4, v5) ->
        let v1 = token env v1 (* "(" *) in
        let v2 = map_column_def env v2 in
        let v3 =
          List.map (fun (v1, v2) ->
            let v1 = token env v1 (* "," *) in
            let v2 = map_column_def env v2 in
            todo env (v1, v2)
          ) v3
        in
        let v4 =
          List.map (fun (v1, v2) ->
            let v1 = token env v1 (* "," *) in
            let v2 = map_table_constraint env v2 in
            todo env (v1, v2)
          ) v4
        in
        let v5 = token env v5 (* ")" *) in
        todo env (v1, v2, v3, v4, v5)
    | None -> todo env ())
  in
  todo env (v1, v2, v3, v4, v5, v6, v7, v8)

let map_create_table_stmt (env : env) ((v1, v2, v3, v4, v5, v6) : CST.create_table_stmt) =
  let v1 =
    token env v1 (* pattern [cC][rR][eE][aA][tT][eE] *)
  in
  let v2 =
    (match v2 with
    | Some x -> map_anon_choice_temp_716f4ac env x
    | None -> todo env ())
  in
  let v3 = token env v3 (* pattern [tT][aA][bB][lL][eE] *) in
  let v4 =
    (match v4 with
    | Some (v1, v2, v3) ->
        let v1 = token env v1 (* pattern [iI][fF] *) in
        let v2 = token env v2 (* pattern [nN][oO][tT] *) in
        let v3 =
          token env v3 (* pattern [eE][xX][iI][sS][tT][sS] *)
        in
        todo env (v1, v2, v3)
    | None -> todo env ())
  in
  let v5 = map_name2 env v5 in
  let v6 =
    (match v6 with
    | `As_select_stmt (v1, v2) ->
        let v1 = token env v1 (* pattern [aA][sS] *) in
        let v2 = map_select_stmt env v2 in
        todo env (v1, v2)
    | `LPAR_column_def_rep_COMMA_column_def_rep_COMMA_table_cons_RPAR_opt_with_rowid (v1, v2, v3, v4, v5, v6) ->
        let v1 = token env v1 (* "(" *) in
        let v2 = map_column_def env v2 in
        let v3 =
          List.map (fun (v1, v2) ->
            let v1 = token env v1 (* "," *) in
            let v2 = map_column_def env v2 in
            todo env (v1, v2)
          ) v3
        in
        let v4 =
          List.map (fun (v1, v2) ->
            let v1 = token env v1 (* "," *) in
            let v2 = map_table_constraint env v2 in
            todo env (v1, v2)
          ) v4
        in
        let v5 = token env v5 (* ")" *) in
        let v6 =
          (match v6 with
          | Some (v1, v2) ->
              let v1 =
                token env v1 (* pattern [wW][iI][tT][hH][oO][uU][tT] *)
              in
              let v2 = token env v2 (* pattern [rR][oO][wW][iI][dD] *) in
              todo env (v1, v2)
          | None -> todo env ())
        in
        todo env (v1, v2, v3, v4, v5, v6)
    )
  in
  todo env (v1, v2, v3, v4, v5, v6)

let map_alter_table_stmt (env : env) ((v1, v2, v3, v4) : CST.alter_table_stmt) =
  let v1 = token env v1 (* pattern [aA][lL][tT][eE][rR] *) in
  let v2 = token env v2 (* pattern [tT][aA][bB][lL][eE] *) in
  let v3 = map_name2 env v3 in
  let v4 =
    (match v4 with
    | `Rename_to_name (v1, v2, v3) ->
        let v1 =
          token env v1 (* pattern [rR][eE][nN][aA][mM][eE] *)
        in
        let v2 = token env v2 (* pattern [tT][oO] *) in
        let v3 = map_error_message env v3 in
        todo env (v1, v2, v3)
    | `Rename_opt_column_name_to_name (v1, v2, v3, v4, v5) ->
        let v1 =
          token env v1 (* pattern [rR][eE][nN][aA][mM][eE] *)
        in
        let v2 =
          (match v2 with
          | Some tok ->
              token env tok (* pattern [cC][oO][lL][uU][mM][nN] *)
          | None -> todo env ())
        in
        let v3 = map_error_message env v3 in
        let v4 = token env v4 (* pattern [tT][oO] *) in
        let v5 = map_error_message env v5 in
        todo env (v1, v2, v3, v4, v5)
    | `Add_opt_column_column_def (v1, v2, v3) ->
        let v1 = token env v1 (* pattern [aA][dD][dD] *) in
        let v2 =
          (match v2 with
          | Some tok ->
              token env tok (* pattern [cC][oO][lL][uU][mM][nN] *)
          | None -> todo env ())
        in
        let v3 = map_column_def env v3 in
        todo env (v1, v2, v3)
    | `Drop_opt_column_name (v1, v2, v3) ->
        let v1 = token env v1 (* pattern [dD][rR][oO][pP] *) in
        let v2 =
          (match v2 with
          | Some tok ->
              token env tok (* pattern [cC][oO][lL][uU][mM][nN] *)
          | None -> todo env ())
        in
        let v3 = map_error_message env v3 in
        todo env (v1, v2, v3)
    )
  in
  todo env (v1, v2, v3, v4)

let map_create_trigger_stmt (env : env) ((v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13) : CST.create_trigger_stmt) =
  let v1 =
    token env v1 (* pattern [cC][rR][eE][aA][tT][eE] *)
  in
  let v2 =
    (match v2 with
    | Some x -> map_anon_choice_temp_716f4ac env x
    | None -> todo env ())
  in
  let v3 =
    token env v3 (* pattern [tT][rR][iI][gG][gG][eE][rR] *)
  in
  let v4 =
    (match v4 with
    | Some (v1, v2, v3) ->
        let v1 = token env v1 (* pattern [iI][fF] *) in
        let v2 = token env v2 (* pattern [nN][oO][tT] *) in
        let v3 =
          token env v3 (* pattern [eE][xX][iI][sS][tT][sS] *)
        in
        todo env (v1, v2, v3)
    | None -> todo env ())
  in
  let v5 = map_name2 env v5 in
  let v6 =
    (match v6 with
    | Some x ->
        (match x with
        | `Before tok ->
            token env tok (* pattern [bB][eE][fF][oO][rR][eE] *)
        | `After tok ->
            token env tok (* pattern [aA][fF][tT][eE][rR] *)
        | `Inst_of (v1, v2) ->
            let v1 =
              token env v1 (* pattern [iI][nN][sS][tT][eE][aA][dD] *)
            in
            let v2 = token env v2 (* pattern [oO][fF] *) in
            todo env (v1, v2)
        )
    | None -> todo env ())
  in
  let v7 =
    (match v7 with
    | `Delete tok ->
        token env tok (* pattern [dD][eE][lL][eE][tT][eE] *)
    | `Insert tok ->
        token env tok (* pattern [iI][nN][sS][eE][rR][tT] *)
    | `Update_opt_of_name_rep_COMMA_name (v1, v2) ->
        let v1 =
          token env v1 (* pattern [uU][pP][dD][aA][tT][eE] *)
        in
        let v2 =
          (match v2 with
          | Some (v1, v2, v3) ->
              let v1 = token env v1 (* pattern [oO][fF] *) in
              let v2 = map_error_message env v2 in
              let v3 =
                List.map (fun (v1, v2) ->
                  let v1 = token env v1 (* "," *) in
                  let v2 = map_error_message env v2 in
                  todo env (v1, v2)
                ) v3
              in
              todo env (v1, v2, v3)
          | None -> todo env ())
        in
        todo env (v1, v2)
    )
  in
  let v8 = token env v8 (* pattern [oO][nN] *) in
  let v9 = map_error_message env v9 in
  let v10 =
    (match v10 with
    | Some x ->
        (match x with
        | `For_each_row_opt_when_expr (v1, v2, v3, v4) ->
            let v1 = token env v1 (* pattern [fF][oO][rR] *) in
            let v2 = token env v2 (* pattern [eE][aA][cC][hH] *) in
            let v3 = token env v3 (* pattern [rR][oO][wW] *) in
            let v4 =
              (match v4 with
              | Some (v1, v2) ->
                  let v1 = token env v1 (* pattern [wW][hH][eE][nN] *) in
                  let v2 = map_filename env v2 in
                  todo env (v1, v2)
              | None -> todo env ())
            in
            todo env (v1, v2, v3, v4)
        | `When_expr (v1, v2) ->
            let v1 = token env v1 (* pattern [wW][hH][eE][nN] *) in
            let v2 = map_filename env v2 in
            todo env (v1, v2)
        )
    | None -> todo env ())
  in
  let v11 =
    token env v11 (* pattern [bB][eE][gG][iI][nN] *)
  in
  let v12 =
    List.map (fun (v1, v2) ->
      let v1 =
        (match v1 with
        | `Update_stmt x -> map_update_stmt env x
        | `Insert_stmt x -> map_insert_stmt env x
        | `Delete_stmt x -> map_delete_stmt env x
        | `Select_stmt x -> map_select_stmt env x
        )
      in
      let v2 = token env v2 (* ";" *) in
      todo env (v1, v2)
    ) v12
  in
  let v13 = token env v13 (* pattern [eE][nN][dD] *) in
  todo env (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13)

let map_sql_stmt (env : env) ((v1, v2) : CST.sql_stmt) =
  let v1 =
    (match v1 with
    | Some (v1, v2) ->
        let v1 =
          token env v1 (* pattern [eE][xX][pP][lL][aA][iI][nN] *)
        in
        let v2 =
          (match v2 with
          | Some (v1, v2) ->
              let v1 = token env v1 (* pattern [qQ][uU][eE][rR][yY] *) in
              let v2 = token env v2 (* pattern [pP][lL][aA][nN] *) in
              todo env (v1, v2)
          | None -> todo env ())
        in
        todo env (v1, v2)
    | None -> todo env ())
  in
  let v2 =
    (match v2 with
    | `Alter_table_stmt x -> map_alter_table_stmt env x
    | `Anal_stmt x -> map_analyze_stmt env x
    | `Attach_stmt x -> map_attach_stmt env x
    | `Begin_stmt x -> map_begin_stmt env x
    | `Commit_stmt x -> map_commit_stmt env x
    | `Create_index_stmt x -> map_create_index_stmt env x
    | `Create_table_stmt x -> map_create_table_stmt env x
    | `Create_trig_stmt x -> map_create_trigger_stmt env x
    | `Create_view_stmt x -> map_create_view_stmt env x
    | `Create_virt_table_stmt x ->
        map_create_virtual_table_stmt env x
    | `Delete_stmt x -> map_delete_stmt env x
    | `Detach_stmt x -> map_detach_stmt env x
    | `Drop_index_stmt x -> map_drop_index_stmt env x
    | `Drop_table_stmt x -> map_drop_table_stmt env x
    | `Drop_trig_stmt x -> map_drop_trigger_stmt env x
    | `Drop_view_stmt x -> map_drop_view_stmt env x
    | `Insert_stmt x -> map_insert_stmt env x
    | `Pragma_stmt x -> map_pragma_stmt env x
    | `Rein_stmt x -> map_reindex_stmt env x
    | `Rele_stmt x -> map_release_stmt env x
    | `Roll_stmt x -> map_rollback_stmt env x
    | `Save_stmt x -> map_savepoint_stmt env x
    | `Select_stmt x -> map_select_stmt env x
    | `Update_stmt x -> map_update_stmt env x
    | `Vacuum_stmt x -> map_vacuum_stmt env x
    )
  in
  todo env (v1, v2)

let map_sql_stmt_list (env : env) ((v1, v2) : CST.sql_stmt_list) =
  let v1 =
    (match v1 with
    | Some x -> map_sql_stmt env x
    | None -> todo env ())
  in
  let v2 =
    List.map (fun (v1, v2) ->
      let v1 = token env v1 (* ";" *) in
      let v2 =
        (match v2 with
        | Some x -> map_sql_stmt env x
        | None -> todo env ())
      in
      todo env (v1, v2)
    ) v2
  in
  todo env (v1, v2)
