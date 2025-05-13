open G

exception Not_implemented 

let tmp_index = ref 0
let label_index = ref 1
let new_temp() = tmp_index := !tmp_index + 1; ".t" ^ (string_of_int !tmp_index)
let new_label() = label_index := !label_index + 1; !label_index

(*************************************)
(*          translation to T         *)
(*************************************)
let translate_decl : S.decl -> T.linstr list
  =fun decl ->
    match decl with
    | (S.TINT, x) -> [(0, T.COPYC (x, 0))]
    | (S.TARR n, x) -> [(0, T.ALLOC (x, n))]
 
let rec translate_decls : S.decls -> T.linstr list
  =fun decls ->
    match decls with
    | [] -> []
    | (typ, x)::rest -> 
      let rest' = translate_decls rest in
      let instr = translate_decl (typ, x) in
      instr @ rest'

let rec translate_exp : S.exp -> (T.linstr list * T.var)
  =fun exp ->
    match exp with
    | S.NUM n -> 
      let n' = new_temp() in
      ([(0, T.COPYC (n', n))], n')
    | LV ID id ->
      let id' = new_temp() in
      ([(0, T.COPY (id', id))], id')
    | LV ARR (id, e) ->
      let t = new_temp() in
      let (sub_instr, e') = translate_exp e in
      (sub_instr @ [(0, T.LOAD (t, (id, e')))], t)
    | ADD (e1, e2) ->
      let t = new_temp() in
      let (sub_instr1, e1') = translate_exp e1 in
      let (sub_instr2, e2') = translate_exp e2 in
      (sub_instr1 @ sub_instr2 @ [(0, T.ASSIGNV (t, T.ADD, e1', e2'))], t)
    | SUB (e1, e2) ->
      let t = new_temp() in
      let (sub_instr1, e1') = translate_exp e1 in
      let (sub_instr2, e2') = translate_exp e2 in
      (sub_instr1 @ sub_instr2 @ [(0, T.ASSIGNV (t, T.SUB, e1', e2'))], t)
    | MUL (e1, e2) ->
      let t = new_temp() in
      let (sub_instr1, e1') = translate_exp e1 in
      let (sub_instr2, e2') = translate_exp e2 in
      (sub_instr1 @ sub_instr2 @ [(0, T.ASSIGNV (t, T.MUL, e1', e2'))], t)
    | DIV (e1, e2) ->
      let t = new_temp() in
      let (sub_instr1, e1') = translate_exp e1 in
      let (sub_instr2, e2') = translate_exp e2 in
      (sub_instr1 @ sub_instr2 @ [(0, T.ASSIGNV (t, T.DIV, e1', e2'))], t)
    | MINUS e ->
      let t = new_temp() in
      let (sub_instr, e') = translate_exp e in
      (sub_instr @ [(0, T.ASSIGNU (t, T.MINUS, e'))], t)
    | NOT e ->
      let t = new_temp() in
      let (sub_instr, e') = translate_exp e in
      (sub_instr @ [(0, T.ASSIGNU (t, T.NOT, e'))], t)
    | LT (e1, e2) ->
      let t = new_temp() in
      let (sub_instr1, e1') = translate_exp e1 in
      let (sub_instr2, e2') = translate_exp e2 in
      (sub_instr1 @ sub_instr2 @ [(0, T.ASSIGNV (t, T.LT, e1', e2'))], t)
    | LE (e1, e2) ->
      let t = new_temp() in
      let (sub_instr1, e1') = translate_exp e1 in
      let (sub_instr2, e2') = translate_exp e2 in
      (sub_instr1 @ sub_instr2 @ [(0, T.ASSIGNV (t, T.LE, e1', e2'))], t)
    | GT (e1, e2) ->
      let t = new_temp() in
      let (sub_instr1, e1') = translate_exp e1 in
      let (sub_instr2, e2') = translate_exp e2 in
      (sub_instr1 @ sub_instr2 @ [(0, T.ASSIGNV (t, T.GT, e1', e2'))], t)
    | GE (e1, e2) ->
      let t = new_temp() in
      let (sub_instr1, e1') = translate_exp e1 in
      let (sub_instr2, e2') = translate_exp e2 in
      (sub_instr1 @ sub_instr2 @ [(0, T.ASSIGNV (t, T.GE, e1', e2'))], t)
    | EQ (e1, e2) ->
      let t = new_temp() in
      let (sub_instr1, e1') = translate_exp e1 in
      let (sub_instr2, e2') = translate_exp e2 in
      (sub_instr1 @ sub_instr2 @ [(0, T.ASSIGNV (t, T.EQ, e1', e2'))], t)
    | AND (e1, e2) ->
      let t = new_temp() in
      let (sub_instr1, e1') = translate_exp e1 in
      let (sub_instr2, e2') = translate_exp e2 in
      (sub_instr1 @ sub_instr2 @ [(0, T.ASSIGNV (t, T.AND, e1', e2'))], t)
    | OR (e1, e2) ->
      let t = new_temp() in
      let (sub_instr1, e1') = translate_exp e1 in
      let (sub_instr2, e2') = translate_exp e2 in
      (sub_instr1 @ sub_instr2 @ [(0, T.ASSIGNV (t, T.OR, e1', e2'))], t)

let rec translate_block : S.block -> T.linstr list
  =fun block ->
  let rec translate_stmt : S.stmt -> T.linstr list
    =fun stmt ->
      match stmt with
      | S.ASSIGN (S.ID x, e) -> 
        let (sub_instr, e') = translate_exp e in
        sub_instr @ [(0, T.COPY(x, e'))]
      | S.ASSIGN (S.ARR (x,i), e) ->
        let (sub_instr_i, i') = translate_exp i in
        let (sub_instr_e, e') = translate_exp e in
        sub_instr_i @ sub_instr_e @ [(0, T.STORE((x, i'), e'))]
      | S.IF (b, s1, s2) ->
        let sym = new_label() in
        let sym2 = new_label() in
        let sym3 = new_label() in
        let (sub_instr_b, b') = translate_exp b in
        sub_instr_b @ [(0, T.CJUMP(b', sym)); (0, T.UJUMP sym2); (sym, T.SKIP)] @ translate_stmt s1 @ [(0, T.UJUMP sym3)] @ [(sym2, T.SKIP)] @ translate_stmt s2 @ [(0, T.UJUMP sym3); (sym3, T.SKIP)]
      | S.WHILE (b, s) ->
        let sym = new_label() in
        let sym2 = new_label() in
        let (sub_instr_b, b') = translate_exp b in
        [(sym, T.SKIP)] @ sub_instr_b @ [(0, T.CJUMPF(b', sym2))] @ translate_stmt s @ [(0, T.UJUMP sym); (sym2, T.SKIP)]
      | S.DOWHILE (s, b) -> translate_stmt s @ translate_stmt (S.WHILE (b, s))
      | S.READ x -> [(0, T.READ x)]
      | S.PRINT e ->
        let (sub_instr, e') = translate_exp e in
        sub_instr @ [(0, T.WRITE e')]
      | S.BLOCK b -> translate_block b in
  let rec translate_stmts : S.stmts -> T.linstr list
    =fun stmts ->
      match stmts with
      | [] -> []
      | s::rest -> 
        let rest' = translate_stmts rest in
        let instr = translate_stmt s in
      instr @ rest' in
    match block with
    | (decls, stmts) ->
      let decls' = (translate_decls decls) in
      let stmts' = (translate_stmts stmts) in
      decls' @ stmts'

let s2t : S.program -> T.program
=fun s -> 
  let pgm = (translate_block s) @ [(0, T.HALT)] in
  pgm

(*************************************)
(*     translation from S to Cfg     *)
(*************************************)
let rec create_cfg_decls : (Cfg.t * S.decls * Node.t) -> (Cfg.t * Node.t)
  =fun (cfg, decls, prev_node) ->
    match decls with
    | [] -> (cfg, prev_node)
    | (S.TINT, x)::rest ->
      let node = Node.create_assign (S.ID x) (S.NUM 0) in
      let cfg = Cfg.add_node node cfg in
      let cfg = Cfg.add_edge prev_node node cfg in
      create_cfg_decls (cfg, rest, node)
    | (S.TARR n, x)::rest ->
      let node = Node.create_alloc x n in
      let cfg = Cfg.add_node node cfg in
      let cfg = Cfg.add_edge prev_node node cfg in
      create_cfg_decls (cfg, rest, node)

let rec create_cfg : (Cfg.t * S.block * Node.t) -> (Cfg.t * Node.t)
  =fun (cfg, s, prev_node) ->
    let rec create_cfg_stmt : (Cfg.t * S.stmt * Node.t) -> (Cfg.t * Node.t)
      =fun (cfg, s, prev_node) ->
        match s with
        | S.ASSIGN (lv, e) ->
          let node = Node.create_assign lv e in
          let cfg = Cfg.add_node node cfg in
          let cfg = Cfg.add_edge prev_node node cfg in
          (cfg, node)
        | S.IF (b, s1, s2) ->
          let skip_node = Node.create_skip () in
          let cfg = Cfg.add_node skip_node cfg in
          let cfg = Cfg.add_edge prev_node skip_node cfg in
          let assumeT_node = Node.create_assume b in
          let assumeF_node = Node.create_assume (S.NOT b) in
          let cfg = Cfg.add_node assumeT_node cfg in
          let cfg = Cfg.add_node assumeF_node cfg in
          let cfg = Cfg.add_edge skip_node assumeT_node cfg in
          let cfg = Cfg.add_edge skip_node assumeF_node cfg in
          let (cfg, t_lnode) = create_cfg_stmt (cfg, s1, assumeT_node) in
          let (cfg, f_lnode) = create_cfg_stmt (cfg, s2, assumeF_node) in
          let skip_node2 = Node.create_skip () in
          let cfg = Cfg.add_node skip_node2 cfg in
          let cfg = Cfg.add_edge t_lnode skip_node2 cfg in
          let cfg = Cfg.add_edge f_lnode skip_node2 cfg in
          (cfg, skip_node2)
        | S.WHILE (b, s) ->
          let skip_node = Node.create_skip () in
          let cfg = Cfg.add_node skip_node cfg in
          let cfg = Cfg.add_edge prev_node skip_node cfg in
          let assumeT_node = Node.create_assume b in
          let assumeF_node = Node.create_assume (S.NOT b) in
          let cfg = Cfg.add_node assumeT_node cfg in
          let cfg = Cfg.add_node assumeF_node cfg in
          let cfg = Cfg.add_edge skip_node assumeT_node cfg in
          let cfg = Cfg.add_edge skip_node assumeF_node cfg in
          let (cfg, t_lnode) = create_cfg_stmt (cfg, s, assumeT_node) in
          let cfg = Cfg.add_edge t_lnode skip_node cfg in
          (cfg, assumeF_node)
        | S.DOWHILE (s, b) ->
          let (cfg, lnode) = create_cfg_stmt (cfg, s, prev_node) in
          create_cfg_stmt (cfg, S.WHILE (b, s), lnode)
        | S.READ x ->
          let node = Node.create_read x in
          let cfg = Cfg.add_node node cfg in
          let cfg = Cfg.add_edge prev_node node cfg in
          (cfg, node)
        | S.PRINT e ->
          let node = Node.create_print e in
          let cfg = Cfg.add_node node cfg in
          let cfg = Cfg.add_edge prev_node node cfg in
          (cfg, node)
        | S.BLOCK b ->
          create_cfg (cfg, b, prev_node)
    in
    let rec create_cfg_stmts : (Cfg.t * S.stmts * Node.t) -> (Cfg.t * Node.t)
      =fun (cfg, stmts, prev_node) ->
        match stmts with
        | [] -> (cfg, prev_node)
        | s::rest ->
          let (cfg, last_node) = create_cfg_stmt (cfg, s, prev_node) in
          create_cfg_stmts (cfg, rest, last_node)
    in
    match s with
    | (decls, stmts) ->
      let (cfg, node) = create_cfg_decls (cfg, decls, prev_node) in
      create_cfg_stmts (cfg, stmts, node)

let s2cfg : S.program -> Cfg.t
  =fun s ->
    let cfg = Cfg.empty in
    let entry_node = Node.create_skip () in
    let last_node = Node.create_skip () in
    let cfg = Cfg.add_node entry_node cfg in
    let cfg, lnode = create_cfg (cfg, s, entry_node) in
    let cfg = Cfg.add_node last_node cfg in
    let cfg = Cfg.add_edge lnode last_node cfg in
    let cfg = Cfg.remove_unnecessary_skips cfg in
    cfg