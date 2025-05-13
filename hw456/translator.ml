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
      let (sub_instr, e') = translate_exp e in
      (sub_instr @ [(0, T.LOAD (new_temp(), (id, e')))], e')
    | ADD (e1, e2) ->
      let (sub_instr1, e1') = translate_exp e1 in
      let (sub_instr2, e2') = translate_exp e2 in
      let t = new_temp() in
      (sub_instr1 @ sub_instr2 @ [(0, T.ASSIGNV (t, T.ADD, e1', e2'))], t)
    | SUB (e1, e2) ->
      let (sub_instr1, e1') = translate_exp e1 in
      let (sub_instr2, e2') = translate_exp e2 in
      let t = new_temp() in
      (sub_instr1 @ sub_instr2 @ [(0, T.ASSIGNV (t, T.SUB, e1', e2'))], t)
    | MUL (e1, e2) ->
      let (sub_instr1, e1') = translate_exp e1 in
      let (sub_instr2, e2') = translate_exp e2 in
      let t = new_temp() in
      (sub_instr1 @ sub_instr2 @ [(0, T.ASSIGNV (t, T.MUL, e1', e2'))], t)
    | DIV (e1, e2) ->
      let (sub_instr1, e1') = translate_exp e1 in
      let (sub_instr2, e2') = translate_exp e2 in
      let t = new_temp() in
      (sub_instr1 @ sub_instr2 @ [(0, T.ASSIGNV (t, T.DIV, e1', e2'))], t)
    | MINUS e ->
      let (sub_instr, e') = translate_exp e in
      let t = new_temp() in
      (sub_instr @ [(0, T.ASSIGNU (t, T.MINUS, e'))], t)
    | NOT e ->
      let (sub_instr, e') = translate_exp e in
      let t = new_temp() in
      (sub_instr @ [(0, T.ASSIGNU (t, T.NOT, e'))], t)
    | LT (e1, e2) ->
      let (sub_instr1, e1') = translate_exp e1 in
      let (sub_instr2, e2') = translate_exp e2 in
      let t = new_temp() in
      (sub_instr1 @ sub_instr2 @ [(0, T.ASSIGNV (t, T.LT, e1', e2'))], t)
    | LE (e1, e2) ->
      let (sub_instr1, e1') = translate_exp e1 in
      let (sub_instr2, e2') = translate_exp e2 in
      let t = new_temp() in
      (sub_instr1 @ sub_instr2 @ [(0, T.ASSIGNV (t, T.LE, e1', e2'))], t)
    | GT (e1, e2) ->
      let (sub_instr1, e1') = translate_exp e1 in
      let (sub_instr2, e2') = translate_exp e2 in
      let t = new_temp() in
      (sub_instr1 @ sub_instr2 @ [(0, T.ASSIGNV (t, T.GT, e1', e2'))], t)
    | GE (e1, e2) ->
      let (sub_instr1, e1') = translate_exp e1 in
      let (sub_instr2, e2') = translate_exp e2 in
      let t = new_temp() in
      (sub_instr1 @ sub_instr2 @ [(0, T.ASSIGNV (t, T.GE, e1', e2'))], t)
    | EQ (e1, e2) ->
      let (sub_instr1, e1') = translate_exp e1 in
      let (sub_instr2, e2') = translate_exp e2 in
      let t = new_temp() in
      (sub_instr1 @ sub_instr2 @ [(0, T.ASSIGNV (t, T.EQ, e1', e2'))], t)
    | AND (e1, e2) ->
      let (sub_instr1, e1') = translate_exp e1 in
      let (sub_instr2, e2') = translate_exp e2 in
      let t = new_temp() in
      (sub_instr1 @ sub_instr2 @ [(0, T.ASSIGNV (t, T.AND, e1', e2'))], t)
    | OR (e1, e2) ->
      let (sub_instr1, e1') = translate_exp e1 in
      let (sub_instr2, e2') = translate_exp e2 in
      let t = new_temp() in
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

let s2cfg : S.program -> Cfg.t 
=fun s -> ignore s; raise Not_implemented