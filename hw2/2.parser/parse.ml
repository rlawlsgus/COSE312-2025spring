open Util

type symbol = T of string | N of string | Epsilon | End
type production = symbol * symbol list
type cfg = symbol list * symbol list * symbol * production list

let string_of_symbol s = 
  match s with 
  | T x -> x 
  | N x -> x 
  | Epsilon -> "epsilon"
  | End -> "$"

let string_of_prod (lhs, rhs) = 
    string_of_symbol lhs ^ " -> " ^ 
      string_of_list ~first:"" ~last:"" ~sep:" " string_of_symbol rhs 

module FIRST = struct 
  type t = (symbol, symbol BatSet.t) BatMap.t

  let empty = BatMap.empty 
  
  let find : symbol -> t -> symbol BatSet.t 
  =fun s t -> try BatMap.find s t with _ -> BatSet.empty 
  
  let add : symbol -> symbol -> t -> t 
  =fun s1 s2 t -> BatMap.add s1 (BatSet.add s2 (find s1 t)) t
  
  let add_set : symbol -> symbol BatSet.t -> t -> t 
  =fun s1 ss t -> BatSet.fold (fun s2 -> add s1 s2) ss t

  let tostring : t -> string
  =fun t -> 
    BatMap.foldi (fun s ss str -> 
      str ^ string_of_symbol s ^ " |-> " ^ string_of_set string_of_symbol ss ^ "\n"
    ) t ""
end   

module FOLLOW = struct
  type t = (symbol, symbol BatSet.t) BatMap.t 

  let empty = BatMap.empty 

  let find : symbol -> t -> symbol BatSet.t 
  =fun s t -> try BatMap.find s t with _ -> BatSet.empty 

  let add : symbol -> symbol -> t -> t 
  =fun s1 s2 t -> BatMap.add s1 (BatSet.add s2 (find s1 t)) t

  let add_set : symbol -> symbol BatSet.t -> t -> t 
  =fun s1 ss t -> BatSet.fold (fun s2 -> add s1 s2) ss t

  let tostring : t -> string
  =fun t -> 
    BatMap.foldi (fun s ss str -> 
      str ^ string_of_symbol s ^ " |-> " ^ string_of_set string_of_symbol ss ^ "\n"
    ) t ""
end

module ParsingTable = struct
  type t = (symbol * symbol, (symbol * symbol list) BatSet.t) BatMap.t 

  let empty = BatMap.empty 

  let find (nonterm, term) t = try BatMap.find (nonterm, term) t with _ -> BatSet.empty 

  let add (nonterm, term) prod t = 
    BatMap.add (nonterm, term) (BatSet.add prod (find (nonterm, term) t)) t
    
  let tostring : t -> string 
  =fun t -> 
    BatMap.foldi (fun (nonterm, term) prods str -> 
      str ^ string_of_symbol nonterm ^ ", " ^ string_of_symbol term ^ " |-> " ^
        string_of_set string_of_prod prods ^ "\n"
    ) t ""
    
  let foldi = BatMap.foldi 

  let for_all = BatMap.for_all
end

let cfg_set_epsilon : cfg -> cfg
= fun (nonterms, terms, start, prods) ->
  let rec cfg_helper : production list -> production list = fun prods ->
    match prods with
    | [] -> []
    | (lhs, rhs)::rest -> 
      if rhs = [] then 
        (lhs, [Epsilon])::(cfg_helper rest)
      else 
        (lhs, rhs)::(cfg_helper rest) in
  (nonterms, terms, start, cfg_helper prods)

let initialize_first : symbol list -> FIRST.t =
  fun symbols ->
    List.fold_left (fun acc s ->
      match s with
      | T _ -> FIRST.add s s acc
      | _ -> BatMap.add s BatSet.empty acc
    ) FIRST.empty symbols

let rec all_nullable rhs t =
  match rhs with
  | [] -> true
  | sy :: rest ->
    let sy_first = FIRST.find sy t in
    if BatSet.mem Epsilon sy_first then all_nullable rest t
    else false
  
let cfg_to_first : cfg -> FIRST.t
= fun (cfg) ->
  let (nonterms, terms, _, prods) = cfg in
  let all_symbols = nonterms @ terms in
  let initial_first = initialize_first all_symbols in
  let initial_first = FIRST.add Epsilon Epsilon initial_first in
  let first_helper : symbol -> FIRST.t -> FIRST.t = fun s t ->
    match s with
    | N _ ->
      let prods_for_s = List.filter (fun (lhs, _) -> lhs = s) prods in
      let process_prod t (_, rhs) =
        let rec loop rhs acc_t =
          match rhs with
          | [] -> acc_t
          | [Epsilon] ->
            FIRST.add_set s (BatSet.singleton Epsilon) acc_t
          | sy :: rest ->
            let sy_first = FIRST.find sy acc_t in
            let acc_t' = FIRST.add_set s (BatSet.remove Epsilon sy_first) acc_t in
            if BatSet.mem Epsilon sy_first then loop rest acc_t' else acc_t'
        in
        let t' = loop rhs t in
        if all_nullable rhs t then FIRST.add s Epsilon t' else t'
      in
      List.fold_left process_prod t prods_for_s
    | _ -> raise (Failure "Invalid symbol") in
  let rec fixpoint t =
    let new_t = List.fold_left (fun acc nonterm -> first_helper nonterm acc) t nonterms in
    if BatMap.equal BatSet.equal t new_t then new_t
    else fixpoint new_t
  in
  fixpoint initial_first

  let cfg_to_follow : cfg -> FIRST.t -> FOLLOW.t
  = fun (cfg) first ->
    let (nonterms, _, start, prods) = cfg in
    let initial_follow = FOLLOW.add start End FOLLOW.empty in
    let follow_helper : symbol -> FOLLOW.t -> FOLLOW.t = fun s t ->
      match s with
      | N _ ->
        List.fold_left (fun acc (lhs, rhs) ->
          let rec walk rhs acc =
            match rhs with
            | [] -> acc
            | sy :: rest ->
              let acc' =
                if sy = s then (
                  match rest with
                  | beta :: _ ->
                      let first_beta = FIRST.find beta first in
                      let without_eps = BatSet.remove Epsilon first_beta in
                      let acc1 = FOLLOW.add_set s without_eps acc in
                      if BatSet.mem Epsilon first_beta then
                        let follow_lhs = FOLLOW.find lhs acc1 in
                        FOLLOW.add_set s follow_lhs acc1
                      else acc1
                  | [] ->
                      let follow_lhs = FOLLOW.find lhs acc in
                      FOLLOW.add_set s follow_lhs acc
                )
                else acc
              in
              walk rest acc'
          in
          walk rhs acc
        ) t prods
      | _ -> raise (Failure "Invalid symbol")
     in
    let rec fixpoint t =
      let new_t = List.fold_left (fun acc nonterm -> follow_helper nonterm acc) t nonterms in
      if BatMap.equal BatSet.equal t new_t then new_t else fixpoint new_t
    in
    fixpoint initial_follow

let cfg_to_parsing_table : cfg -> FIRST.t -> FOLLOW.t -> ParsingTable.t
= fun (_, _, _, prods) first follow ->
  let table = ref ParsingTable.empty in
  let add_entry nt term prod =
    table := ParsingTable.add (nt, term) prod !table
  in
  let is_terminal = function T _ -> true | End -> true | _ -> false in
  List.iter (fun (lhs, rhs) ->
    (*print_endline (string_of_prod (lhs, rhs));*)
    let rec first_of_seq symbols =
      match symbols with
      | [] -> BatSet.singleton Epsilon
      | x::xs ->
        let fx = FIRST.find x first in
        let fx_no_eps = BatSet.remove Epsilon fx in
        if BatSet.mem Epsilon fx then
          BatSet.union fx_no_eps (first_of_seq xs)
        else fx_no_eps
    in
    let first_rhs = first_of_seq rhs in
    (*print_endline ("First of RHS: " ^ string_of_set string_of_symbol first_rhs);*)
    BatSet.iter (fun sym ->
      if is_terminal sym then
        add_entry lhs sym (lhs, rhs)
    ) first_rhs;
    if BatSet.mem Epsilon first_rhs then
      let follow_lhs = FOLLOW.find lhs follow in
      BatSet.iter (fun sym ->
        if is_terminal sym then
          add_entry lhs sym (lhs, rhs)
      ) follow_lhs;
  ) prods;
  !table



  let check_LL1 : cfg -> bool
  = fun cfg ->
    let cfg = cfg_set_epsilon cfg in
    let first = cfg_to_first cfg in
    (*FIRST.tostring first |> print_endline;*)
    let follow = cfg_to_follow cfg first in
    (*FOLLOW.tostring follow |> print_endline;*)
    let table = cfg_to_parsing_table cfg first follow in
    (*print_endline (ParsingTable.tostring table);*)
    ParsingTable.for_all (fun _ prods -> BatSet.cardinal prods <= 1) table
  


let parse : cfg -> symbol list -> bool
=fun cfg syms ->
  let cfg = cfg_set_epsilon cfg in
  let (_, _, start, _) = cfg in
  let first = cfg_to_first cfg in
  let follow = cfg_to_follow cfg first in
  let table = cfg_to_parsing_table cfg first follow in
  let rec parse_helper stack input =
    (*print_endline ("Stack: " ^ string_of_list string_of_symbol stack ^ "            Input: " ^ string_of_list string_of_symbol input);*)
    match (stack, input) with
    | ([End], [End]) -> true
    | ([End], _) -> false
    | (s::stack', [End]) ->
      (match s with
      | T x when x = string_of_symbol End ->
        (*print_endline ("match");*)
        parse_helper stack' [End]
      | N _ ->
        let prod = ParsingTable.find (s, End) table in
        if BatSet.cardinal prod == 1 then (
          let prod = BatSet.choose prod in
          let (_, rhs) = prod in
          if rhs = [Epsilon] then parse_helper stack' [End]
          else parse_helper (rhs @ stack') [End]
        ) else false
      | _ -> false)
    | (s::stack', t::input') ->
      (match s with
      | T x when x = string_of_symbol t ->
        (*print_endline ("match");*)
        parse_helper stack' input'
      | N _ ->
      let prod = ParsingTable.find (s, t) table in
      if BatSet.cardinal prod == 1 then (
        let prod = BatSet.choose prod in
        let (_, rhs) = prod in
        if rhs = [Epsilon] then parse_helper stack' input
        else parse_helper (rhs @ stack') input
      ) else false
      | _ -> false)
    | _ -> false in
  parse_helper [start; End] syms
