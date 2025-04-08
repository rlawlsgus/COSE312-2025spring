open Regex 

exception Not_implemented

let regex2nfa : Regex.t -> Nfa.t 
=fun reg ->
  let rec convert regex =
    match regex with
    | Empty ->
      let nfa = Nfa.create_new_nfa () in
      let final, new_nfa = Nfa.create_state nfa in
      let new_nfa = Nfa.add_final_state new_nfa final in
      new_nfa
    | Epsilon ->
      let nfa = Nfa.create_new_nfa () in
      let is = Nfa.get_initial_state nfa in
      let final, new_nfa = Nfa.create_state nfa in
      let new_nfa = Nfa.add_final_state new_nfa final in
      let new_nfa = Nfa.add_epsilon_edge new_nfa (is, final) in
      new_nfa
    | Alpha a -> 
      let nfa = Nfa.create_new_nfa () in
      let is = Nfa.get_initial_state nfa in
      let final, new_nfa = Nfa.create_state nfa in
      let new_nfa = Nfa.add_final_state new_nfa final in
      let new_nfa = Nfa.add_edge new_nfa (is, a, final) in
      new_nfa
    | OR (r1, r2) -> 
      let nfa1 = convert r1 in
      let nfa2 = convert r2 in
      let is1 = Nfa.get_initial_state nfa1 in
      let is2 = Nfa.get_initial_state nfa2 in
      let final1 = Nfa.get_final_states nfa1 in
      let final2 = Nfa.get_final_states nfa2 in

      let new_nfa = Nfa.create_new_nfa () in
      let final, new_nfa = Nfa.create_state new_nfa in
      let new_nfa = Nfa.add_states new_nfa (Nfa.get_states nfa1) in
      let new_nfa = Nfa.add_states new_nfa (Nfa.get_states nfa2) in
      let new_nfa = Nfa.add_edges new_nfa (Nfa.get_edges nfa1) in
      let new_nfa = Nfa.add_edges new_nfa (Nfa.get_edges nfa2) in
      let new_nfa = Nfa.add_final_state new_nfa final in
      let new_nfa = Nfa.add_epsilon_edge new_nfa (Nfa.get_initial_state new_nfa, is1) in
      let new_nfa = Nfa.add_epsilon_edge new_nfa (Nfa.get_initial_state new_nfa, is2) in
      let new_nfa = BatSet.fold (fun f acc -> 
        Nfa.add_epsilon_edge acc (f, final)) final1 new_nfa in
      let new_nfa = BatSet.fold (fun f acc -> 
        Nfa.add_epsilon_edge acc (f, final)) final2 new_nfa in
      new_nfa
    | CONCAT (r1, r2) ->
      let nfa1 = convert r1 in
      let nfa2 = convert r2 in
      let final1 = Nfa.get_final_states nfa1 in
      let final2 = Nfa.get_final_states nfa2 in
      let is1 = Nfa.get_initial_state nfa1 in
      let is2 = Nfa.get_initial_state nfa2 in

      let new_nfa = Nfa.create_new_nfa () in
      let final, new_nfa = Nfa.create_state new_nfa in
      let new_nfa = Nfa.add_final_state new_nfa final in
      let new_nfa = Nfa.add_states new_nfa (Nfa.get_states nfa1) in
      let new_nfa = Nfa.add_states new_nfa (Nfa.get_states nfa2) in
      let new_nfa = Nfa.add_edges new_nfa (Nfa.get_edges nfa1) in
      let new_nfa = Nfa.add_edges new_nfa (Nfa.get_edges nfa2) in
      let new_nfa = BatSet.fold (fun f acc -> 
        Nfa.add_epsilon_edge acc (f, is2)) final1 new_nfa in
      let new_nfa = Nfa.add_epsilon_edge new_nfa (Nfa.get_initial_state new_nfa, is1) in
      let new_nfa = BatSet.fold (fun f acc -> 
        Nfa.add_epsilon_edge acc (f, final)) final2 new_nfa in
      new_nfa
    | STAR r -> 
      let nfa = convert r in
      let is = Nfa.get_initial_state nfa in
      let final1 = Nfa.get_final_states nfa in
      let nfa = BatSet.fold (fun f acc ->
        Nfa.add_epsilon_edge acc (f, is)) final1 nfa in
      
      let new_nfa = Nfa.create_new_nfa () in
      let final, new_nfa = Nfa.create_state new_nfa in
      let new_nfa = Nfa.add_final_state new_nfa final in
      let new_nfa = Nfa.add_states new_nfa (Nfa.get_states nfa) in
      let new_nfa = Nfa.add_edges new_nfa (Nfa.get_edges nfa) in
      let new_nfa = Nfa.add_epsilon_edge new_nfa (Nfa.get_initial_state new_nfa, final) in
      let new_nfa = Nfa.add_epsilon_edge new_nfa (Nfa.get_initial_state new_nfa, is) in
      let new_nfa = BatSet.fold (fun f acc -> 
        Nfa.add_epsilon_edge acc (f, final)) final1 new_nfa in
      new_nfa
  in
  (* Nfa.print (convert reg); *)
  convert reg

 

  let nfa2dfa : Nfa.t -> Dfa.t
  =fun nfa ->
    let rec e_closure (s : Nfa.states) : Nfa.states =
      let rec aux (visited : Nfa.states) (to_visit : Nfa.states) : Nfa.states =
        if BatSet.is_empty to_visit then
          visited
        else
          let current = BatSet.choose to_visit in
          let new_visited = BatSet.add current visited in
          let new_to_visit = 
            match BatMap.find_opt current e_delta with
            | Some next_states -> BatSet.union (BatSet.diff next_states new_visited) (BatSet.remove current to_visit)
            | None -> BatSet.remove current to_visit
          in
          aux new_visited new_to_visit
      in
      aux BatSet.empty (BatSet.singleton s) in
    let edge, eps_edge = Nfa.get_edges nfa in
    let is_eclosure = e_closure (BatSet.singleton (Nfa.get_initial_state nfa)) in

    let dfa = Dfa.create_new_dfa is_eclosure in
    let e_closure_of_A_states (nfa : Nfa.t) (states : Nfa.states) : Nfa.states =
      let (_, _, e_delta : Nfa.e_delta, _, _) = nfa in
      let A_states = BatSet.fold (fun state acc -> BatSet.union (Nfa.get_next_state nfa state Regex.A) acc) states BatSet.empty in
      e_closure e_delta A_states
    
    let dfa = aux dfa (BatSet.singleton (Nfa.get_initial_state nfa)) in
    Dfa.print dfa;
    dfa
 
(* Do not modify this function *)
(* let regex2dfa : Regex.t -> Dfa.t
=fun regex -> 
  let nfa = regex2nfa regex in
  let dfa = nfa2dfa nfa in
    dfa

let run_dfa : Dfa.t -> alphabet list -> bool
=fun _ _ -> raise Not_implemented TODO *)
