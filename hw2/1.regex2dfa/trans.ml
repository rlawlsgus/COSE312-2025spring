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
      let new_e_closure = BatSet.fold (fun state acc ->
        let e_states = Nfa.get_next_state_epsilon nfa state in
        BatSet.union acc e_states) s s in
      if BatSet.equal new_e_closure s then
        new_e_closure
      else
        e_closure new_e_closure
    in
    let dfa = ref( Dfa.create_new_dfa ( e_closure (BatSet.singleton (Nfa.get_initial_state nfa)))) in
    let visited = ref (BatSet.empty) in
    let nfa_final = Nfa.get_final_states nfa in

    let next_e_closure (s : Nfa.states) (a : Regex.alphabet) : Nfa.states =
      let new_states = BatSet.fold (fun state acc ->
        let next_states = Nfa.get_next_state nfa state a in
        BatSet.union acc (e_closure next_states)) s BatSet.empty in
      new_states
    in

    let check_final_state (s : Nfa.states) =
      let is_final = BatSet.exists (fun state ->
        BatSet.mem state nfa_final) s in
      if is_final then
        dfa := Dfa.add_final_state !dfa s;
    in

    check_final_state (Dfa.get_initial_state !dfa);

    let rec loop (s : Nfa.states) =
      let a_states = next_e_closure s Regex.A in
      let b_states = next_e_closure s Regex.B in
      if not (BatSet.is_empty (a_states)) then begin
        if not (BatSet.mem a_states !visited) then begin
          visited := BatSet.add a_states !visited;
          dfa := Dfa.add_state !dfa a_states;
          check_final_state a_states;
          loop a_states;
        end;
        dfa := Dfa.add_edge !dfa (s, Regex.A, a_states);
      end;
      if not (BatSet.is_empty (b_states)) then begin
        if not (BatSet.mem b_states !visited) then begin
          visited := BatSet.add b_states !visited;
          dfa := Dfa.add_state !dfa b_states;
          check_final_state b_states;
          loop b_states;
        end;
        dfa := Dfa.add_edge !dfa (s, Regex.B, b_states);
      end;
    in
    loop (Dfa.get_initial_state !dfa);
    (* Dfa.print !dfa; *)
    !dfa
 
(* Do not modify this function *)
let regex2dfa : Regex.t -> Dfa.t
=fun regex -> 
  let nfa = regex2nfa regex in
  let dfa = nfa2dfa nfa in
    dfa

let run_dfa : Dfa.t -> alphabet list -> bool
=fun dfa al -> 
  let rec loop (s : Dfa.state) al = 
    match al with
    | [] -> Dfa.is_final_state dfa s
    | a::rest -> 
      (try 
       let next_s = Dfa.get_next_state dfa s a in
       loop next_s rest
       with Failure _ -> false)
    in
    loop (Dfa.get_initial_state dfa) al
