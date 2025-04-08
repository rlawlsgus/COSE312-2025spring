open Regex

let testcases : (Regex.t * alphabet list) list = 
  [ 
    (Empty, []); (*fasle*)
    (Epsilon, []); (*true*)
    (Alpha A, [A]); (*true*)
    (Alpha A, [B]); (*false*)
    (OR (Alpha A, Alpha B), [B]); (*true*)
    (CONCAT (STAR (Alpha A), Alpha B), [B]); (*true*)
    (CONCAT (STAR (Alpha A), Alpha B), [A;B]); (*true*)
    (CONCAT (STAR (Alpha A), Alpha B), [A;A;B]); (*true*)
    (CONCAT (STAR (Alpha A), Alpha B), [A;B;B]); (*false*)
    (CONCAT (CONCAT (STAR (CONCAT (Alpha A, Alpha A)), STAR (CONCAT (Alpha B, Alpha B))), Alpha B), [B]); (*true*)
    (CONCAT (CONCAT (STAR (CONCAT (Alpha A, Alpha A)), STAR (CONCAT (Alpha B, Alpha B))), Alpha B), [A;A;B]); (*true*)
    (CONCAT (CONCAT (STAR (CONCAT (Alpha A, Alpha A)), STAR (CONCAT (Alpha B, Alpha B))), Alpha B), [B;B;B]); (*true*)
    (CONCAT (CONCAT (STAR (CONCAT (Alpha A, Alpha A)), STAR (CONCAT (Alpha B, Alpha B))), Alpha B), [A;A;A;A;B;B;B]); (*true*)
    (CONCAT (CONCAT (STAR (CONCAT (Alpha A, Alpha A)), STAR (CONCAT (Alpha B, Alpha B))), Alpha B), [A;A;A;B;B;B]); (*false*)
  ]

(* let match_regex : Regex.t -> alphabet list -> bool
=fun regex input -> Trans.run_dfa (Trans.regex2dfa regex) input *)

(* run testcases *)
let _ = 
  (* List.iter (fun (regex, str) -> 
    prerr_endline (string_of_bool (match_regex regex str)) 
  ) testcases *)
   List.iter (fun (regex, str) -> 
    ignore str;
    ignore (Trans.nfa2dfa (Trans.regex2nfa regex))) testcases
