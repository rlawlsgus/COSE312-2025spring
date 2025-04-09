open Parse

let cfg1 = (
  [N "E"; N "E'"; N "T"; N "T'"; N "F"], 
  [T "+"; T "*"; T "("; T ")"; T "id"], 
  N "E",
  [
    (N "E", [N "T"; N "E'"]);
    (N "E'", [T "+"; N "T"; N "E'"]);
    (N "E'", []);
    (N "T", [N "F"; N "T'"]);
    (N "T'", [T "*"; N "F"; N "T'"]);
    (N "T'", []);
    (N "F", [T "("; N "E"; T ")"]);
    (N "F", [T "id"])
  ])

let cfg2 = (
  [N "S"; N "E"; N "E'"; N "T"; N "T'"; N "F"],
  [T "+"; T "-"; T "*"; T "/"; T "id"; T "num"; T "("; T ")"],
  N "S",
  [
      (N "S", [N "E"]);
      (N "E", [N "T"; N "E'"]);
      (N "E'", [T "+"; N "T"; N "E'"]);
      (N "E'", [T "-"; N "T"; N "E'"]);
      (N "E'", []);
      (N "T", [N "F"; N "T'"]);
      (N "T'", [T "*"; N "F"; N "T'"]);
      (N "T'", [T "/"; N "F"; N "T'"]);
      (N "T'", []);
      (N "F", [T "id"]);
      (N "F", [T "num"]);
      (N "F", [T "("; N "E" ;T ")"]);
  ]
)

let cfg3 = (
  [N "X"; N "Y"; N "Z"],
  [T "a"; T"c"; T"d"], 
  N "X", 
  [
    (N "X", [N "Y"]);
    (N "X", [T "a"]);
    (N "Y", [T "c"]);
    (N "Y", []);
    (N "Z", [T "d"]);
    (N "Z", [N "X"; N "Y"; N "Z"])
  ]
)

let cfg4 = (
  [N "S"; N "S'"; N "E"],
  [T "a"; T "b"; T "e"; T "i"; T "t"],
  N "S",
  [
   (N "S", [T "i"; N "E"; T "t"; N "S"; N "S'"]);
   (N "S", [T "a"]);
   (N "S'", [T "e"; N "S"]);
   (N "S'", []);
   (N "E", [T "b"])
  ] 
)

let s1 = [T "id"; T "+"; T "id"; T "*"; T "id"; End]
let s2 = [T "id"; T "/"; T "("; T "num"; T "+"; T "id"; T ")"; End]

let cfgs = [cfg1; cfg2; cfg3; cfg4]
let main () =
  List.iter (fun cfg ->
    print_endline (string_of_bool (check_LL1 cfg))
  ) cfgs;
  print_endline "";
  print_endline (string_of_bool (parse cfg1 s1)); (*true*)
  print_endline (string_of_bool (parse cfg1 s2)); (*false*)
  print_endline (string_of_bool (parse cfg2 s1)); (*true*)
  print_endline (string_of_bool (parse cfg2 s2)) (*true*)

let _ = main ()

(* ——————————————————————— *)
(* Helper functions for generating test strings   *)
(* ——————————————————————— *)

(* cfg는 (nonterminals, terminals, start, productions) 형태라고 가정합니다. productions는 (lhs,
   rhs) 쌍의 리스트이며, rhs는 symbol 리스트입니다. *)

   exception GenerationTooDeep

   let rec generate_valid_string_from_symbol cfg sym count =
     (* 재귀 횟수가 지나치게 많으면 예외 발생 *)
     if count > 10000
     then raise GenerationTooDeep
     else
       match sym with
         | T _ -> [ sym ] (* 터미널이면 그대로 반환 *)
         | N _ ->
           let _, _, _, productions = cfg in
           (* sym을 좌변(lhs)로 갖는 production들을 필터링 *)
           let prods = List.filter (fun (lhs, _) -> lhs = sym) productions in
             if prods = []
             then [] (* 해당 production이 없으면 빈 리스트 *)
             else
               (* production 중 하나를 무작위 선택하여 우변(rhs)의 각 symbol에 대해 재귀적으로 전개 *)
               let _, rhs = List.nth prods (Random.int (List.length prods)) in
                 List.concat
                   (List.map
                      (fun s -> generate_valid_string_from_symbol cfg s (count + 1))
                      rhs )
         | Epsilon -> [] (* Epsilon은 빈 리스트로 처리 *)
         | End -> raise GenerationTooDeep (* End는 예외 발생 *)
   
   
   let generate_valid_string cfg =
     let _, _, start, _ = cfg in
       try generate_valid_string_from_symbol cfg start 0 @ [ End ] with
         | GenerationTooDeep -> [ T "id"; End ]
   
   
   (* 재귀가 너무 깊어져 무한 전개가 의심되면 빈 문자열을 반환하여 건너뜀 *)
   
   let generate_random_string cfg =
     let _, terminals, _, _ = cfg in
     let n = 1 + Random.int 10 in
       List.init n (fun _ -> List.nth terminals (Random.int (List.length terminals)))
   
   
   (* ——————————————————————— *)
   (* Fuzzing function                             *)
   (* ——————————————————————— *)
   
   let fuzz () =
     Random.self_init ();
     (* 랜덤 시드 초기화 *)
     (* 통계 기록용 mutable 변수들 *)
     let valid_tests_run = ref 0 in
     let valid_tests_passed = ref 0 in
     let invalid_tests_run = ref 0 in
     let invalid_tests_passed = ref 0 in
   
     (* 1분마다 로그를 출력하는 스레드 *)
     let _ =
       Thread.create
         (fun () ->
           while true do
             Thread.delay 1.0;
             let total_valid = !valid_tests_run in
             let total_invalid = !invalid_tests_run in
             let valid_ratio =
               if total_valid = 0
               then 0.0
               else float_of_int !valid_tests_passed /. float_of_int total_valid
             in
             let invalid_ratio =
               if total_invalid = 0
               then 0.0
               else float_of_int !invalid_tests_passed /. float_of_int total_invalid
             in
               print_endline
                 ( "[FUZZ LOG] Valid tests: "
                 ^ string_of_int total_valid
                 ^ " (pass ratio: "
                 ^ string_of_float valid_ratio
                 ^ "), "
                 ^ "Invalid tests: "
                 ^ string_of_int total_invalid
                 ^ " (pass ratio: "
                 ^ string_of_float invalid_ratio
                 ^ ")" )
           done )
         ()
     in
   
     (* 무한반복으로 fuzz 테스트 수행 *)
     while true do
       (* cfgs 리스트에서 임의의 CFG 선택 *)
       let cfg = cfg1 in
       let is_valid = Random.bool () in
       let s =
         if is_valid then generate_valid_string cfg else generate_random_string cfg
       in
       let result = parse cfg s in
         if is_valid
         then begin
           valid_tests_run := !valid_tests_run + 1;
           if result
           then valid_tests_passed := !valid_tests_passed + 1
           else begin
             print_endline "[ERROR] Valid test failed";
             List.fold_left (fun _ s -> print_string (string_of_symbol s)) () s;
             print_endline "";
             Thread.delay 0.1
           end
         end
         else begin
           invalid_tests_run := !invalid_tests_run + 1;
           if not result then invalid_tests_passed := !invalid_tests_passed + 1
         end
       (*List.fold_left (fun _ s -> print_string (string_of_symbol s)) () s;*)
       (*print_endline "";*)
       (*print_endline (string_of_bool is_valid);*)
       (*(* 테스트 결과 출력 *)*)
       (*print_endline (string_of_bool result);*)
       (*Thread.delay 1.0*)
       (* 테스트 간 약간의 짧은 지연을 둘 수도 있습니다. 예: Thread.delay 0.001 *)
     done
   
   
   (* 테스트 시 fuzz 모드를 사용하려면 아래 라인의 주석을 해제하세요 *)
   let _ = fuzz ()
   