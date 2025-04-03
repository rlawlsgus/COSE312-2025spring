exception NotImplemented;;

type mobile = branch * branch
and branch =
  | SimpleBranch of length * weight
  | CompoundBranch of length * mobile
and length = int
and weight = int;;

let rec calc: mobile -> weight
= fun mobile -> match mobile with
  | (SimpleBranch (l1, w1), SimpleBranch (l2, w2)) -> w1 + w2
  | (SimpleBranch (l1, w1), CompoundBranch (l2, m2)) -> w1 + (calc m2)
  | (CompoundBranch (l1, m1), SimpleBranch (l2, w2)) -> (calc m1) + w2
  | (CompoundBranch (l1, m1), CompoundBranch (l2, m2)) -> (calc m1) + (calc m2);;

let check: mobile -> bool
= fun mobile -> match mobile with
  | (SimpleBranch (l1, w1), SimpleBranch (l2, w2)) -> l1 * w1 = l2 * w2
  | (SimpleBranch (l1, w1), CompoundBranch (l2, m2)) -> l1 * w1 = l2 * (calc m2)
  | (CompoundBranch (l1, m1), SimpleBranch (l2, w2)) -> l1 * (calc m1) = l2 * w2
  | (CompoundBranch (l1, m1), CompoundBranch (l2, m2)) -> l1 * (calc m1) = l2 * (calc m2);;

let rec balanced: mobile -> bool
= fun mobile ->
  match mobile with
  | (SimpleBranch (l1, w1), SimpleBranch (l2, w2)) -> check mobile
  | (SimpleBranch (l1, w1), CompoundBranch (l2, m2)) -> check mobile && balanced m2
  | (CompoundBranch (l1, m1), SimpleBranch (l2, w2)) -> check mobile && balanced m1
  | (CompoundBranch (l1, m1), CompoundBranch (l2, m2)) -> check mobile && balanced m1 && balanced m2;;
