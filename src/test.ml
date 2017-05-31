
(* Example usage *)

(* firs, define your score_label module *)
module SL = struct
  type t = string * float * int * bool
  let get_score (_, s, _, _) = s
  let get_label (_, _, _, l) = l
end

(* second, instantiate the ROC functor for your score_label module *)
module ROC = MakeROC.Make (SL)

(* third, call any classification performance metric you need *)
let main () =
  let create name score index label =
    (name, score, index, label) in
  let scores =
    [ create "" 14.0 0 true;
      create "" 13.0 0 true;
      create "" 12.0 0 false;
      create "" 11.0 0 true;
      create "" 10.0 0 false;
      create ""  9.0 0 false;
      create ""  8.0 0 false;
      create ""  7.0 0 false;
      create ""  6.0 0 true;
      create ""  5.0 0 false;
      create ""  4.0 0 false;
      create ""  3.0 0 false;
      create ""  2.0 0 false;
      create ""  1.0 0 false ] in
  let tpr_x = 3. /. 4. in
  let fpr_x = (5. -. 3.) /. (14. -. 4.) in
  assert(ROC.power_metric 0.35 scores = tpr_x /. (tpr_x +. fpr_x));
  assert(ROC.auc [("", 1.0, 0, true) ; (("", 0.9, 1, false))] = 1.0);
  assert(ROC.auc [("", 1.0, 0, false); (("", 0.9, 1, true))] = 0.0);
  Printf.printf "all OK\n"

let () = main ()
