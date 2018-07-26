(* Performance measures for regression models
   cf. chapter 12 "regression models" in book
   Varnek, A. ed., 2017. Tutorials in chemoinformatics. John Wiley & Sons. *)

module A = BatArray

let square x =
  x *. x

(** Root Mean Squared Error
    [rmse exp pred] *)
let rmse (l1: float list) (l2: float list): float =
  let a1 = A.of_list l1 in
  let a2 = A.of_list l2 in
  let m = A.length a1 in
  let n = A.length a2 in
  assert(m = n);
  let sum_squared_diffs =
    A.fold_lefti (fun acc i x ->
        let y = a2.(i) in
        acc +. square (x -. y)
      ) 0.0 a1 in
  sqrt (sum_squared_diffs /. (float n))

(** Mean Absolute Error
    [mae exp pred] *)
let mae (l1: float list) (l2: float list): float =
  let a1 = A.of_list l1 in
  let a2 = A.of_list l2 in
  let m = A.length a1 in
  let n = A.length a2 in
  assert(m = n);
  let sum_abs_diffs =
    A.fold_lefti (fun acc i x ->
        let y = a2.(i) in
        acc +. abs_float (x -. y)
      ) 0.0 a1 in
  sum_abs_diffs /. (float n)

(** standard deviation of residuals
    [std_dev_res exp pred] *)
let std_dev_res (l1: float list) (l2: float list): float =
  let a1 = A.of_list l1 in
  let a2 = A.of_list l2 in
  let m = A.length a1 in
  let n = A.length a2 in
  assert(m = n);
  let sum_squared_diffs =
    A.fold_lefti (fun acc i x ->
        let y = a2.(i) in
        acc +. square (x -. y)
      ) 0.0 a1 in
  sqrt (sum_squared_diffs /. (float (n - 2)))

(** coefficient of determination
    [r2 exp pred] *)
let r2 (l1: float list) (l2: float list): float =
  let a1 = A.of_list l1 in
  let a2 = A.of_list l2 in
  let m = A.length a1 in
  let n = A.length a2 in
  assert(m = n);
  let sum_squared_diffs =
    A.fold_lefti (fun acc i x ->
        let y = a2.(i) in
        acc +. square (x -. y)
      ) 0.0 a1 in
  let sum_squared_exp_diffs =
    let avg_exp = A.favg a1 in
    A.fold_left (fun acc x ->
        acc +. square (x -. avg_exp)
      ) 0.0 a1 in
  1.0 -. (sum_squared_diffs /. sum_squared_exp_diffs)
