
module type SCORE_LABEL = sig
  type t
  val get_score: t -> float
  val get_label: t -> bool
end

module type ROC_FUNCTOR = functor (SL: SCORE_LABEL) ->
sig
  (** sort score labels putting high scores first *)
  val rank_order_by_score: SL.t list -> SL.t list
  (** compute the cumulated actives curve given
      an already sorted list of score labels *)
  val cumulated_actives_curve: SL.t list -> int list
  (** compute Area Under the ROC curve given an already sorted list of
      score labels *)
  val fast_auc: SL.t list -> float
  (** compute Area Under the ROC curve given an unsorted list
      of score labels *)
  val auc: SL.t list -> float
  (** (early) enrichment factor at given threshold (database percentage)
      given an unsorted list of score labels *)
  val enrichment_factor: float -> SL.t list -> float
  (** power metric at given threshold given an unsorted list of score labels *)
  val power_metric: float -> SL.t list -> float
end

(* functions for ROC analysis *)
module Make: ROC_FUNCTOR = functor (SL: SCORE_LABEL) ->
struct

  module L = BatList

  let trapezoid_surface x1 x2 y1 y2 =
    let base = abs_float (x1 -. x2) in
    let height = 0.5 *. (y1 +. y2) in
    base *. height

  (* put molecules with the highest scores at the top of the list *)
  let rank_order_by_score (score_labels: SL.t list) =
    L.sort (fun x y ->
        BatFloat.compare (SL.get_score y) (SL.get_score x)
      ) score_labels

  (* compute the cumulated number of actives curve,
     given an already sorted list of score labels *)
  let cumulated_actives_curve (high_scores_first: SL.t list) =
    let sum = ref 0 in
    L.map (fun sl ->
        if SL.get_label sl then incr sum;
        !sum
      ) high_scores_first

  (* area under the ROC curve given an already sorted list of score-labels *)
  let fast_auc high_scores_first =
    let fp, tp, fp_prev, tp_prev, a, _p_prev =
      L.fold_left
        (fun (fp, tp, fp_prev, tp_prev, a, p_prev) sl ->
           let si = SL.get_score sl in
           let li = SL.get_label sl in
           let new_a, new_p_prev, new_fp_prev, new_tp_prev =
             if si <> p_prev then
               a +. trapezoid_surface fp fp_prev tp tp_prev,
               si,
               fp,
               tp
             else
               a,
               p_prev,
               fp_prev,
               tp_prev
           in
           let new_tp, new_fp =
             if li then
               tp +. 1., fp
             else
               tp, fp +. 1.
           in
           (new_fp, new_tp, new_fp_prev, new_tp_prev, new_a, new_p_prev)
        )
        (0., 0., 0., 0., 0., neg_infinity)
        high_scores_first
    in
    (a +. trapezoid_surface fp fp_prev tp tp_prev) /. (fp *. tp)

  (* area under the ROC curve given an unsorted list of score-labels
     TP cases have the label set to true
     TN cases have the label unset *)
  let auc (score_labels: SL.t list) =
    let high_scores_first = rank_order_by_score score_labels in
    fast_auc high_scores_first

  (* proportion of actives given an unsorted list of score-labels
     TP cases have the label set to true
     TN cases have the label unset
     returns: (nb_molecules, actives_rate) *)
  let actives_rate (score_labels: SL.t list) =
    let tp_count, fp_count =
      L.fold_left
        (fun (tp_c, fp_c) sl ->
           if SL.get_label sl then
             (tp_c + 1, fp_c)
           else
             (tp_c, fp_c + 1)
        ) (0, 0) score_labels
    in
    let nb_molecules = tp_count + fp_count in
    (nb_molecules, (float tp_count) /. (float nb_molecules))

  (* enrichment rate at x (e.g. x = 0.01 --> ER @ 1%) given a list
     of unsorted score-labels
     returns: (top_n, top_actives_rate, rand_actives_rate, enr_rate) *)
  let enrichment_factor (p: float) (score_labels: SL.t list) =
    let nb_molecules, rand_actives_rate = actives_rate score_labels in
    let top_n = BatFloat.round_to_int (p *. (float nb_molecules)) in
    let top_p_percent_molecules =
      L.take top_n (rank_order_by_score score_labels) in
    let _, top_actives_rate = actives_rate top_p_percent_molecules in
    let enr_rate = top_actives_rate /. rand_actives_rate in
    enr_rate

  (* Cf. http://jcheminf.springeropen.com/articles/10.1186/s13321-016-0189-4
     for formulas:
     The power metric: a new statistically robust enrichment-type metric for
     virtual screening applications with early recovery capability
     Lopes et. al. Journal of Cheminformatics 2017 *)
  let power_metric (cutoff: float) (scores_tot: SL.t list): float =
    let nb_actives l =
      L.length (L.filter SL.get_label l)
    in
    assert(cutoff > 0.0 && cutoff <= 1.0);
    let size_tot = float (L.length scores_tot) in
    let x = BatFloat.round (cutoff *. size_tot) in
    let size_x = int_of_float x in
    assert(size_x >= 1);
    let sorted = rank_order_by_score scores_tot in
    let scores_x = L.take size_x sorted in
    let actives_x = float (nb_actives scores_x) in
    let actives_tot = float (nb_actives scores_tot) in
    let tpr_x = actives_x /. actives_tot in
    let fpr_x = (x -. actives_x) /. (size_tot -. actives_tot) in
    tpr_x /. (tpr_x +. fpr_x)

end
