type t

(** create a [Top_keeper.t] that will keep up to [n] best elements *)
val create: int -> t

(** [add name score] add score [score] to the top_keeper under name [name] *)
val add : string -> float -> t -> t

(** retrieves the [n] best score with name that were kept *)
val high_scores_first : t -> (float * string) list
