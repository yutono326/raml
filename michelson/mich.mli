exception Invalid_argument

type ct
and t


val failwith : t list -> t list

val seq : (t list -> t list) list -> t list -> t list

val contract : t -> t list -> t list

val transfer_tokens : t list -> t list
