module Property :
  sig
    (** Type d'une propriété portant sur des éléments de type 'a
      * Une propriété est une fonction booléenne. *)
    type 'a t = 'a -> bool

    (* CONSTANTES *)

    (** Propriété toujours vérifiée *)
    val always_true  : 'a t

    (** Propriété jamais   vérifiée *)
    val always_false : 'a t

    (* ERREURS *)

    (** Propriété vraie si l'entrée n'est pas None *)
    val not_none: 'a option t

    (** Propriété vraie si l'entrée n'est pas Error *)
    val not_error: ('a, 'e) result t
  end =
  struct
    type 'a t = 'a -> bool ;;

    let always_true (x: 'a) : bool = 
              true ;;
    let always_false (x: 'a) : bool = 
              false ;;
    let not_none (x: 'a option): bool =
      Option.is_some x ;;
    let not_error (x :('a, 'e) result) : bool =
      Result.is_ok x ;;
  end ;;
