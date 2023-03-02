module Generator :
  sig
    (** Type du générateur pseudo-aléatoire de données de type 'a *)
    type 'a t

    (** Renvoie une nouvelle valeur aléeatoire
      * @param gen générateur pseudo-aléatoire
      * @return    nouvelle valeur aléatoire en utilisant `gen`
      *)
    val next : 'a t -> 'a

    (** Générateur constant
      * @param x valeur
      * @return  générateur de l'unique valeur `x`
      *)
    val const : 'a -> 'a t

    (* GENERATEURS DE TYPES DE BASE *)
 
    (** Générateur pseudo-aléatoire de booléens
      * @param prob probabilité de la valeur `true`
      * @return     générateur pseudo-aléatoire de valeurs booléennes
      *)
    val bool : float -> bool t

    (** Générateur pseudo-aléatoire d'entiers
      * @param a borne inférieure
      * @param b borne supérieure
      * @return  générateur pseudo-aléatoire de valeurs entières entre `a` et `b` inclus
      *)
    val int : int -> int -> int   t

    (** Générateur pseudo-aléatoire d'entiers positifs ou nuls
      * @param n borne supérieure
      * @return  générateur pseudo-aléatoire de valeurs entières entre 0 et `n` inclus
      *)
    val int_nonneg : int -> int   t

    (** Générateur pseudo-aléatoire de flottants
      * @param x borne supérieure
      * @param y borne supérieure
      * @return  générateur pseudo-aléatoire de valeurs flottantes entre `x` et `y` inclus
      *)
val float : float -> float -> float t

    (** Générateur pseudo-aléatoire de flottants positifs ou nuls
      * @param x borne supérieure
      * @return  générateur pseudo-aléatoire de valeurs flottantes entre 0 et `x` inclus
      *)
    val float_nonneg : float -> float t

    (** Générateur pseudo-aléatoire de caractères *)
    val char : char t

  end =
  struct
    (* TODO : Implémenter le type et tous les éléments de la signature *)

    (*on definit un type 'a t , il s'agit d'une fonction qui ne prend pas d'argument unit et renvoie une valeur de type 'a *) 
  type 'a t = unit -> 'a ;;

  let next (gen : 'a t) : 'a = gen () ;;

  let const (x : 'a) : 'a t = fun () -> x ;; 

  let bool (prob : float) : bool t =
  fun() -> Random.float 1.0 < prob ;;

  let int (a : int)(b : int) : int t =
  fun() -> Random.int (a - b + 1) + b ;;

  let int_nonneg (n : int) : int t =
  fun() -> Random.int n ;; 

  let float (x: float)(y: float): float t =
  fun() -> Random.float (x -. y +. 1.0) +. y ;; 

  let float_nonneg (x: float ) : float t = 
  fun() -> Random.float x ;;

  let char (): char =
      Char.chr @@ int_nonneg 255  @@ ()
  ;;
  end ;;
