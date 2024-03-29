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

    (** Générateur pseudo-aléatoire de caractères alphanumériques *)
    val alphanum : char t

  (** Générateur pseudo-aléatoire de caractères en majuscule *)
    val majuscule : char t

   (** Générateur pseudo-aléatoire de caractères en minuscule *)
    val minuscule : char t 


    (* GENERATEURS DE CHAINE DE CARACTERE *)

    (** Générateur de chaînes de caractères
      * @param n   longueur maximale de la chaîne de caractère
      * @param gen générateur pseudo-aléatoire de caractères
      * @return    générateur pseudo-aléatoire de chaînes de caractères dont chaque caractéré est généré avec `gen`
      *)
    val string : int -> char t -> string t

    (* GENERATEURS DE LISTES *)

    (** Générateur de listes
      * @param n   longueur maximale de la liste
      * @param gen générateur pseudo-aléatoire d'éléments
      * @return    générateur pseudo-aléatoire de listes dont chaque élément est généré avec `gen`
      *)
    val list : int -> 'a t -> ('a list) t

    (* TRANSFORMATIONS *)

    (** Générateur pseudo-aléatoire de couples
      * @param fst_gen générateur pseudo-aléatoire de la première coordonnée
      * @param snd_gen générateur pseudo-aléatoire de la deuxième coordonnée
      * @return        générateur pseudo-aléatoire du couple
      *)
    val combine : 'a t -> 'b t -> ('a * 'b) t

    (** Applique un post-traitement à un générateur pseudo-aléatoire
      * @param f   post-traitement à appliquer à chaque valeur générée
      * @param gen générateur pseudo-aléatoire
      * @return    générateur pseudo-aléatoire obtenu en appliquant `f` à chaque valeur générée par `gen`
      *)
    val map : ('a -> 'b) -> 'a t -> 'b t

    (** Applique un filtre à un générateur pseudo-aléatoire
      * @param p   filtre à appliquer à chaque valeur générée
      * @param gen générateur pseudo-aléatoire
      * @return    générateur pseudo-aléatoire ne générant des valeurs de `gen` que si elles vérifient `p`
      *)
    val filter : ('a -> bool) -> 'a t -> 'a t

    (** Applique un post-traitement dépendant d'un filtre à un générateur pseudo-aléatoire
      * @param p   filtre à appliquer à chaque valeur générée
      * @param f   couple des post-traitements à appliquer à chaque valeur générée
      * @param gen générateur pseudo-aléatoire
      * @return    générateur pseudo-aléatoire obtenu en appliquant `fst f` pour toute valeur vérifiant `p`
      *                                                          et `snd f` pour toute valeur ne le vérifiant pas
      *)
    val partitioned_map : ('a -> bool) -> (('a -> 'b) * ('a -> 'b)) -> 'a t -> 'b t
  end =
  struct

    (*on definit un type 'a t , il s'agit d'une fonction qui ne prend pas d'argument unit et renvoie une valeur de type 'a *) 
  type 'a t = unit -> 'a ;;

  let next (gen : 'a t) : 'a = gen () ;;

  let const (x : 'a) : 'a t = fun () -> x ;; 

  let bool (prob : float) : bool t =
      fun() -> Random.float 1.0 < prob ;;

  let int (a : int)(b : int) : int t =
      fun() -> Random.int (b - a + 1) + a ;;

  let int_nonneg (n : int) : int t =
      fun() -> Random.int n ;; 

  let float (x: float)(y: float): float t =
      fun() -> Random.float (y -. x) +. x ;; 

  let float_nonneg (x: float ) : float t = 
      fun() -> Random.float x ;;
  
  (*La fonction Char.chr est utilisée pour convertir un entier en caractère*)
  let char : char t = 
      fun() -> Char.chr (Random.int 255) ;;

  let alphanum : char t =
      fun() ->
          let value = (int 0 61) () in
          if (0 <= value && value <= 9) then
              Char.chr (value + Char.code '0') 
          else if (10 <= value && value <= 35) then
              Char.chr (value - 10 + Char.code 'a') 
          else
              Char.chr (value - 36 + Char.code 'A') ;;

  let string (n : int)(gen : char t) : (string t) =
      fun() ->
          String.concat "" (List.init n (fun _ -> String.make 1 (gen ()))) ;;

  let list (n: int) (gen: 'a t): ('a list) t =
      fun () ->
          List.init
            n
            (fun _ -> gen());;

  let majuscule = 
    fun () ->
        let lettre = Random.int  26 in 
        char_of_int (lettre+65) ;; 

  let minuscule = 
    fun () ->
        let lettre = Random.int  26 in 
        char_of_int (lettre+97) ;; 
  

  (* TRANSFORMATIONS *) 

  let combine (fst_gen : 'a t) (snd_gen : 'b t) : ('a * 'b) t =
      fun() -> (fst_gen(), snd_gen());; 
  
  let map (f :'a -> 'b ) (gen :'a t) : 'b t = fun() -> f(gen()) ;; 

  let rec filter (p : 'a -> bool) (gen : 'a t) :'a t= 
      fun() ->
          let x = gen() in 
          if p x then x else filter p gen() ;; 
    
  let partitioned_map (p : 'a -> bool) ((f, g) : ('a -> 'b) * ('a -> 'b)) (gen : 'a t) : 'b t = 
      fun() ->
          let val_gen = gen() in
          if p val_gen then
              f val_gen
          else g val_gen;;

  end;;
