module Reduction :
  sig
    (** Type d'une stratégie de réduction des éléments de type 'a
      * Une stratégie associe à chaque valeur une liste de propositions plus "simples".
      * NB : Les propositions sont ordonnées dans l'ordre croissance de "simplicité"
      *      (i.e. les valeurs les plus "simples" sont en début de liste).
      * IMPORTANT : Les stratégies implémentées respectent les conditions des générateurs correspondants.
      *)
    type 'a t = 'a -> 'a list

    (** La stratégie vide : ne renvoie aucune proposition de réduction *)
    val empty : 'a t

    (* TYPES DE BASE *)

    (** Stratégie de réduction sur les entiers
      * @param n entier
      * @return  liste d'entiers plus "simples" entre `-|n|` et `|n|`
      *)
    val int : int t

    (** Stratégie de réduction sur les entiers positifs
      * @param n entier positif
      * @return  liste d'entiers naturels plus "simples" entre 0 et `n`
      *)
    val int_nonneg : int t

    (** Stratégie de réduction sur les flottants
      * @param x flottant
      * @return  liste de flottants plus "simples" entre `-|x|` et `|x|`
      *)
    val float : float t

    (** Stratégie de réduction sur les flottants positifs
      * @param x flottant positif
      * @return  liste de flottants positifs plus "simples" entre `0` et `x`
      *)
    val float_nonneg : float t

    (** Stratégie de réduction sur les caractères
      * @param c caractère
      * @return  liste de caractères plus "simples"
      *)
    val char : char t

    (** Stratégie de réduction sur les caractères alphanumériques
      * @param c caractère alphanumérique
      * @return  liste de caractères alphanumériques plus "simples"
      *)
    val alphanum : char t

    (* CHAINES DE CARACTERES *)

    (** Stratégie de réduction sur les chaînes de caractères
      * @param red stratégie de réduction à utiliser sur chaque caractère
      * @param s   chaîne de caractères
      * @return    liste de chaînes de caractères plus "simples" au pire aussi longues que `s`
      *)
    val string : char t -> string t

    (* LISTES *)

    (** Stratégie de réduction sur les listes
      * @param red stratégie de réduction à utiliser sur chaque élément
      * @param l   liste
      * @return    liste de listes plus "simples" au pire aussi longues que `l`
      *)
    val list : 'a t -> ('a list) t

    (* TRANSFORMATIONS *)

    (** Stratégie de réduction sur les couples
      * @param fst_red stratégie de réduction de la première coordonnée
      * @param snd_red stratégie de réduction de la deuxième coordonnée
      * @return        stratégie de réduction sur les couples correspondants
      *)
    val combine : 'a t -> 'b t -> ('a * 'b) t

    (** Applique un filtre à une stratégie de réduction
      * @param p   filtre à appliquer à chaque réduction
      * @param red stratégie de réduction
      * @return    stratégie de réduction ne contenant que des propositions vérifiant `p`
      *)
    val filter : ('a -> bool) -> 'a t -> 'a t
  end =
  struct
    type 'a t = 'a -> 'a list ;;

    let empty (n: 'a): 'a list = 
        []
    ;;

    let iterate (f: 'a -> 'a) (x: 'a) (n: int): ('a list) =
        let rec aux (f: 'a -> 'a) (x: 'a) (n: int) (res: 'a list): ('a list) =
            if (n <= 0) then
                res
            else
                x::(List.map f res)
        in aux f x n []
    ;;

    let rec iterate_while (f: 'a -> 'a) (x: 'a) (p: 'a -> bool): ('a list) =
        if (not @@ p x) then
            []
        else
            x :: iterate_while f (f x) p
    ;;

    let plus_petit_f (f: float) (rapport: float): (float list) =
        List.rev @@
            iterate_while
                (fun x -> x *. rapport)
                f
                (fun x -> x > 1.)
    ;;

    let plus_petit_i (i: int) (dividende: int) (diviseur: int): (int list) =
        List.rev @@
            iterate_while
                (fun x -> x * dividende / diviseur)
                i
                (fun x -> x > 1)
    ;;

    let int (n: int): int list =
        [0] @
        (plus_petit_i n 9 10) @
        (plus_petit_i (-n) 9 10)
   ;;

    let int_nonneg (n: int): int list =
        [0] @
        (plus_petit_i n 9 10)
    ;;

    let float (x: float): float list =
        [0.; -1.; 1.; Float.floor x; Float.ceil x; x -. Float.floor x] @
        (plus_petit_f x 0.9) @
        (plus_petit_f (-.x) 0.9)
    ;;

    let float_nonneg (x: float): float list =
        [0.; 1.; Float.floor x; Float.ceil x; x -. Float.floor x] @
        (plus_petit_f x 0.9)
    ;;

    let char (c: char): char list =
        List.map
            Char.chr
            (List.init
                (Char.code c)
                Fun.id
            )
    ;;

    let alphanum (c: char): char list =
        if ('0' <= c && c <= '9') then
            List.map
                Char.chr
                (List.init
                    (Char.code c - Char.code '0')
                    (fun x -> x + Char.code '0')
                )
        else if ('a' <= c && c <= 'z') then
            List.map
                Char.chr
                (List.init
                    (Char.code c - Char.code 'a')
                    (fun x -> x + Char.code 'a')
                )
        else if ('A' <= c && c <= 'Z') then
            List.map
                Char.chr
                (List.init
                    (Char.code c - Char.code 'A')
                    (fun x -> x + Char.code 'A')
                )
        else
            []
    ;;

    let prefixes_l (l: 'a list): ('a list) list =
        List.fold_right
            (fun tete res_temp ->
                []::List.map
                (fun element ->
                    tete::element
                )
                res_temp
            )
            l
            [[]]
    ;;

    let rec prefixes_s (s: string) (n: int): string list =
        if (n <= 0) then
            []
        else
            (String.sub s 0 n) :: (prefixes_s s (n - 1))
    ;;

    let string (red: char -> char list) (s: string): string list =
        prefixes_s s (String.length s)
    ;;

    let list (red: 'a -> 'a list) (l: 'a list): ('a list) list =
        prefixes_l l
    ;;

    let combine (fst_red: 'a -> 'a list) (snd_red: 'b -> 'b list) ((x, y): ('a * 'b)): ('a * 'b) list =
        List.flatten @@
            List.map
                (fun element1 ->
                    List.map
                        (fun element2 ->
                            (element1, element2)
                        )
                        (snd_red y)
                )
                (fst_red x)
    ;;

    let filter (p: 'a -> bool) (red: 'a -> 'a list) (x: 'a): 'a list =
        List.filter
            p
            (red x)
    ;;

  end ;;
