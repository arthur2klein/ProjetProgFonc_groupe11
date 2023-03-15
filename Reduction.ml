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

    let int (n: int): int list =
        if (n = 0) then []
        else if (n >= 0) then [0; n * 9 / 10]
        else [0; n * 9 / 10; -n]
   ;;

    let int_nonneg (n: int): int list =
        if (n = 0) then []
        else [0; n * 9 / 10]
    ;;

    let add_reduced_float (x: float) (l: float list): float list =
        (x *. 0.9) :: l
    ;;

    let add_dec_if_sup_1 (x: float) (l: float list): float list =
        if (x <= 1.) then l
        else (x -. Float.floor x) :: l
    ;;

    let add_floor_if_not_integer (x: float) (l: float list): float list =
        if (Float.is_integer x) then l
        else (Float.floor x) :: l
    ;;

    let add_positive_if_negative (x: float) (l: float list): float list =
        if (x >= 0.) then l
        else (-.x) :: l
    ;;

    let float (x: float): float list =
        let res = [] in
        if (x = 0.) then res
        else
            0. :: add_dec_if_sup_1
                x
                (add_floor_if_not_integer
                    x
                    (add_positive_if_negative
                        x
                        (add_reduced_float
                            x
                            res
                        )
                    )
                )
    ;;

    let float_nonneg (x: float): float list =
        let res = [] in
        if (x = 0.) then res
        else
            0. :: add_dec_if_sup_1
                x
                (add_floor_if_not_integer
                    x
                    (add_reduced_float
                        x
                        res
                    )
                )
    ;;

    let char (c: char): char list =
        if (c = '\000') then []
        else
            ['\000'; Char.chr (Char.code c - 1)]
    ;;

    let alphanum (c: char): char list =
        if ('0' < c && c <= '9') then
            ['0'; Char.chr (Char.code c - 1)]
        else if ('a' < c && c <= 'z') then
            ['a'; Char.chr (Char.code c - 1)]
        else if ('A' < c && c <= 'Z') then
            ['A'; Char.chr (Char.code c - 1)]
        else
            []
    ;;

    let rec list_sub_aux (l: 'a list) (deb: int) (fin: int) (res: 'a list): ('a list) =
        match l with
        | [] -> res
        | h :: t ->
            if (deb > 0) then
                list_sub_aux t (deb - 1) (fin - 1) res
            else if (fin > 0) then
                list_sub_aux t deb (fin - 1) (h::res)
            else
                res
    ;;

    let list_sub (l: 'a list) (deb: int) (fin: int): ('a list) =
        list_sub_aux l deb fin []
    ;;
    
    let reduce_element (red: 'a -> 'a list) (l: 'a list) (i: int): ('a list) list =
        let new_element = red (List.nth l i)
        and pre_list = list_sub l 0 (i - 1)
        and post_list = list_sub l (i + 1) (List.length l - 1) in
        List.map
            (fun element ->
                pre_list @ (element :: post_list)
            )
            new_element
    ;;

    let reduce_elements (red: 'a -> 'a list) (l: 'a list): ('a list) list =
        List.flatten
            (List.init
                (List.length l)
                (reduce_element red l)
            )
    ;;

    let reduce_size (l: 'a list): ('a list) list =
        List.init
            (List.length l)
            (fun i ->
                (list_sub
                    l
                    0
                    (i - 1)
                ) @ (list_sub
                    l
                    (i + 1)
                    (List.length l - 1)
                )
            )
    ;;

    let string_from_list (l: char list): string =
        String.concat
            ""
            (List.map
                (String.make 1)
                l
            )
    ;;
    
    let string_to_list (s: string): char list =
        List.init
            (String.length s)
            (String.get s)
    ;;

    let list (red: 'a -> 'a list) (l: 'a list): ('a list) list =
        [] :: (reduce_size l) @ (reduce_elements red l)
    ;;

    let string (red: char -> char list) (s: string): string list =
        List.map
            string_from_list
            (list
                red
                (string_to_list s)
            )
    ;;

    let combine (fst_red: 'a -> 'a list) (snd_red: 'b -> 'b list) ((x, y): ('a * 'b)): ('a * 'b) list =
        List.flatten
            (List.map
                (fun element1 ->
                    List.map
                        (fun element2 ->
                            (element1, element2)
                        )
                        (snd_red y)
                )
                (fst_red x)
            )
    ;;

    let filter (p: 'a -> bool) (red: 'a -> 'a list) (x: 'a): 'a list =
        List.filter
            p
            (red x)
    ;;

  end ;;
