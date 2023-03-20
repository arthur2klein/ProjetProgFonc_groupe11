module Reduction :
  sig
    (** Type d'une stratégie de réduction des éléments de type 'a
      * Une stratégie associe à chaque valeur une liste de propositions plus
      * "simples".
      * NB : Les propositions sont ordonnées dans l'ordre croissance de
      *      "simplicité"
      *      (i.e. les valeurs les plus "simples" sont en début de liste).
      * IMPORTANT : Les stratégies implémentées respectent les conditions des
      *             générateurs correspondants.
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
      * @return    liste de chaînes de caractères plus "simples" au pire aussi
      *            longues que `s`
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
      * @return    stratégie de réduction ne contenant que des propositions
      *            vérifiant `p`
      *)
    val filter : ('a -> bool) -> 'a t -> 'a t
  end =
  struct
    type 'a t = 'a -> 'a list ;;

    (* Si aucune simplification n'est possible, on ne propose rien *)
    let empty (n: 'a): 'a list = 
        []
    ;;

    (* Pour simplifier un entier relatif, on propose plusieurs valeurs entières
     * ayant un rapport infèrieur à 1 par rapport à l'entier de départ et on
     * essaie de se ramener à un entier positif aussi rapidement que possible
     * jusqu'à ce que 0 qui est considérée comme la valeur la plus simple.
     *)
    let int (n: int): int list =
        if      (n  = 0) then []
        else if (n >= 0) then [0;     n / 2; n * 3 /4 ; n * 7 / 8; n * 15 / 16]
        else                  [0; -n; n / 2; n * 3 /4 ; n * 7 / 8; n * 15 / 16]
   ;;

    (* Pour simplifier un entier naturel, on propose plusieurs valeurs entières
     * ayant un rapport infèrieur à 1 par rapport à l'entier de départ jusqu'à
     * atteindre 0 qui est considérée comme la valeur la plus simple.
     *)
    let int_nonneg (n: int): int list =
        if (n = 0) then []
        else            [0; n / 2; n * 3 /4 ; n * 7 / 8; n * 15 / 16]
    ;;

    (* Fonction privée ajoutant des valeurs infèrieures à un flottant à une
     * liste.
     *)
    let add_reduced_float (x: float) (l: float list): float list =
        [x *. 0.5; x *. 0.75; x *. 0.875; x *. 0.9375] @ l
    ;;

    (* Fonction privée ajoutant la partie fractionnaire d'un flottant à une
     * liste si ce flottant n'est pas déjà sa valeur fractionnaire.
     *)
    let add_dec_if_sup_1 (x: float) (l: float list): float list =
        if (x <= 1.) then
            l
        else
            (x -. Float.floor x) :: l
    ;;

    (* Fonction privée ajoutant la partie entière d'un flottant à une liste si
     * ce flottant n'est pas déjà entier.
     *)
    let add_floor_if_not_integer (x: float) (l: float list): float list =
        if (Float.is_integer x) then
            l
        else
            (Float.floor x) :: l
    ;;

    (* Fonction privée ajoutant la valeur absolue d'un flottant à une liste si
     * ce flottant est négatif.
     *)
    let add_positive_if_negative (x: float) (l: float list): float list =
        if (x >= 0.) then
            l
        else
            (-.x) :: l
    ;;

    (* Pour simplifier un nombre réel, on propose plusieurs valeurs ayant un
     * rapport infèrieur à 1 par rapport à la valeur de départ, ainsi que la
     * partie entière, la partie décimale et la valeur absolue de la valeur de
     * départ jusqu'à atteindre 0. qui est considérée comme la valeur la plus
     * simple.
     *)
    let float (x: float): float list =
        if (x = 0.) then
            []
        else
            0. :: add_dec_if_sup_1
                x
                (add_floor_if_not_integer
                    x
                    (add_positive_if_negative
                        x
                        (add_reduced_float
                            x
                            []
                        )
                    )
                )
    ;;

    (* Pour simplifier un nombre réel positif, on propose plusieurs valeurs
     * ayant un rapport infèrieur à 1 par rapport à la valeur de départ, ainsi
     * que la partie entière et la partie décimale de la valeur de départ
     * jusqu'à atteindre 0. qui est considérée comme la valeur la plus simple.
     *)
    let float_nonneg (x: float): float list =
        if (x = 0.) then
            []
        else
            0. :: add_dec_if_sup_1
                x
                (add_floor_if_not_integer
                    x
                    (add_reduced_float
                        x
                        []
                    )
                )
    ;;

    (* On simplifie un caractère avec la même méthode que la simplification
     * des entiers.
     *)
    let char (c: char): char list =
        List.map
            Char.chr
            (int_nonneg (Char.code c))
    ;;

    (* On simplifie un caractère alphanumérique, en proposant des valeurs plus
     * petites pour les chiffres, des lettres infèrieures dans l'alphabet pour
     * les lettres, ainsi que des minuscules pour les majuscules jusqu'à
     * atteindre 'a' ou '0' qui sont les valeurs les plus simples pour leurs
     * sous-catégories respectives.
     *)
    let alphanum (c: char): char list =
        if (('0' < c && c <= '9') || ('a' < c && c <= 'z')) then
            ['0'; Char.chr (Char.code c - 1)]
        else if ('A' <= c && c <= 'Z') then
            (* 32 est l'écart entre 'A' et 'a' *)
            ['A'; Char.chr (Char.code c + 32); Char.chr (Char.code c - 1)]
        else
            []
    ;;

    (* Fonction privée qui permet d'obtenir une sous-liste d'une liste de
     * départ.
     *)
    let list_sub (l: 'a list) (deb: int) (fin: int): ('a list) =
        let rec list_sub_aux
            (l  : 'a list)
            (deb:     int)
            (fin:     int)
            (res: 'a list)
        : ('a list) =
            match l with
            | [] -> res
            | h :: t ->
                if      (deb > 0) then
                    list_sub_aux t (deb - 1) (fin - 1) res
                else if (fin > 0) then
                    list_sub_aux t deb       (fin - 1) (h::res)
                else
                    res
        in list_sub_aux l deb fin []
    ;;

    (* Fonction privée qui simplifie une liste en créant des liste sous-listes
     * de taille n-1 de la liste de départ.
     *)
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
    
    (* Fonction privée qui simplifie une liste en créant des liste ou un seul
     * élément est simplifié à chaque fois.
     *)
    let reduce_elements (red: 'a -> 'a list) (l: 'a list): ('a list) list =
        let reduce_element
            (red: 'a -> 'a list)
            (l  :       'a list)
            (i  :           int)
        : ('a list) list =
            let new_element = red (List.nth l i)
            and pre_list    = list_sub l 0       (i - 1)
            and post_list   = list_sub l (i + 1) (List.length l - 1) in
            List.map
                (fun element ->
                    pre_list @ (element :: post_list)
                )
                new_element
        in List.flatten
            (List.init
                (List.length l)
                (reduce_element red l)
            )
    ;;

    (* Pour simplifier une liste, on propose tout d'abord des liste plus
     * petites, puis des listes ayant des valeurs plus simples.
     *)
    let list (red: 'a -> 'a list) (l: 'a list): ('a list) list =
        if l = [] then
            []
        else
            [] :: (reduce_size l) @ (reduce_elements red l)
    ;;

    (* Fonction privée convertissant une liste en chaîne de caractères. *)
    let string_from_list (l: char list): string =
        String.concat
            ""
            (List.map
                (String.make 1)
                l
            )
    ;;
    
    (* Fonction privée convertissant une chaîne de caractères en liste. *)
    let string_to_list (s: string): char list =
        List.init
            (String.length s)
            (String.get s)
    ;;

    (* On simplifie une chaîne de caractère comme une liste de caractères. *)
    let string (red: char -> char list) (s: string): string list =
        List.map
            string_from_list
            (list
                red
                (string_to_list s)
            )
    ;;

    (* On simplifie une paire en simplifiant un de ses éléments. *)
    let combine
        (fst_red: 'a -> 'a list)
        (snd_red: 'b -> 'b list)
        (x, y   :       'a * 'b)
    : ('a * 'b) list =
        List.map
            (fun new_y -> (x, new_y))
            (snd_red y)
        @ List.map
            (fun new_x -> (new_x, y))
            (fst_red x)
    ;;

    (* Pour filter une stratégie, on applique le filtre à chaque élément plus
     * simple proposé.
     *)
    let filter (p: 'a -> bool) (red: 'a -> 'a list) (x: 'a): 'a list =
        List.filter
            p
            (red x)
    ;;

  end ;;
