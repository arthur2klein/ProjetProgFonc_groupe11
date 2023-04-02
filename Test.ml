#use  "Property.ml" ;;
#use "Generator.ml" ;;
#use "Reduction.ml" ;;

module Test :
  sig
    (** Type d'un test portant sur des éléments de type 'a *)
    type 'a t

    (** Construit un test
      * @param gen  générateur pseudo-aléatoire de valeurs de test
      * @param red  stratégie de réduction
      * @param name nom du test
      * @param prop propriété qui fait l'objet du test
      * @return     test créé
      *)
    val make_test : 'a Generator.t -> 'a Reduction.t -> string -> 'a Property.t -> 'a t

    (** Effectue un test
      * @param n    nombre de valeurs à tester
      * @param test test à effectuer
      * @return     `true` si n > 0 et que toutes les valeurs à tester satisfont les conditions
      *)
    val check : int -> 'a t -> bool

    (** Liste toutes les valeurs réduites échouant au test
      * @param n    Nombre de valeurs à tester
      * @param test Test à effectuer
      * @return     Liste des valeurs réduites échouant au test
      *)
    val findall_reduced_not_satisfy: int -> 'a t ->'a list 

    (** Cherche une valeur simple ne vérifiant pas la propriété
      * @param n nombre de valeurs à tester
      * @return  `None` si toutes les valeurs de test générées par `gen` vérifient `prop`,
                 une valeur ne vérifiant pas `prop` (éventuellement en appliquant `red`) sinon
      *)
    val fails_at : int -> 'a t -> 'a option

    (** Exécute plusieurs tests
      * @param n     nombre de valeurs testées par test
      * @param tests liste des tests à vérifier
      * @return      tableau associatif des résultats
      *)
    val execute : int -> ('a t) list -> ('a t * 'a option) list


  end =
  struct
   
    type 'a t = {
       name: string;
       generator : 'a Generator.t;
       reduction : 'a Reduction.t;
       property  : 'a Property.t
    } ;;

   (** Cette fonction crée un test en utilisant un générateur, une fonction de réduction, 
     * une propriété et un nom. Elle renvoie le test créé
     *)
    let make_test (g : 'a Generator.t) (r : 'a Reduction.t) (name: string) (p : 'a Property.t) :'a t =
        {name = name; generator = g; reduction = r; property = p} ;;

   (** Cette fonction effectue une vérification de test en utilisant un test, 
     * un nombre de valeurs à tester et une propriété.
     * Si toutes les valeurs à tester satisfont les conditions.
     * Elle retourne true 
     *)   
    let check (n : int) (test : 'a t) :bool =  
       if n <= 0 then false
       else 
          let rec rec_check n = 
              if n = 0 then true
             else if test.property (Generator.next test.generator) then rec_check (n - 1)
             else false
          in 
          rec_check n ;;
   (** Cette fonction prend une fonction de comparaison et une liste. 
     * Si aucun élément de la liste ne satisfait la fonction, elle renvoie None,
     * sinon elle renvoie le premier élément qui ne satisfait pas la fonction.
     *)

    let rec find_first_not_satisfying (f : 'a -> bool) (l : 'a list) :'a option = 
       match l with
       | []   -> None
       | h::t -> if f h then find_first_not_satisfying f t else Some h ;;

   (** Cette fonction prend une fonction de comparaison, un générateur et un entier.
     * Elle génère une valeur aléatoire à partir du générateur 
     * et renvoie la valeur qui ne satisfait pas la fonction de comparaison.    
     *)
    let rec generate_not_satisfy (f: 'a -> bool) (gen: 'a Generator.t) (n: int): 'a option =
        if n = 0 then None
        else
            let val_gen = Generator.next gen in
            if (f val_gen) then generate_not_satisfy f gen (n - 1)
            else Some val_gen
    ;;

   (** Cette fonction prend une fonction de comparaison, une fonction de réduction et une valeur.
     * Si aucune valeur réduite de la valeur initiale ne satisfait la fonction,
     * elle renvoie la valeur initiale, sinon elle renvoie la valeur réduite qui ne 
     * satisfait pas la fonction de comparaison. 
     *)
    let rec reduce_not_satisfy (f: 'a -> bool) (red: 'a Reduction.t) (value: 'a): 'a =
        let list_reduced = red value in
        let value_reduced = find_first_not_satisfying f list_reduced in
        match value_reduced with
        | None -> value
        | Some x -> reduce_not_satisfy f red x
    ;;

   (** Cette fonction prend un nombre d'essais et un test.
     * Elle renvoie la valeur générée qui ne satisfait pas la propriété du test.
     *)
    let fails_at (n : int) (test : 'a t) :'a option =
        let value_gen = generate_not_satisfy test.property test.generator n in
        match value_gen with
        | None -> None
        | Some x -> Some(reduce_not_satisfy test.property test.reduction x)
    ;;

   (** Cette fonction prend un nombre d'essais et un test.
     * Elle renvoie une liste de valeurs réduites qui ne satisfont pas la propriété du test 
     *)
    let findall_reduced_not_satisfy (n : int) (test : 'a t) :'a list =
      if n <= 0 then []
      else
        let rec find_list_red_helper n =
          if n = 0 then []
          else
            let val_gen = Generator.next test.generator in
            let list_red = (test.reduction val_gen)@[val_gen] in
            let not_satisfying = List.filter (fun x -> not (test.property x)) list_red in
            match not_satisfying with
            | [] -> find_list_red_helper (n - 1)
            | _ -> not_satisfying @ (find_list_red_helper (n - 1))
        in find_list_red_helper n
    ;;

   (** Cette fonction prend un nombre d'essais et une liste de tests. 
     * Elle renvoie une liste de tuples contenant chaque test et son résultat   
     *)
    let execute (n : int) (tests :'a t list) : ('a t * 'a option) list =
        List.map (fun test-> (test,fails_at n test)) tests
    ;;
  end ;;
