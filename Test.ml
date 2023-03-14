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


    let make_test (g : 'a Generator.t) (r : 'a Reduction.t) (name: string) (p : 'a Property.t) :'a t =
        {name = name; generator = g; reduction = r; property = p} ;;
      
       
    let check (n : int) (test : 'a t) :bool =  
       if n <= 0 then false
       else 
          let rec rec_check n = 
              if n = 0 then (Printf.printf "%s is true: " test.name; true)
             else if test.property (Generator.next test.generator) then rec_check (n - 1)
             else (Printf.printf "%s is false: " test.name; false)
          in 
          rec_check n ;;

    let rec find_first_not_satisfying (f : 'a -> bool) (l : 'a list) :'a option = 
       match l with
       | []   -> None
       | h::t -> if f h then find_first_not_satisfying f t else Some h ;;

    let fails_at (n : int) (test : 'a t) :'a option =
        if n <= 0 then None
        else
            let rec rec_fails_at n =
                if n = 0 then (Printf.printf "%s does not fail: " test.name; None)
                else
                    let val_gen = Generator.next test.generator in
                    let list_red = (test.reduction val_gen)@[val_gen] in
                    match find_first_not_satisfying test.property list_red with 
                    |Some x -> (Printf.printf "%s fails for the value: " test.name; Some x)
                    |None   -> rec_fails_at(n-1)
        in rec_fails_at n ;;

  
    let execute (n : int) (tests :'a t list) : ('a t * 'a option) list =
        List.map (fun test-> (test,fails_at n test)) tests
    ;;

  end ;;
