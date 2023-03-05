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
      * @param prop propriété qui fait l'objet du test
      * @return     test créé
      *)
    val make_test : 'a Generator.t -> 'a Reduction.t -> 'a Property.t -> 'a t

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
       generator : 'a Generator.t;
       reduction : 'a Reduction.t;
       property  : 'a Property.t
    }


    let make_test (g : 'a Generator.t) (r :'a Reduction.t) (p :'a Property.t) : 'a t =
       { generator = g; reduction = r; property = p }
      
       
    let generate_reduce_verify (test: 'a t): 'a option =
       let val_gen = test.generator() in 
       let liste_red = test.reduction val_gen in 
       let liste_red_satisfy_property = List.for_all test.property liste_red in 
       if liste_red_satisfy_property then Some val_gen 
       else None 

    
    let check (n : int) (test : 'a t) : bool =  
       if n <= 0 then false
       else 
          let rec boucle n = 
             if n = 0 then true 
             else 
                match generate_reduce_verify test with 
                   | Some _ -> boucle (n-1)
                   | None -> false
          in 
          boucle n



  end ;;
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
   
