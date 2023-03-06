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
    val find_first_not_satisfying : 'a -> bool -> 'a list ->'a option 
    val generate_and_verify : 'a t -> 'a option  

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
    } ;;


    let make_test (g : 'a Generator.t) (r : 'a Reduction.t) (p : 'a Property.t) :'a t =
       {g; r; p} ;;
      
       
    let generate_and_verify (test : 'a t) :'a option =
       let val_gen = test.generator() in 
       if test.property val_gen then Some val_gen 
       else None ;;

    
    let check (n : int) (test : 'a t) :bool =  
       if n <= 0 then false
       else 
          let rec rec_check n = 
             if n = 0 then true 
             else 
                match generate_and_verify test with 
                   | Some _ ->  rec_check (n - 1)
                   | None   -> false
          in 
          rec_check n ;;



    let rec find_first_not_satisfying : (f : 'a -> bool) (l : 'a list) :'a option = 
       match l with
       | []   -> None
       | h::t -> if f h then find_first_not_satisfying f t else Some h ;;
    
    

    let fails_at (n : int) (test : 'a t) :'a option =
       if n <= 0 then None
       else
          let rec rec_fails_at n =
	     if n = 0 then None 
             else
                let val_gen = test.generator() in
                if not(test.property val_gen) then Some val_gen 
                else  
                   let list_red = test.reduction val_gen in
                   match find_first_not_satisfying test.property list_red with 
                   |Some x -> Some x
                   |None   -> rec_fails_at(n-1)
         in
         rec_fails_at n ;;

  
   let execute (n : int) (tests :'a t list) : ('a t * 'a option) list =
	let list_result = List.map (fun test-> (test,fails_at n test)) tests in
	List.fold_left
	(fun lst (test, value)-> 
	match value with
	| None   -> lst
	| Some x -> (test, Some x) :: lst
	) 
	[]
	list_result
	;;

  end ;;
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
   

