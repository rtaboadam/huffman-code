(*
 *Universidad Nacional Autónoma de México
 *Facultad de Ciencias
 *Programación Declarativa
 *Proyecto 02: Codigos de Huffman
 *)

(**Aqui se implementa las colas de prioridades necesarias para el proyecto*)

(**Definimos el signatura SET*)
module type SET = sig
    (**Tipo de la signatura*)
    type 'a t 
    val create: unit -> 'a t 
    (**Funcion que crea un valor SET*)
    val add: 'a -> 'a t -> unit
    (**Funcion que agrega un elemento a un tipo SET*)
    val to_list: 'a t -> ('a*int) list
    (**Funcion que convierte un SET a una lista*)
end 

module PQueue = struct
  (**Asi se declara el tipo del modulo*)
  type 'a t = ('a option*int array) ref;;

  (**Elemento de una pqueue*)
  let single = (None,-1);;

  (**Funcion que crea un pqueue*)
  let create () = ref [|None,-1|];;

  (**Funcion que duplica el tamaño de un pqueue*)
  let double pqueue =
    let len = Array.length !pqueue in
    let new' = Array.make len single in
    pqueue := Array.append !pqueue new';;

  (**Funcion auxiliar de add*)
  let rec add' a array n =
    match array.(n) with
      (Some x, v) when x = a -> array.(n) <- (Some x, v + 1)
     |(None,_) -> array.(n) <- (Some a, 1)
     | _ -> add' a array (n+1);;
    
  (**Funcion que permite agregar un elemento a un pqueue*)  
  let add a pqueue =
    try
      add' a !pqueue 0
    with e ->
      double pqueue;
      let len = Array.length !pqueue in
      add' a !pqueue (len -1);;

  (**Funcion que ordena un pqueue*)
  let sort pqueue =
    let f (x,n) (y,m) = m - n in
    Array.sort f !pqueue;;

  (**Funcion que devuelve una lista de los elementos ordenados*)
  let to_list pqueue =
    sort pqueue;
    let list = Array.to_list !pqueue in
    let list_f = List.filter (fun (x,y) -> not (x = None)) list in
    let map_fun = function None,_ -> failwith "error" | Some x,y -> x,y in
    List.rev (List.map map_fun list_f);;

  (**Funcion que dada una lista de char regresa una lista de char*int*)
  let table list_c =
    let pqueue = create () in
    let rec aux list pqueue =
      match list with
        [] -> to_list pqueue
       |x::xs -> add x pqueue;aux xs pqueue in
    aux list_c pqueue;;
end
    
(*module PQueue1 = (PQueue:SET);;*)  
