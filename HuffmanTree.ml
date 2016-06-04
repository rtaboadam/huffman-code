(*
 *Universidad Nacional Autónoma de México
 *Facultad de Ciencias
 *Programación Declarativa
 *Proyecto 02: Codigos de Huffman
 *)

(**Aqui se implementa todo lo necesario para el algoritmo de huffman*)

(**Module que implementa los arboles de Huffman*)
module HTree= struct
  (**Declaramos el tipo del arbol de huffman*)
  type 'a htree = Leaf of 'a*int | Branch of 'a htree* int * 'a htree;;

  (**Modificacion de map para arboles de huffman*)
  let map_h xs = let leaf (x,y) = Leaf (x,y) in
              List.map leaf xs;;

  (**Funcion que obtiene el valor de cada nodo u hoja*)
  let extract = function
      Leaf (_,i) -> i
     |Branch (_,i,_) -> i;; 

  (**Operador mayor que para arboles de huffman*)
  let ( <* ) x y = extract x <= extract y;;

  (**Funcion que ordena una lista de arboles de huffman*)
  let rec sort xs = match xs with
      [] -> []
     |x::(y::xs) when y <* x -> y::(sort (x::xs))
     | _ -> xs;;

  (**Algoritmo de Huffman: Dado una tabla devuelve un arbol de huffman*)
  let rec huffman_algo xs =
    let hxs = map_h xs in
    let rec huffman = function
        [] -> failwith "No existe alfabeto para el algoritmo"
       |x::[] -> x
       |x::(y::xs) -> let sum = extract x + extract y in
                      huffman (sort (Branch(x,sum,y)::xs)) in
    huffman hxs;;

  (**Funcion que da un tabla de codificacion de un alfabeto dado un arbol de huffman*)
  let code_table tree =
    let rec aux xs = function
        Leaf (x,_) -> [(x,xs)]
       |Branch (l,_,r) -> aux (xs@[0]) l @ aux (xs@[1]) r in
    aux [] tree;;
                                           
end
                 
