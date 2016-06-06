(*
 *Universidad Nacional Autónoma de México
 *Facultad de Ciencias
 *Programación Declarativa
 *Proyecto 02: Codigos de Huffman
 *)

(**Implementacion de los arboles de huffman*)

(**Declaramos el arbol de huffman*)
type 'a htree = Leaf of 'a*int | Branch of 'a htree* int * 'a htree;;

(*Funcion para facilitar el mapeo de arboles*)
let map_h xs = let leaf (x,y) = Leaf (x,y) in
               List.map leaf xs;;

(**Funcion que obtiene el valor de cada nodo u hoja*)
let extract = function
    Leaf (_,i) -> i
   |Branch (_,i,_) -> i;; 

(**Operador «mayor que» para arboles de huffman*)
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

(**Funcion que dado una tabla de frecuencias crea un arbol de huffman*)
let create xs = huffman_algo xs;;
  
(**Funcion que da un tabla de codificacion de un alfabeto dado un arbol de huffman*)
let code_table tree =
  let rec aux xs = function
      Leaf (x,_) -> [(x,xs)]
     |Branch (l,_,r) -> aux (xs@[0]) l @ aux (xs@[1]) r in
  aux [] tree;;

(**Funcion que descomprime un archivo*)
let decode tree list =
  let rec aux t lc l =
    match (t,lc) with
      (Leaf (x,_),ls) -> aux tree ls (l@[x])
     |(_,[]) -> l
     |(Branch(x,_,_),'0'::xs) -> aux x xs l
     |(Branch(_,_,x),'1'::xs) -> aux x xs l
     | _ -> failwith "Error: Esta lista contiene mas que 0's y 1's"  
  in
  aux tree list [];;
