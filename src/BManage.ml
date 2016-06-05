(*Universidad Nacional Autónoma de México
 *Facultad de Ciencias
 *Programación Declarativa
 *Proyecto 02: Codigos de Huffman
 *)
(**En este modulo va todo lo relacionado a bytes*)

(**Funcion que dado un char y una tabla devuelve su nuevo valor 
 *dado por el algoritmo*)
let rec findcode xs x = match xs with
    [] -> failwith ""
   |(y,t)::ys when y = x -> t
   | y::ys -> findcode ys x;;

(**Funcion que dado una lista de char y una tabla de frecuencias
 *regresa una lista de 0 y 1 que es la representacion en bits por
 *el algoritmo de Huffman*)
let encode list_c table =
  let list = List.map (findcode table) list_c in
  List.flatten list;;

(**Funcion que toma los primero n elementos de una lista*)
let rec take n xs = match (n,xs) with
    (0,_) -> []
   |(_,[]) -> []
   |(n,x::xs) -> x::(take (n-1) xs);;

(**Funcion que toma los primero n elementos de una lista*)
let rec drop n xs = match (n,xs) with
    (0,x) -> x
   |(_,[]) -> []
   |(n,x::xs) -> (drop (n-1) xs);;


  
(**Funcion que dado una lista de 0 y 1 te devuelve una lista de 
 *listas en las cuales cada una tiene 8 elementos*)
let only_eight list=
  let rec aux = function
      [] -> []
     |x -> (take 8 x)::(aux (drop 8 x)) in
  let fill xs = let len = List.length xs in
                if len < 8 then
                  xs@(Array.to_list (Array.make (8-len) 0))
                else xs in
  List.map (fill) (aux list);;
  
(**Funcion que dado una cadena de int de 0's y 1's regresa su numero
 *decimal*)
let num xs =
  let res = ref 0 in
  let len = List.length xs in
  for i = 0 to len-1 do
    res := !res + (List.nth xs i)*(int_of_float (2.**(float_of_int i)))
  done;!res;;


(**Funcion que dado una lista de lista de 0's y 1's regresa una lista
 *De numeros*)
let num_of_bin xs = List.map num xs;;

(**Funcion que dado una lista de numeros regresa un string*)
let to_write xs =
  let list_of_num = num_of_bin xs in
  let f = fun x -> Char.escaped (Char.chr x) in
  String.concat "" (List.map f list_of_num);;
