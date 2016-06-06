(*
 *Universidad Nacional Autónoma de México
 *Facultad de Ciencias
 *Programación Declarativa
 *Proyecto 02: Codigos de Huffman
 *)

(*
#use "topfind"
#require "extlib"
*)


(**Implementacion de un parser*)

(**Funcion que lee todo un archivo*)
let read_file filename =
  let chan = open_in filename in
  Std.input_list chan

(**Funcion que convierte un string a una lista de char*)
let explode s =
  let len = String.length s in
  let rec exp i l = if i < 0 then l else
                      exp (i-1) (s.[i]::l) in
  exp (len-1) [];;

(**Funcion que lee todo un archivo y lo transforma a 
 *una lista de char*)             
let read filename =
  let list = read_file filename in
  let rec iter = function
      [] -> []
     |x::xs -> explode x @ '\n'::iter xs in
  iter list;;


(**Funcion que escribe todo lo de un string*)
let write filename text =
  Std.output_file filename text;;

(**Funcion que separa en tokens*)
let rec split_char sep str =
  try
    let i = String.index str sep in
    String.sub str 0 i ::
      split_char sep (String.sub str (i+1) (String.length str - i - 1))
  with Not_found ->
    [str]

(***)
let save_table filename =
  let strings = read_file filename in
  let rec create ss = match ss with
      [] -> []
     |x::xs -> let list = split_char ',' x in
	       let len = List.length list in
	       if len > 2 then
		 (',',int_of_string (List.nth list (len-1)))::create xs
	       else
		 match List.hd list with
		   "\\n" -> ('\n',int_of_string (List.nth list (len-1)))::create xs
		  |_ -> ((List.hd list).[0],int_of_string (List.nth list (len-1)))::create xs
  in
  create strings;;
