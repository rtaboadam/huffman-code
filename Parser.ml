(*
 *Universidad Nacional Autónoma de México
 *Facultad de Ciencias
 *Programación Declarativa
 *Proyecto 02: Codigos de Huffman
 *)

#use "topfind"
#require "extlib"

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

(**Funcion que convierte lee todo un archivo y lo transforma a una lista de char*)             
let char_list filename =
  let list = read_file filename in
  let rec iter = function
      [] -> []
     |x::xs -> explode x @ '\n'::iter xs in
  iter list;;
