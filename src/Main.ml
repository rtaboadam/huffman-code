(*Universidad Nacional Autónoma de México
 *Facultad de Ciencias
 *Progrmación Declarativa
 *Proyecto02:Códigos de Huffman*)

open BManage
open Huffman
open Parser
open PQueue
open Printf

(**Modulo principal del proyecto*)


(**Funcion de que indica cuando se comprimo un archivo*)
let message x y = printf "Se comprimieron %d bytes a %d bytes\n" x (y/8);; 

(**Funcion para guardar la tabla de frecuencias*)
let save_table table output =
  let row chan (x,y) = fprintf chan "%c,%d\n" x y in
  let oc = open_out output in
  let rec save = function
      [] -> close_out oc
     |('\n',k)::xs ->  fprintf oc "%s,%d\n" "\\n" k; save xs
     |x::xs -> row oc x; save xs in
  save table;;
  
(**Funcion que comprime un archivo, tambien genera la tabla de frecuencia*)
let compress filename output output1 =
  let archive = Parser.read filename in
  let table = PQueue.table archive in
  let tree = Huffman.create table in
  let table_c = Huffman.code_table tree in
  let to_write = BManage.encode archive table_c in
  message (extract tree) (String.length to_write);
  save_table table output1;
  Parser.write output to_write;;

(**Funcion que descomprime una archivo*)
let descompress filename tree' output =
  let archive = Parser.read filename in
  let table = Parser.save_table tree' in
  let tree = Huffman.create table in
  let l = List.filter (fun x -> x = '0' || x = '1') archive in
  let list_c = Huffman.decode tree l in
  let funchar = fun x -> if x = '\n' then "\n" else Char.escaped x in
  let to_write = String.concat "" (List.map funchar list_c) in
  Sys.remove tree';
  Sys.remove filename;
  Parser.write output to_write;;

let warning ="No se encontro el archivo";;
let help = "Modo de uso:\n\t huff [-c|-d] archive (archive_tree)";;

(**Funcion con la cual obtiene un nombre*)
let obtain_name xs =
  let rev = List.rev xs in
  let name = List.rev (List.tl rev) in
  String.concat "" name;;
  
let () =
  try
    let opt = Sys.argv.(1) in
    let name = Sys.argv.(2) in
    match opt with
      "-c" -> let output = (String.concat "" [name;".hff"]) in
	      let tree = (String.concat "" [name;".tree"]) in
	      compress name output tree
     |"-d" -> let tree = Sys.argv.(3) in
	      let name' = Parser.split_char '.' name in
	      let check =Parser.split_char '.' tree in
	      let verify= List.nth check ((List.length check) -1)in
	      let verify2 = List.nth name' ((List.length name')-1)in
	      if verify = "tree" && verify2 = "hff" then
		descompress name tree (obtain_name check)
	      else
		print_endline "Archivo no valido"
     | _ -> print_endline "Error de parametros";
	    print_endline help
  with Invalid_argument x -> print_endline warning;
			     print_endline help
     |Sys_error x -> print_endline help





   
