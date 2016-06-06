(*Universidad Nacional Autónoma de México
 *Facultad de Ciencias
 *Progrmación Declarativa
 *Proyecto02:Códigos de Huffman*)

open BManage
open Huffman
open Parser
open PQueue

(**Funcion Principal*)
let compress filename output =
  let archive = Parser.read filename in
  let table = PQueue.table archive in
  let tree = Huffman.create table in
  let table_c = Huffman.code_table tree in
  let to_write = BManage.encode archive table_c in
  Printf.printf "%s\n" to_write;
  Parser.write output to_write;;

let descompress filename tree = ();;

let () = let name = Sys.argv.(1) in
         compress name (String.concat "" [name;".hff"]);;
 
		     
    
