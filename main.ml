open Printf
open Scanf

(*ファイル名*)
let input_file = "textcolor.csv"
let output_file_satyh = "csv.satyh"

let cmd_list = ref []

(*input_fileから一行ずつ取得してcmd_listに格納*)
let cat filename =
  let fin = open_in filename in
  let rec cat_sub () =
    cmd_list := (input_line fin) :: !cmd_list;
    cat_sub ()
  in
    try cat_sub () with End_of_file -> close_in fin

let () = cat input_file

let () = cmd_list := List.rev !cmd_list


(*(color, inline-text)ではない形のものを削除する*)
let reg = Str.regexp "[a-zA-Z]+,[a-zA-Z]+"
let hoge r s = Str.string_match r s 0
let cmd_list_2 = List.filter (hoge reg) !cmd_list


(*color部分とinline-text部分を抽出*)
let reg_cmd_color = Str.regexp "\\([a-zA-Z]*\\).*"
let reg_cmd_it = Str.regexp ".*,\\([a-zA-Z]*\\)"
(*inline-text部分に中括弧を付ける*)
let fuga_1 s =
  "(" ^ (Str.replace_first reg_cmd_color "\\1" s) ^ ", " ^
  (Str.replace_first reg_cmd_it "{\\1}" s) ^ ");"
let cmd_list_3 = List.map fuga_1 cmd_list_2


let message_list = cmd_list_3


let message_satyh =
    List.map (fun s -> s ^ "\n") message_list
      |> List.fold_left (^) ""


(*.satyファイルに書き出す内容*)
let output_list_satyh = [
  "@import: color";
  "let csvlist = [";
  message_satyh;
  "]"
  ]

let output_satyh =
  List.map (fun s -> s ^ "\n") output_list_satyh
    |> List.fold_left (^) ""


(*.satyhファイルに書き出す*)
let () =
  let oc_satyh = open_out output_file_satyh in
  fprintf oc_satyh "%s" output_satyh;
  close_out oc_satyh

