open Otm_lib

exception CompilationError

let preberi_file filename =
  let in_channel = In_channel.open_text filename in
  let rec aux acc = 
    match In_channel.input_line in_channel with
    | None -> List.rev acc
    | Some line -> aux (line::acc) in
  aux []
;;

let print_error ?(line_num=None) del error_text = 
  
  let del = match line_num with
    | None    -> del
    | Some i  -> del^" [vrstica "^ (string_of_int (i+1))^"]" in

  prerr_endline ("[!] Napaka: " ^ del);
  prerr_string "|-> ";
  prerr_endline error_text; print_newline ()
;;

let print_info del info_text = 
  print_endline ("[@] Obvestilo: " ^ del);
  print_string "|-> ";
  print_endline info_text; print_newline ()
;;

(** Funkcija prevede datoteko na lokaciji [filename]  *)
let prevedi filename = 
  (* najprej preberemo file *)
  let vrstice = 
    try preberi_file filename with
      _ -> begin 
      print_error "branje" "Napaka pri branju datoteke.";
      raise CompilationError end in
  
  (* ga parsiramo *)
  let parsirano = 
    try Parser.parsiraj vrstice with 
      | Parser.InvalidPosNumber i -> begin 
        print_error ~line_num:(Some i) "parsiranje" "Pričakovano pozitivno število.";
        raise CompilationError end
      | Parser.InvalidBrackets i -> begin 
        print_error ~line_num:(Some i) "parsiranje" "Nepravilni oklepaji.";
        raise CompilationError end
      | Parser.InvalidDirection i-> begin 
        print_error ~line_num:(Some i) "parsiranje" "V tranzcijah se pojavi neveljavna smer.";
        raise CompilationError end
      | Parser.InvalidTransition i-> begin 
        print_error ~line_num:(Some i) "parsiranje" "Sintaktična napaka pri definiciji tranzicije.";
        raise CompilationError end
      | Parser.ParseException i -> begin 
        print_error ~line_num:(Some i) "parsiranje" "Splošna sintaktična napaka.";
        raise CompilationError end in
  
  (* in nazadnje prevedemo *)
  let turing =
    try Compiler.prevedi_tokene parsirano with
      | Compiler.MissingDefinitions str -> begin 
        print_error "prevajanje" ("Manjkajoče ključno navodilo: \""^str^"\".");
        raise CompilationError end
      | Compiler.MultipleDefinitions str -> begin 
        print_error "prevajanje" ("Podvojeno ključno navodilo: \""^str^"\".");
        raise CompilationError end
      | Compiler.MissingTranstion (a,b) -> begin 
        let str = "(" ^ (string_of_int a) ^ "/" ^ (string_of_int b) ^ ")" in
        print_error "prevajanje" ("Manjkajoče tranzicije, prisotnih le " ^ str);
        raise CompilationError end
      | Compiler.UnknownSymbol sym -> begin 
        print_error "prevajanje" ("V tranzicijah se pojavi simbol: \""^sym^"\", ki ni definiran.");
        raise CompilationError end in turing
;;

if Array.length Sys.argv <> 2 then
  print_error "uporaba" "Neveljavni/manjkajoči argumenti\n\nUporaba: [pot_do_datoteke]"
else
  let filename = Sys.argv.(1) in
  try
    let _ = prevedi filename in
    print_info "parsiranje" "Uspešno parsirano."
  with _ -> ()