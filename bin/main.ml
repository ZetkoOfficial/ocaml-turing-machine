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
let prevedi ~force ~deterministic_only filename = 
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
  print_info "parsiranje" "Uspešno.";
  (* in nazadnje prevedemo *)
  let turing =
    try Compiler.prevedi_tokene ~force ~deterministic_only parsirano with
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
        raise CompilationError end 
      | Compiler.ExpectedDeterministic -> begin 
        print_error "prevajanje" "Pričakovan je bil determinističen turingov stroj, a to ni.";
        raise CompilationError end 
      | _ -> begin
        print_error "prevajanje" "Neznana napaka.";
        raise CompilationError end in

  print_info "prevajanje" "Uspešno.";
  turing
;;

let uporaba_str = "Uporaba:\notm <filename> [--(f)orce] [--(d)eterministic_only] [--(c)ompile_only]\n" in
let force =               ref false in
let deterministic_only =  ref false in
let compile_only =        ref false in
let extra =               ref [] in

let specifikacija = [
  ("--force", Arg.Set force, 
    " -> Prisili compiler da prevede kljub nekritičnim napakam(morda potem pride do runtime errorjev");
  ("-f", Arg.Set force, "");

  ("--deterministic_only", Arg.Set deterministic_only, 
    " -> Compiler preveri še da je turingova naprava deterministična");
  ("-d", Arg.Set deterministic_only, "");
    
  ("--compile_only", Arg.Set compile_only, 
    " -> Navodila za turingovo napravo ne požene, temveč le prevede");
  ("-c", Arg.Set compile_only, "");
] in

Arg.parse specifikacija (fun e -> extra := e::!extra) uporaba_str;

let force, deterministic_only, compile_only, extra = 
  !force, !deterministic_only, !compile_only, !extra in

match extra with
| [filename] -> begin
  try
    let _turing = prevedi ~force ~deterministic_only filename in
    if compile_only then exit 0
  with _ -> () end
| _ -> let _ = Arg.usage specifikacija uporaba_str in exit 1