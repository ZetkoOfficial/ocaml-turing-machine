open Otm_lib
open Turing.Tipi

exception CompilationError

let preberi_file filename =
  let in_channel = In_channel.open_text filename in
  let rec aux acc = 
    match In_channel.input_line in_channel with
    | None -> List.rev acc
    | Some line -> aux (line::acc) in
  aux []
;;

let get_input () = 
  Logging.print_q "pogon programa" "vnesite input ločen s presledki:";
  let input = read_line () in
  Logging.ANSI.delete_line ();
  Logging.ANSI.delete_line ();
  input
;;

(** Funkcija prevede datoteko na lokaciji [filename]  *)
let prevedi ~force ~deterministic_only filename = 
  (* najprej preberemo file *)
  let vrstice = 
    try preberi_file filename with
      _ -> begin 
      Logging.print_error "branje" ["datoteke ni bilo mogoče prebrati."];
      raise CompilationError end in
  
  (* ga parsiramo *)
  let parsirano = 
    try Parser.parsiraj vrstice with 
      | Parser.InvalidPosNumber i -> begin 
        Logging.print_error ~line_num:(Some i) "parsiranje" ["pričakovano pozitivno število."];
        raise CompilationError end
      | Parser.InvalidBrackets i -> begin 
        Logging.print_error ~line_num:(Some i) "parsiranje" ["nepravilni oklepaji."];
        raise CompilationError end
      | Parser.InvalidDirection i-> begin 
        Logging.print_error ~line_num:(Some i) "parsiranje" ["v tranzcijah se pojavi neveljavna smer."];
        raise CompilationError end
      | Parser.InvalidTransition i-> begin 
        Logging.print_error ~line_num:(Some i) "parsiranje" ["sintaktična napaka pri definiciji tranzicije."];
        raise CompilationError end
      | Parser.ParseException i -> begin 
        Logging.print_error ~line_num:(Some i) "parsiranje" ["splošna sintaktična napaka."];
        raise CompilationError end in
  Logging.print_uspeh "parsiranje";

  (* in nazadnje prevedemo *)
  let turing =
    try Compiler.prevedi_tokene ~force ~deterministic_only parsirano with
      | Compiler.MissingDefinitions str -> begin 
        Logging.print_error "prevajanje" ["manjkajoče ključno navodilo: \""^str^"\"."];
        raise CompilationError end
      | Compiler.MultipleDefinitions str -> begin 
        Logging.print_error "prevajanje" ["podvojeno ključno navodilo: \""^str^"\"."];
        raise CompilationError end
      | Compiler.MissingTranstion (a,b) -> begin 
        let str = "(" ^ (string_of_int a) ^ "/" ^ (string_of_int b) ^ ")" in
        Logging.print_error "prevajanje" ["manjkajoče tranzicije, prisotnih le " ^ str];
        raise CompilationError end
      | Compiler.UnknownSymbol sym -> begin 
        Logging.print_error "prevajanje" ["v tranzicijah se pojavi simbol: \""^sym^"\", ki ni definiran."];
        raise CompilationError end 
      | Compiler.ExpectedDeterministic -> begin 
        Logging.print_error "prevajanje" ["pričakovan je bil determinističen turingov stroj, a to ni."];
        raise CompilationError end 
      | _ -> begin
        Logging.print_error "prevajanje" ["neznana napaka."];
        raise CompilationError end in
  Logging.print_uspeh "prevajanje";
  turing
;;

let pozeni turing = 
  let input = Str.split (Str.regexp " +") (get_input ()) in
  let input = List.map (fun p -> if p = "_" then Prazno else Podatek p) input in
  Logging.print_wait "pogon programa"; print_string "\r"; flush stdout;
  (* print_newline (); print_info "runtime" "Program se je pognal."; *)

  let turing = try Turing.NDVT.pozeni turing input with
    | RuntimeException -> begin
      Logging.print_error "pogon programa" ["program je dosegel neznano stanje/simbol."];
      raise RuntimeException end
    | _ -> begin
      Logging.print_error "pogon programa" ["neznana napaka."];
      raise RuntimeException end in

  Logging.print_uspeh "pogon programa";
  turing
;;  

(** Natisne prvi trak turingovega stroja.  *)
let print_prvi_trak turing = 
  let trak = Trak.bindings @@ (List.hd turing.data.tiri).trak in
  List.iter (fun (_,v) -> 
    match v with
    | Prazno    -> print_string "_ "
    | Podatek p -> print_string p; print_string " ";
  ) trak; print_newline ()
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
    let turing = prevedi ~force ~deterministic_only filename in
    if compile_only then exit 0;
    let turing = pozeni turing in 
    print_newline (); print_prvi_trak turing
  with _ -> exit 1 end
| _ -> let _ = Arg.usage specifikacija uporaba_str in exit 1