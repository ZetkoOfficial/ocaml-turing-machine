open Turing
open Tipi

let tiri = List.mapi (fun glava l -> { glava; trak = l |> List.to_seq |> Trak.of_seq })[
  [(0,Podatek 0);(1,Podatek 1);(2,Podatek 2);];
  [(0,Podatek 1);(1,Podatek 2);(2,Podatek 3);];
  [(0,Podatek 2);(1,Podatek 3);(2,Podatek 4);]
];;

(** unsafe  *)
let pod_to_val def podatek = 
  match podatek with
  | Podatek p -> p
  | Prazno -> def
;;
(** unsafe *)
let tir_to_list tir = tir.trak |> Trak.bindings |> List.map (fun (i,v) ->   (i,pod_to_val 0 v));;
let tir_to_list_s tir = tir.trak |> Trak.bindings |> List.map (fun (i,v) -> (i,pod_to_val "0" v));;

(* Testi za branje in pisanje na tire. *)

let%test_unit "preberi_tire" = [%test_eq: Base.int Base.list] 
  ( tiri |> NDVT.Helper.preberi_tire |> List.map (pod_to_val 0) ) 
  ([0;2;4])
;;

let%test_unit "posodobi_tire" = [%test_eq: Base.int Base.list] 
  ( 
    (NDVT.Helper.posodobi_tire tiri [Podatek 3,Desno; Podatek 3,Levo; Podatek 10, N]) 
    |> NDVT.Helper.preberi_tire |> List.map (pod_to_val 0) 
  ) 
  ([1;1;10])
;;

let%test_unit "nastavi_input" = [%test_eq: ((Base.int*Base.int) Base.list) Base.list] 
  (
    let tiri' = NDVT.Helper.nastavi_input 0 [Podatek 6; Podatek 6; Podatek 6; Podatek 6; ] tiri in
    List.map (fun t -> tir_to_list t) tiri'
  )
  ([[0,6;1,6;2,6;3,6];[0,1;1,2;2,3];[0,2;1,3;2,4]])
;;

(* 
    Test determinstičnega stroja z enim tirom 
    (busy beaver 3 stanja 2 simbola iz https://en.wikipedia.org/wiki/Busy_beaver)     
*)

let tranzicije = [
  (Stanje "A", [Prazno]),     (Stanje "B", [Podatek 1,Desno]);
  (Stanje "A", [Podatek 1]),  (Stanje "H", [Podatek 1,Desno]);

  (Stanje "B", [Prazno]),     (Stanje "C", [Prazno,Desno]);
  (Stanje "B", [Podatek 1]),  (Stanje "B", [Podatek 1,Desno]);

  (Stanje "C", [Prazno]),     (Stanje "C", [Podatek 1,Levo]);
  (Stanje "C", [Podatek 1]),  (Stanje "A", [Podatek 1,Levo]);
]

let turing = NDVT.ustvari 1 (Stanje "A") [Stanje "H"] tranzicije;;

let%test_unit "pozeni busy_beaver" = [%test_eq: (Base.int*Base.int) Base.list ] 
  (
    match NDVT.pozeni turing [] with
    | Konec turing -> tir_to_list @@ List.hd turing.data.tiri
    | _ -> failwith "neuspešno"
  )
  ([ -1,1; 0,1; 1,1; 2,1; 3,1; 4,1 ])
;;

(* Testi za parsinarje *)

let%test_unit "v_strtokene" = [%test_eq: Base.string Base.list]
  (Parser.v_strtokene "A _ ->  B [1 - desno]")
  (["A";"_";"->";"B";"[1-desno]"])
;;

let%test_unit "v_strtokene" = [%test_eq: Base.string Base.list]
  (Parser.v_strtokene "A [_, B,C, D] ->  B [1 - desno]")
  (["A";"[_,B,C,D]";"->"; "B";"[1-desno]"])
;;

let%test_unit "v_seznam" = [%test_eq: Base.string Base.list]
  (Parser.v_seznam "[_,B,C,D]")
  (["_";"B";"C";"D"])
;;

let%test_unit "v_seznam" = [%test_eq: Base.string Base.list]
  (Parser.v_seznam "D")
  (["D"])
;;

let preberi_testfile filename = 
  let in_channel = In_channel.open_text ("example_files/"^filename) in
  let rec aux acc = 
    match In_channel.input_line in_channel with
    | None -> List.rev acc
    | Some line -> aux (line::acc) in
  aux []
;;

let%test "parsiraj" = 
  (Parser.parsiraj @@ preberi_testfile "primer1") = 
  ([
    Parser.Zacetek ("A", 1);
    Parser.Podatki (["_";"1"]);
    Parser.Tranzicija ( ("A", ["_"]),( "B", ["1",Parser.Desno]) );
    Parser.Tranzicija ( ("A", ["1"]),("!H", ["1",Parser.Desno]) );

    Parser.Tranzicija ( ("B", ["_"]),("C", ["_",Parser.Desno]) );
    Parser.Tranzicija ( ("B", ["1"]),("B", ["1",Parser.Desno]) );

    Parser.Tranzicija ( ("C", ["_"]),("C", ["1",Parser.Levo]) );
    Parser.Tranzicija ( ("C", ["1"]),("A", ["1",Parser.Levo]) );
  ])
;;

let%test "parsiraj" = 
  (Parser.parsiraj @@ preberi_testfile "primer2") = 
  ([
    Parser.Tranzicija ( ("test", ["a";"b";"c"]),( "s1", ["a",Parser.Levo;"b",Parser.Desno;"c",Parser.Ni]) );
    Parser.Tranzicija ( ("test", ["a";"b";"c"]),( "s2", ["a",Parser.Levo;"b",Parser.Desno;"c",Parser.Ni]) );

    Parser.Tranzicija ( ("test2", ["e";"f";"g"]),( "s1", ["a",Parser.Levo;"b",Parser.Desno;"c",Parser.Ni]) );
    Parser.Tranzicija ( ("test2", ["e";"f";"g"]),( "s2", ["a",Parser.Levo;"b",Parser.Desno;"c",Parser.Ni]) );
  ])
;;

(* Testi za prevajanje v turingov stroj *)

let parsirano = [
  Parser.Zacetek ("A", 1);
  Parser.Podatki (["_";"1"]);
  Parser.Tranzicija ( ("A", ["_"]),( "B", ["1",Parser.Desno]) );
  Parser.Tranzicija ( ("A", ["1"]),("!H", ["1",Parser.Desno]) );

  Parser.Tranzicija ( ("B", ["_"]),("C", ["_",Parser.Desno]) );
  Parser.Tranzicija ( ("B", ["1"]),("B", ["1",Parser.Desno]) );

  Parser.Tranzicija ( ("C", ["_"]),("C", ["1",Parser.Levo]) );
  Parser.Tranzicija ( ("C", ["1"]),("A", ["1",Parser.Levo]) );
]

let%test_unit "pozeni in prevedi parsiran busy_beaver" = [%test_eq: (Base.int*Base.string) Base.list ] 
  (
    let turing = Compiler.prevedi_tokene parsirano in 
    match NDVT.pozeni turing [] with
    | Konec turing -> tir_to_list_s @@ List.hd turing.data.tiri
    | _ -> failwith "neuspešno"
  )
  ([ -1,"1"; 0,"1"; 1,"1"; 2,"1"; 3,"1"; 4,"1" ])
;;