open Turing
open Tipi

let tiri = List.mapi (fun glava l -> { glava; trak = l |> List.to_seq |> Trak.of_seq })[
  [(0,Podatek 0);(1,Podatek 1);(2,Podatek 2);];
  [(0,Podatek 1);(1,Podatek 2);(2,Podatek 3);];
  [(0,Podatek 2);(1,Podatek 3);(2,Podatek 4);]
];;

(** unsafe  *)
let pod_to_int podatek = 
  match podatek with
  | Podatek p -> p
  | Prazno -> 0 
;;
(** unsafe *)
let tir_to_list tir = tir.trak |> Trak.bindings |> List.map (fun (i,v) -> (i,pod_to_int v));;

(* Testi za branje in pisanje na tire. *)

let%test_unit "preberi_tire" = [%test_eq: Base.int Base.list] 
  ( tiri |> NDVT.Helper.preberi_tire |> List.map (pod_to_int) ) 
  ([0;2;4])
;;

let%test_unit "posodobi_tire" = [%test_eq: Base.int Base.list] 
  ( 
    (NDVT.Helper.posodobi_tire tiri [Podatek 3,Desno; Podatek 3,Levo; Podatek 10, N]) 
    |> NDVT.Helper.preberi_tire |> List.map (pod_to_int) 
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