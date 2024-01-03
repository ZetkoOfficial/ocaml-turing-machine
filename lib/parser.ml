(* Knjižnica funkcij za parsiranje datotek pred compilanjem. *)

exception InvalidBrackets
exception InvalidDirection
exception InvalidTransition
exception ParseException

type smer = Levo | Desno | Ni
type tranzicija_in = string * (string list)
type tranzicija_out = string * ((string * smer) list)

type token = 
  | Zacetek of string * int
  | Podatki of string list
  | Tranzicija of tranzicija_in * tranzicija_out

let trim vrstice = List.map(fun vrstica -> String.trim vrstica) vrstice

let odstrani_komentarje vrstice = 
  List.filter (fun vrstica -> 
    not @@ String.starts_with ~prefix:"#" vrstica && vrstica <> ""
  ) vrstice
;;

(** 
    Trim-ano vrstico spremeni v seznam str tokenov ločenih po presledkih.
    obnašanje seznamov tu ni validrano znotraj tokenov.
    
    @raise InvalidBrackets
*)
let v_strtokene vrstica = 
  let split = Str.split (Str.regexp " +") vrstica in
  let rec aux parts in_array acc = 
    match parts, in_array with
    | [],true -> raise InvalidBrackets
    | [],false -> List.rev acc
    | h::t,true ->
        let delta = if String.starts_with ~prefix:"[" h then 1 else 0 in
        let delta = if String.ends_with ~suffix:"]" h then delta-1 else delta in

        if delta > 0 then raise InvalidBrackets
        else 
          let status = if delta = 0 then true else false in
          aux t status ( ((List.hd acc)^h) :: (List.tl acc) )
    | h::t,false ->
        let delta = if String.starts_with ~prefix:"[" h then 1 else 0 in
        let delta = if String.ends_with ~suffix:"]" h then delta-1 else delta in

        if delta < 0 then raise InvalidBrackets
        else 
          let status = if delta = 0 then false else true in
          aux t status (h::acc) in

  aux split false []
;;

(** Pretvori token seznama v seznam *)
let v_seznam seznam_strtoken = 
  let seznam_token = Str.global_replace (Str.regexp "\\[\\|\\]") "" seznam_strtoken in
  Str.split (Str.regexp ", *") seznam_token
;;

(** Parsira strtoken seznama navodil  *)
let v_navodilo seznam = 
  List.map (fun s -> 
    match Str.split (Str.regexp " *- *") s with
    | [podatek;smer] -> begin
        match smer with
        | "levo"  -> podatek, Levo  | "l"  -> podatek, Levo
        | "desno" -> podatek, Desno | "d"  -> podatek, Desno
        | "ni"    -> podatek, Ni    | "n"  -> podatek, Ni     
        | _       -> raise InvalidDirection
      end
    | _ -> raise InvalidTransition
  ) seznam
;;

(** 
    Parsira seznam vrstic v seznam tokenov. 
    @raise ParseException
*)
let parsiraj vrstice = 
  let vrstice = vrstice |> trim |> odstrani_komentarje |> List.map (v_strtokene) in

  let rec aux vrstice t_in acc = 
    match vrstice with
    | [] -> List.rev acc
    | strtokeni::t -> begin 
      match strtokeni,t_in with
      | ["@"; zacetek; st_tirov],_ ->
        aux t None ( Zacetek (zacetek, int_of_string st_tirov) :: acc )

      | ["$"; podatki],_ ->
        aux t None ( Podatki (v_seznam podatki) :: acc )

      | [stanje; podatki; "->"; stanje'; navodila], _ ->
        let t_in = stanje, v_seznam podatki in 
        let t_out = stanje', navodila |> v_seznam |> v_navodilo in
        aux t None ( Tranzicija (t_in,t_out) :: acc )

      | [stanje; podatki; "->"], _ ->
        aux t ( Some (stanje, v_seznam podatki) ) acc

      | ["|"; stanje'; navodila], Some t_in ->
        let t_out = stanje', navodila |> v_seznam |> v_navodilo in
        aux t (Some t_in) ( Tranzicija (t_in,t_out) :: acc )

      | _ -> raise ParseException
    end in
  
  aux vrstice None []
;;
