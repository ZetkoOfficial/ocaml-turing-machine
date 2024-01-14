(* Knjižnica funkcij za parsiranje datotek pred compilanjem. *)

(* Napake vsebujejo vrstico v kateri se pojavijo *)
exception InvalidBrackets   of int
exception InvalidDirection  of int
exception InvalidTransition of int
exception ParseException    of int
exception InvalidPosNumber  of int

let validiraj_pos_num i num_str = 
  let num = try int_of_string num_str with _ -> raise (InvalidPosNumber i) in
  if num <= 0 then raise (InvalidPosNumber i) else num
;;

type smer = Levo | Desno | Ni
type tranzicija_in = string * (string list)
type tranzicija_out = string * ((string * smer) list)

type token = 
  | Zacetek of string * int
  | Podatki of string list
  | Tranzicija of int * (tranzicija_in * tranzicija_out)

let trim vrstice = List.mapi(fun i vrstica -> i,String.trim vrstica) vrstice

let odstrani_komentarje vrstice = 
  List.filter (fun (_,vrstica) -> 
    not @@ String.starts_with ~prefix:"#" vrstica && vrstica <> ""
  ) vrstice
;;

(** 
    Trim-ano in številčeno vrstico spremeni v številčen seznam str tokenov ločenih po presledkih.
    obnašanje seznamov tu ni validrano znotraj tokenov.
    
    @raise InvalidBrackets
*)
let v_strtokene (i,vrstica) = 
  let split = Str.split (Str.regexp " +") vrstica in
  let rec aux parts in_array acc = 
    match parts, in_array with
    | [],true -> raise (InvalidBrackets i)
    | [],false -> List.rev acc
    | h::t,true ->
        let delta = if String.starts_with ~prefix:"[" h then 1 else 0 in
        let delta = if String.ends_with ~suffix:"]" h then delta-1 else delta in

        if delta > 0 then raise (InvalidBrackets i)
        else 
          let status = if delta = 0 then true else false in
          aux t status ( ((List.hd acc)^h) :: (List.tl acc) )
    | h::t,false ->
        let delta = if String.starts_with ~prefix:"[" h then 1 else 0 in
        let delta = if String.ends_with ~suffix:"]" h then delta-1 else delta in

        if delta < 0 then raise (InvalidBrackets i)
        else 
          let status = if delta = 0 then false else true in
          aux t status (h::acc) in

  i, aux split false []
;;

(** Pretvori token seznama v seznam *)
let v_seznam seznam_strtoken = 
  let seznam_token = Str.global_replace (Str.regexp "\\[\\|\\]") "" seznam_strtoken in
  Str.split (Str.regexp ", *") seznam_token
;;

(** Parsira strtoken seznama navodil  *)
let v_navodilo i seznam = 
  List.map (fun s -> 
    match Str.split (Str.regexp " *- *") s with
    | [podatek;smer] -> begin
        match smer with
        | "levo"  -> podatek, Levo  | "l"  -> podatek, Levo
        | "desno" -> podatek, Desno | "d"  -> podatek, Desno
        | "ni"    -> podatek, Ni    | "n"  -> podatek, Ni     
        | _       -> raise (InvalidDirection i)
      end
    | _ -> raise (InvalidTransition i)
  ) seznam
;;

(** 
    Parsira seznam vrstic v seznam tokenov. 
    @raise ParseException v primeru da ni nobena druga napaka
*)
let parsiraj vrstice = 
  let vrstice = vrstice |> trim |> odstrani_komentarje |> List.map (v_strtokene) in

  let rec aux vrstice t_in acc = 
    match vrstice with
    | [] -> List.rev acc
    | (i,strtokeni)::t -> begin 
      match strtokeni,t_in with
      | ["@"; zacetek; st_tirov],_ ->
        aux t None ( Zacetek (zacetek, validiraj_pos_num i st_tirov) :: acc )

      | ["$"; podatki],_ ->
        aux t None ( Podatki (v_seznam podatki) :: acc )

      | [stanje; podatki; "->"; stanje'; navodila], _ ->
        let t_in = stanje, v_seznam podatki in 
        let t_out = stanje', navodila |> v_seznam |> v_navodilo i in
        aux t None ( Tranzicija (i,(t_in,t_out)) :: acc )

      | [stanje; podatki; "->"], _ ->
        aux t ( Some (stanje, v_seznam podatki) ) acc

      | ["|"; stanje'; navodila], Some t_in ->
        let t_out = stanje', navodila |> v_seznam |> v_navodilo i in
        aux t (Some t_in) ( Tranzicija (i, (t_in,t_out)) :: acc )

      | _ -> raise (ParseException i)
    end in
  
  aux vrstice None []
;;
