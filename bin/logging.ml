module ANSI = struct 
  let red     = "\027[31m"
  let yellow  = "\027[33m"
  let green   = "\027[32m"
  let reset   = "\027[0m"

  let format prefix str = prefix^str^reset;;

  let delete_line () = 
    print_string "\027[MhelloWorld";
    print_string "\027[2k"; flush stdout;
    print_string "\r"; flush stdout
  ;;
end

let print_uspeh del = 
  let ok_sym = ANSI.format ANSI.green " (✓)" in
  print_endline ("| "^ del ^ ok_sym)
;;

let print_wait del = 
  let wait_sym = ANSI.format ANSI.yellow " (*)" in
  print_string ("| "^ del ^ wait_sym); flush stdout
;;

let print_error ?(line_num=None) del extra =
  let error_sym = ANSI.format ANSI.red " (X)" in
  print_endline ("| " ^ del ^ error_sym);

  begin
    match line_num with
    | None ->   print_endline (ANSI.format ANSI.red "    > napaka:")
    | Some i -> print_endline
      (ANSI.format ANSI.red ("    > napaka: (vrstica "^string_of_int (i+1)^")"))
  end;

  List.iter (fun line -> print_endline (ANSI.format ANSI.red ("    > "^line)) ) extra
;;

let print_q del quesiton =
  let question_sym = " (?)" in
  print_endline ("| " ^ del ^ question_sym);
  print_endline ("    > "^quesiton);
  print_string  ("    > "); flush stdout
;;