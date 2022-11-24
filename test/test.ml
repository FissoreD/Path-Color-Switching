let sep () = print_endline "------------------------"

let () =
  let stdout' = open_out "./res.txt" in
  (* Sys.chdir "test"; *)
  print_endline @@ Sys.getcwd ();
  (* print_endline "1";
     FW.classic ();
     sep ();
     FW.generalized ();
     sep ();
     Col_switch.main ();
     sep ();
     Col_switch.main' ~stdout:stdout' ();*)
  sep ();
  Col_switch.main_mdd ();
  Printf.printf "start main_mdd'\n";
  Col_switch.main_mdd' ~stdout:stdout' ();
  close_out stdout'
