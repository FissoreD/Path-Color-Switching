let sep () = print_endline "------------------------"

let _ =
  Sys.chdir "../../../";
  let stdout' = open_out "./res.txt" in
  print_endline "1";
  FW.classic ();
  sep ();
  FW.generalized ();
  sep ();
  Col_switch.main ();
  sep ();
  Col_switch.main' ~stdout:stdout' ();
  sep ();
  Col_switch.main_mdd ();
  Printf.printf "start main_mdd'\n";
  (* Col_switch.main_mdd' (); *)
  close_out stdout'
