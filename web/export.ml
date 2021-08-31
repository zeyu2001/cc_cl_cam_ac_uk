open Js_of_ocaml
open Slang

let wrap interp str =
  try interp str
  with 
    | Errors.Error s -> s

let frontend str = Front_end.front_end_from_string (Js.to_string str)
let _ =
  Js.export "slang"
    (object%js
       method interp0      str = Js.string (wrap (fun x -> (Interp_0.string_of_value (Interp_0.interpret_top_level (frontend x)))) str)
       method interp2_code str = Js.string (wrap (fun x -> (Interp_2.string_of_code  (Interp_2.compile (frontend x)))) str)
       method interp3_code str = Js.string (wrap (fun x -> (Interp_3.reset(); (Interp_3.string_of_code  (Interp_3.compile (frontend x))))) str)
       method jargon_code  str = Js.string (wrap (fun x -> (Jargon.reset(); Jargon.string_of_listing (Jargon.compile (frontend x)))) str)
     end)
