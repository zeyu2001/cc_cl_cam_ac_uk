open Js_of_ocaml
open Slang


let frontend str = Front_end.front_end_from_string (Js.to_string str)
let _ =
  Js.export "slang"
    (object%js
       method interp0      str = Js.string (Interp_0.string_of_value (Interp_0.interpret_top_level (frontend str)))
       method interp2_code str = Js.string (Interp_2.string_of_code  (Interp_2.compile (frontend str)))
       method interp3_code str = Js.string (Interp_3.string_of_code  (Interp_3.compile (frontend str)))
       method jargon_code  str = Js.string (Jargon.string_of_listing (Jargon.compile (frontend str)))
     end)
