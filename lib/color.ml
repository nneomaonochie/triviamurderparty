open! Core

let black = Graphics.rgb 000 000 000
let white = Graphics.rgb 255 255 255
let green = Graphics.rgb 000 255 000
let red = Graphics.rgb 255 000 000
let blue = Graphics.rgb 000 000 255
let dark_blue = Graphics.rgb 000 000 204
let dark_red = Graphics.rgb 168 41 41
let skeleton_white = Graphics.rgb 245 245 245
let skeleton_gray = Graphics.rgb 119 119 119
let dead_gray = Graphics.rgb 146 146 146
let pink = Graphics.rgb 252 225 255

let random () =
  Graphics.rgb (Random.int 256) (Random.int 256) (Random.int 256)
;;
