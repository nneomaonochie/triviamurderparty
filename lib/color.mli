module Color : sig
    val black = Graphics.rgb 000 000 000
    val white = Graphics.rgb 255 255 255
    val green = Graphics.rgb 000 255 000
    val red = Graphics.rgb 255 000 000
    val blue = Graphics.rgb 000 000 255
  val random : unit -> int
end
