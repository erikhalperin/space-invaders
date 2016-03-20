module Gun where

import Keyboard exposing (arrows)
import Graphics.Element as E
import Graphics.Collage as C
import Color
import Signal
import Window
import Time exposing (fps)

-- count is used to limit gun firing rate
type alias Gun = {x:Int, count:Int, lives:Int, r:Float, size:Int}
type alias Keys = {x:Int, y:Int}

gunY = 100

startGun = (Gun 0 0 3 10 30)

updateGun : Keys -> Gun -> Gun
updateGun keys g =
  if keys.x < 0 && g.x > -445 then             
    (Gun (g.x - 4) g.count g.lives g.r g.size)
  else if keys.x > 0 && g.x < 445 then 
    (Gun (g.x + 4) g.count g.lives g.r g.size)
  else
    g

gunToForm : Gun -> C.Form
gunToForm g =
  C.move ((toFloat g.x), gunY) (C.toForm (E.fittedImage g.size g.size "gun.png"))