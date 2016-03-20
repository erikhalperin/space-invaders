module State where

import Window
import Color
import Text as T
import Graphics.Element as E
import Graphics.Collage as C exposing (defaultLine)
import Graphics.Input exposing (button, customButton)
import Time exposing (Time)
import Signal exposing (Mailbox, mailbox)
import Debug
import Keyboard exposing (arrows)
import Gun exposing (..)
import Projectile exposing (..)

{- State includes:

   1. time elapsed
   2. RunState 
   3. score
   4. points representing invaders
   5. point representing player
   6. Level
   7. Projectile-related objects (this was used because a tuple can only contain 10 items)
      a. List of bullets fired by player
      b. Alien rate of fire
      c. List of alien bullets
      d. ammo left
-}

type alias State = (Int, RunState, Int, List Alien, Gun, Level, ProjStuff)
type alias ProjStuff = {bs: List Bullet, rate: Float, abs: List Bullet, ammo: Int}
type RunState    = Paused | Playing Time
type Msg         = Start | Stop | Reset | NextLevel
type TickOr msg  = Tick | M msg
type alias Event = {time:Time, tick:TickOr Msg, keys:Keys}
type alias Alien = {x:Float, y:Float, r:Float, origin:Float, bound: Float, dx:Float, count:Int, downCount:Int}

type Level = One | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten

delay = 0.5 * Time.second
alienRadius = 14
startAmmo = 500

upstate : Event -> State -> State
upstate e (n,p,s,invaders,gun,level,proj) = 
  -- update gun position
  let newG =
    case p of
      Playing _ -> updateGun e.keys gun
      _         -> gun
  in
  -- update player bullet(s) position
  let (newBs, ammo', newG') =
    case p of
      Playing _ -> updatePlayerBulletList e.time newG e.keys proj.bs proj.ammo
      _         -> (proj.bs, proj.ammo, newG)
  in
  -- update bullets and invaders if collisions
  let (newBs', invaders', s') =
    case p of
      Playing _ -> detectAlienCollisions newBs invaders s
      _         -> (newBs, invaders, s)
  in
  -- update alien bullets
  let newAbs =
    case p of
      Playing _ -> updateAlienBulletList e.time invaders' proj.rate proj.abs
      _         -> proj.abs
  in
  -- update if alien bullets hit the gun
  let newGun =
    case p of
      Playing _ -> detectGunCollision newAbs newG'
      _         -> newG'
  in
  -- update invaders
  let invaders'' = updateInvaders invaders' in
  -- update ProjStuff Record
  let proj' = (ProjStuff newBs' proj.rate newAbs ammo') in
  case (e.tick,p) of
    (M Start, Paused)      -> (n, Playing (e.time + delay), s', invaders'', newGun,level,proj')
    (M Stop,  Playing _)   -> (n, Paused, s', invaders, newGun,level,proj')
    (Tick,    Paused)      -> (n, Paused, s', invaders, newGun,level,proj')
    (M NextLevel, _)       -> let nextL = nextLevel level in
                              let (newAmmo, newRate, newGun') = levelUpState nextL in
                                (n, Playing (e.time + delay), s', updateInvaders (initAliens 1 1 (startX-colSpace)), newGun',nextL,ProjStuff newBs' newRate newAbs newAmmo)
    (M Reset, _)           -> (0, Playing (e.time + delay), 0, updateInvaders (initAliens 1 1 (startX-colSpace)),Gun 0 0 3 10 30,One,ProjStuff newBs' 0.02 newAbs 300)
    (Tick,    Playing end) -> if e.time < end then (n, p, s', invaders'', newGun,level,proj')
                            else (n+1, Playing (end + delay), s', updateInvaders invaders', newGun,level,proj')
    _                      -> Debug.crash "upstate: impossible"

theMailbox : Mailbox Msg
theMailbox = mailbox Start

colSpace = 50
rowSpace = 30
startX = -250
startY = 400

nextLevel : Level -> Level
nextLevel l =
  case l of
    One -> Two
    Two -> Three
    Three -> Four
    Four -> Five
    Five -> Six
    Six -> Seven
    Seven -> Eight
    Eight -> Nine
    Nine -> Ten
    Ten -> Debug.crash "Impossible"

levelUpState : Level -> (Int, Float, Gun)
levelUpState l =
  case l of
    One -> (300, 0.02, Gun 0 0 3 10 30)
    Two -> (250, 0.04, Gun 0 0 3 11 32)
    Three -> (200, 0.06, Gun 0 0 3 12 34)
    Four -> (150, 0.08, Gun 0 0 3 13 36)
    Five -> (125, 0.1, Gun 0 0 3 14 38)
    Six -> (100, 0.12, Gun 0 0 3 15 40)
    Seven -> (80, 0.14, Gun 0 0 3 16 42)
    Eight -> (70, 0.16, Gun 0 0 3 17 44)
    Nine -> (60, 0.18, Gun 0 0 3 18 46)
    Ten -> (50, 0.2, Gun 0 0 3 20 48)
 
strStyle : String -> E.Element
strStyle = T.fromString >> T.height 30 >> E.centered

lineStyle = { defaultLine | color = Color.darkCharcoal , width = 10 }

btnW = 200
btnH = 50

myButton msg s =
  let drawButton c =
     C.collage btnW btnH
       [ C.filled c (C.rect btnW btnH)
         , C.outlined lineStyle (C.rect btnW btnH)
         , strStyle s |> C.toForm
        ] 
  in
  customButton msg
    (drawButton Color.lightYellow) 
    (drawButton Color.lightOrange)
    (drawButton Color.lightBlue)

scoreStr : Int -> E.Element
scoreStr s =
  let score = s |> toString |> T.fromString in
  let str = "Score: " |> T.fromString in
    (T.append str score) |> T.color Color.white |> (T.height 30 >> E.centered) |> E.container btnW btnH E.middle

timeStr : Int -> E.Element
timeStr n =
  let time = n//2 |> toString |> T.fromString in
  let str = "Time: " |> T.fromString in
    (T.append str time) |> T.color Color.white |> (T.height 30 >> E.centered) |> E.container btnW btnH E.middle 

ammoStr : Int -> E.Element
ammoStr a =
  let ammo = a |> toString |> T.fromString in
  let str = "Ammo: " |> T.fromString in
    (T.append str ammo) |> T.color Color.red |> (T.height 30 >> E.centered) |> E.container btnW btnH E.middle

levelStr : Level -> E.Element
levelStr l =
  let level = l |> toString |> T.fromString in
  let str = "Level: " |> T.fromString in
    (T.append str level) |> T.color Color.red |> (T.height 30 >> E.centered) |> E.container btnW btnH E.middle

gameOverStr : (Int, Int) -> E.Element
gameOverStr (w, h) =
  -- Create the game over screen
  let str = "GAME OVER" |> T.fromString in
    str |> T.color Color.red |> (T.height 100 >> E.centered) |> E.container w (h-100) E.middle

levelUpStr : (Int, Int) -> E.Element
levelUpStr (w, h) =
  -- Create the game over screen
  let str = "LEVEL UP" |> T.fromString in
    str |> T.color Color.green |> (T.height 100 >> E.centered) |> E.container w (h-200) E.middle

gameWonStr : (Int, Int) -> E.Element
gameWonStr (w, h) =
  -- Create the game won screen if beat level 10
  let str = "YOU WON!!!" |> T.fromString in
    str |> T.color Color.green |> (T.height 100 >> E.centered) |> E.container w h E.middle

printLives : Gun -> E.Element
printLives gun =
  let pString = ("Lives: " ++ (toString gun.lives)) |> T.fromString in
    pString |> T.color Color.white |> (T.height 30 >> E.centered) |> E.container btnW btnH E.middle

initAliens : Int -> Int -> Float -> List Alien
initAliens row col prevX =
  if row == 5 && (col % 11 == 0) then
    [{x=prevX+colSpace, y=startY-(row*rowSpace), r=alienRadius, origin=prevX+colSpace, bound=200, dx=-10, count=30, downCount=0}]
  else if (col % 11 == 0) then
    {x=prevX+colSpace, y=startY-(row*rowSpace), r=alienRadius, origin=prevX+colSpace, bound=200, dx=-10, count=30, downCount=0} :: initAliens (row+1) 1 (startX-colSpace)
  else
    {x=prevX+colSpace, y=startY-(row*rowSpace), r=alienRadius, origin=prevX+colSpace, bound=200, dx=-10, count=30, downCount=0} :: initAliens row (col+1) (prevX+colSpace)

drawInvader : Alien -> C.Form
drawInvader pt = 
  C.move (pt.x, pt.y) (C.toForm (E.fittedImage 30 30 "invader.jpg"))

initInvaders : List Alien -> List C.Form
initInvaders pts =
  case pts of
    [] -> []
    pt :: pts' -> 
      drawInvader pt :: initInvaders pts'

updateInvaders : List Alien -> List Alien
updateInvaders als =
  case als of
    []       -> []
    a::als' -> 
      -- it is not time to move yet, so aliens stay in place and decrement counter
      if a.count /= 0 then
        List.map (\a -> (Alien a.x a.y a.r a.origin a.bound a.dx (a.count - 1) a.downCount)) als
      -- aliens are at left bound, so move down, change dx, reset counter, incrememnt downCount
      else if a.x <= a.origin - a.bound then
        List.map (\a -> (Alien (a.x - a.dx) (a.y - 10) a.r a.origin a.bound 10 30 (a.downCount + 1))) als
      -- aliens are at right bound, so move down, change dx, reset counter
      else if a.x >= a.origin + a.bound then
        List.map (\a -> (Alien (a.x - a.dx) (a.y - 10) a.r a.origin a.bound -10 30 (a.downCount + 1))) als
      -- move aliens by dx and reset counter
      else
        List.map (\a -> (Alien (a.x + a.dx) a.y a.r a.origin a.bound a.dx 30 a.downCount)) als

view : (Int, Int) -> State -> E.Element
view (w,h) (n,p,s,invaders,gun,level,proj) =
  let caption = timeStr n in
  let lev = levelStr level in
  let score = scoreStr s in
  let a = ammoStr proj.ammo in
  let invs = initInvaders invaders in
  let buls = bulletsToForms proj.bs in
  let aBuls = bulletsToForms proj.abs in
  let g = gunToForm gun in
  let objs = g :: (List.append (List.append invs buls) aBuls) in
  let (gameO, levelUp) = gameOver (n,p,s,invaders,gun,level,proj) in
  let btn = 
    let makeMessage = Signal.message theMailbox.address in
    case (n,p,s) of 
      (0, Paused, _)    -> myButton (makeMessage Start) "Fight" 
      (_, Paused, _)    -> myButton (makeMessage Start) "Resume"
      (_, Playing _, _) -> 
        if gameO then 
          if levelUp then myButton (makeMessage NextLevel)  "Next Level"
          else myButton (makeMessage Reset)  "New Game"
        else myButton (makeMessage Stop)  "Pause"
  in
    if gameO then
      if levelUp then
        if level == Ten then
          E.color Color.black <| gameWonStr (w, h)
        else
          E.color Color.black <| E.flow E.down <| [score, btn, printLives gun, levelUpStr (w, h)]
      else
        E.color Color.black <| E.flow E.down <| [btn, gameOverStr (w, h)]
    else
      E.color Color.black <| E.flow E.down <| [lev, caption, btn, score, a, printLives gun, C.collage w h objs]


mergeSignals : Time -> Signal Keys -> Signal Msg -> Signal Event
mergeSignals t keys msg =
  let time = Time.every t in
  let msgSig =
    Signal.merge (Signal.map  (\t -> (t, Tick)) time) (Signal.map2 (\t m -> (t, M m )) (Signal.sampleOn msg time) msg)
  in
  Signal.map2 (\k m -> (Event (fst m) (snd m) k)) keys msgSig

detectGunCollision : List Bullet -> Gun -> Gun
detectGunCollision bs g =
  let newBs = List.filter (gunIntersects g) bs in
  if List.length bs == List.length newBs then g
  else (Gun g.x g.count (g.lives - 1) g.r g.size)

-- used for filtering, so this checks if it doesn't intersect
gunIntersects : Gun -> Bullet -> Bool
gunIntersects g b =
  let sqr = (b.x - (toFloat g.x))^2 + (b.y - (toFloat (gunY - 10)))^2 in
  if (b.r - g.r)^2 <= sqr && sqr <= (b.r + g.r)^2 then False
  else True

detectAlienCollisions : List Bullet -> List Alien -> Int -> (List Bullet, List Alien, Int)
detectAlienCollisions bs als s =
  let res = detect_ (bs, []) als s in
  let newBs = snd (fst res) in
  let newAls = fst (snd res) in
  let newS = snd (snd res) in
  (newBs, newAls, newS)

-- originally, the second bullet list is empty. this function will process the bullets one by one,
-- and if a bullet hasn't, it will be moved to the second list. if not, the bullet will not be added
-- and the list of aliens will be filtered
detect_ : (List Bullet, List Bullet) -> List Alien -> Int -> ((List Bullet, List Bullet), (List Alien, Int))
detect_ (bs1, bs2) als s =
  case bs1 of
    []      -> ((bs1, bs2), (als, s))
    b::bs1' -> 
      let (newB, newAls, newS) = detectAlienCollision b als s in
      case newB of
        Nothing -> detect_ (bs1', bs2) newAls newS
        Just x  -> detect_ (bs1', x::bs2) newAls newS

detectAlienCollision : Bullet -> List Alien -> Int -> (Maybe Bullet, List Alien, Int)
detectAlienCollision b als s =
  let newAls = List.filter (alienIntersects b) als in
  if List.length als == List.length newAls then (Just b, newAls, s)
  else (Nothing, newAls, s+1) -- update score

-- used for filtering, so this checks if it doesn't intersect
alienIntersects : Bullet -> Alien -> Bool
alienIntersects b a =
  let sqr = (b.x - a.x)^2 + (b.y - a.y)^2 in
  if (b.r - a.r)^2 <= sqr && sqr <= (b.r + a.r)^2 then False
  else True

gameOver : State -> (Bool, Bool) -- (Game Over, Game Won)
gameOver (n,p,s,invaders,gun,level,proj) =
  case invaders of
    []     -> (True, True)
    inv::_ ->
      if proj.ammo == 0 && proj.bs == [] then 
        (True, False)
      else if gun.lives <= 0 then
        (True, False)
      else if inv.downCount >= 13 then
        (True, False)
      else
        (False, False)

main =
  let
    state : Signal State
    state =
      let invs = (initAliens 1 1 (startX-colSpace)) in 
      let proj = (ProjStuff playerBulletList 0.02 alienBulletList 300) in
        Signal.foldp upstate (0, Paused, 0, invs, startGun, One, proj)
              (mergeSignals (33 * Time.millisecond) arrows theMailbox.signal)
  in
  Signal.map2 view Window.dimensions state