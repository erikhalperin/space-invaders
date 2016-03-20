module Projectile where

import Graphics.Collage as C
import Color
import Time exposing (Time)
import Gun exposing (..)
import Random

type alias Bullet = {x:Float, y:Float, dy:Float, r:Float}
type alias PAlien = {x:Float, y:Float, r:Float, origin:Float, bound:Float, dx:Float, count:Int, downCount:Int}

playerBulletLimit = 500

playerBulletList : List Bullet
playerBulletList = []

updatePlayerBulletList : Time -> Gun -> Keys -> List Bullet -> Int -> (List Bullet, Int, Gun)
updatePlayerBulletList tim gun keys bs ammo =
  let newBs = 
    List.filter validPlayerBullet (List.map (\b -> Bullet b.x (b.y + b.dy) b.dy b.r) bs)
  in
  -- only shoot a bullet if counter is at 0, otherwise decrement counter
  if gun.count /= 0 then (newBs, ammo, (Gun gun.x (gun.count - 1) gun.lives gun.r gun.size))
  else
    -- only shoot if player has ammo
    case ammo of
      0 -> (newBs, ammo, gun)
      _ ->
        if keys.y > 0
          then ((Bullet (toFloat gun.x) (gunY + 10) 6 3) :: newBs, ammo-1, (Gun gun.x 15 gun.lives gun.r gun.size)) 
        else (newBs, ammo, gun)

validPlayerBullet : Bullet -> Bool
validPlayerBullet b = 
  if b.y >= playerBulletLimit then False
  else True

alienBulletLimit = gunY - 5

alienBulletList : List Bullet
alienBulletList = []

gen = Random.float 0 1

-- assume this is being called 30 times per second
updateAlienBulletList : Time -> List PAlien -> Float -> List Bullet -> List Bullet
updateAlienBulletList tim als rate bs =
  let newBs = 
    List.filter validAlienBullet (List.map (\b -> Bullet b.x (b.y + b.dy) b.dy b.r) bs)
  in
  let len = List.length als in
  let seed = Random.initialSeed (round tim) in
  let val = (fst (Random.generate gen seed)) in
  if val < rate then
    let alienGenerator = (Random.int 1 len) in
    let alienNum = (fst (Random.generate alienGenerator seed)) in
    let alien = List.head (List.drop (alienNum - 1) als) in
    case alien of
      Nothing -> newBs
      Just a  -> (Bullet a.x a.y -6 3) :: newBs
  else newBs

validAlienBullet : Bullet -> Bool
validAlienBullet b = 
  if b.y <= alienBulletLimit then False
  else True

bulletsToForms : List Bullet -> List C.Form
bulletsToForms bs =
  List.map bulletToForm bs

bulletToForm : Bullet -> C.Form
bulletToForm b =
  C.move (b.x, b.y) (C.filled Color.red (C.circle b.r))
  -- (C.toForm (E.fittedImage 30 30 "bullet.png"))

-- Collision detection