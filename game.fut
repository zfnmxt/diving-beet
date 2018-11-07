import "lib/github.com/athas/matte/colour"

import "step"
import "world"

let shiftHoods [w][h] (offset: i32) (hoods: [w][h]hood): [w][h]hood =
  let new_offset = if offset == 0 then -1 else 0
  in map (\x -> map (\y ->
                     let ul = worldIndex offset hoods (x*2+new_offset+0, y*2+new_offset+0)
                     let dl = worldIndex offset hoods (x*2+new_offset+0, y*2+new_offset+1)
                     let ur = worldIndex offset hoods (x*2+new_offset+1, y*2+new_offset+0)
                     let dr = worldIndex offset hoods (x*2+new_offset+1, y*2+new_offset+1)
                     in hoodFromQuadrants ul ur dl dr)
          (iota h)) (iota w)

  let divRoundingUp (x: i32) (y: i32): i32 =
    (x + y - 1) / y

type game_state [w][h] = {generation: i32,   -- generation
                          hoods: [w][h]hood, -- world data
                          width: i32,        -- world width
                          height: i32        -- world height
                          }

-- | The un-parameterised game state type that we expose to the
-- outside world.
type ext_game_state = game_state [] []

let new_game_with (ww:i32,wh:i32) (e: element): ext_game_state =
  let w = divRoundingUp ww 2
  let h = divRoundingUp wh 2
  in {generation = 0,
      hoods = replicate w (replicate h (hoodFromQuadrants e e e e)),
      width = ww, height = wh}

entry new_game (ww: i32) (wh: i32): ext_game_state = new_game_with (ww,wh) (new #nothing)
entry new_game_random (ww: i32) (wh: i32): ext_game_state = new_game_with (ww,wh) (new #turnip)

entry step ({generation=gen,hoods,width=ww,height=wh}: ext_game_state): ext_game_state =
  let hoods' = one_step (gen+1) (shiftHoods (gen%2) hoods)
  in {generation=gen+1, hoods=hoods', width=ww, height=wh}

open argb

let screen_point_to_world_point ((ul_x, ul_y): (f32,f32)) (s: f32)
                                ((sw,sh): (i32,i32)) ((ww,wh): (i32,i32))
                                ((x,y): (i32,i32)) =
  let x' = t32 ((ul_x + s * ((r32 x + 0.5f32) / r32 sw)) * r32 ww)
  let y' = t32 ((ul_y + s * ((r32 y + 0.5f32) / r32 sh)) * r32 wh)
  in (x', y')

let elemColour (x: element): i32 =
  match x.1
  case #steam_water     -> bright (light (light (light blue)))
  case #steam_condensed -> bright (light (light (light blue)))
  case #oil             -> brown
  case #water           -> bright (bright (light blue))
  case #salt_water      -> bright (bright (light (light blue)))
  case #sand            -> dim yellow
  case #salt            -> gray 0.95f32
  case #stone           -> gray 0.7f32
  case #torch           -> bright orange
  case #plant           -> dim green
  case #spout           -> blue
  case #metal           -> mix 0.2f32 blue 0.8f32 (gray 0.5f32)
  case #lava            -> bright red
  case #napalm          -> dark orange
  case #turnip          -> violet
  case #wall            -> gray 0.4f32
  case #fire            -> mix (f32.u8 x.2) red (f32.u8 (fire_end - x.2)) yellow
  case #nothing         -> black


let dist_sq(x0:f32,y0:f32) (x1:f32,y1:f32): f32 =
  (x0-x1)*(x0-x1) + (y0-y1)*(y0-y1)

let f32p (x:i32,y:i32): (f32,f32) =
  (r32 x, r32 y)

entry render ({generation=gen,hoods,width=ww,height=wh}: ext_game_state)
             (ul_x: f32) (ul_y: f32) (s: f32) (sw: i32) (sh: i32)
             (b1: i32) (b2: i32) (r: i32) =
  let mouse_pos = screen_point_to_world_point (ul_x,ul_y) s (sw,sh) (ww,wh) (b1,b2)
  let offset = gen % 2
  let particle_pixel (x: i32) (y: i32) =
    elemColour (worldIndex offset hoods (x,y))
  let world_pixels = map (\x -> map (particle_pixel x) (iota wh)) (iota ww)
  let screen_pixel (x: i32) (y: i32) =
    (let (x',y') = screen_point_to_world_point (ul_x,ul_y) s (sw,sh) (ww,wh) (x,y)
     let dist_to_mouse = dist_sq (f32p (x',y')) (f32p mouse_pos)
     let on_select_border = t32 (f32.round (f32.sqrt dist_to_mouse)) == r
     in if x' >= 0 && x' < ww && y' >= 0 && y' < wh && !on_select_border
        then unsafe world_pixels[x', y']
        else 0xFFFFFFFF)
  in map (\y -> map (`screen_pixel` y) (iota sw)) (iota sh)

let line_dist_sq (p: (f32,f32)) (v: (f32,f32)) (w: (f32,f32)): f32 =
  let l2 = dist_sq v w
  in if l2 == 0f32 then dist_sq p v
     else let t = ((p.1 - v.1) * (w.1 - v.1) + (p.2 - v.2) * (w.2 - v.2)) / l2
          let t = if t > 1f32 then 1f32
                  else if t < 0f32 then 0f32
                  else t
          in dist_sq p
  ((v.1) + t * (w.1 - v.1),
   (v.2) + t * (w.2 - v.2))


let line_dist (p: (i32,i32)) (v: (i32,i32)) (w: (i32,i32)): f32 =
  f32.sqrt (line_dist_sq (f32p p) (f32p v) (f32p w))

entry add_element [h][w]
                  ({generation=gen,hoods: [w][h]hood,width=ww,height=wh}: ext_game_state)
                  (ul_x: f32) (ul_y: f32) (s: f32) (sw: i32) (sh: i32)
                  (b1: i32) (b2: i32) (c1: i32) (c2: i32) (r: i32) (elem: element_type): ext_game_state =
  let from = screen_point_to_world_point (ul_x,ul_y) s (sw,sh) (ww,wh) (b1,b2)
  let to   = screen_point_to_world_point (ul_x,ul_y) s (sw,sh) (ww,wh) (c1,c2)
  let offset = gen % 2
  let hoods' =
    map (\x -> map (\y ->
                    let (ul, ur, dl, dr) = hoodQuadrants hoods[x,y]
                    let ul_p = ((x*2)+offset+0, (y*2)+offset+0)
                    let ur_p = ((x*2)+offset+1, (y*2)+offset+0)
                    let dl_p = ((x*2)+offset+0, (y*2)+offset+1)
                    let dr_p = ((x*2)+offset+1, (y*2)+offset+1)
                    in hoodFromQuadrants
                       (if line_dist ul_p from to < r32 r && ul.1 == #nothing then new elem else ul)
                       (if line_dist ur_p from to < r32 r && ur.1 == #nothing then new elem else ur)
                       (if line_dist dl_p from to < r32 r && dl.1 == #nothing then new elem else dl)
                       (if line_dist dr_p from to < r32 r && dr.1 == #nothing then new elem else dr))
         (iota h)) (iota w)
  in {generation=gen, hoods=hoods', width=ww, height=wh}

entry clear_element [h][w]
                    ({generation=gen,hoods:[w][h]hood,width=ww,height=wh}: ext_game_state)
                    (ul_x: f32) (ul_y: f32) (s: f32) (sw: i32) (sh: i32)
                    (b1: i32) (b2: i32) (c1: i32) (c2: i32) (r: i32): ext_game_state =
  let from = screen_point_to_world_point (ul_x,ul_y) s (sw,sh) (ww,wh) (b1,b2)
  let to   = screen_point_to_world_point (ul_x,ul_y) s (sw,sh) (ww,wh) (c1,c2)
  let offset = gen % 2
  let hoods' =
    map (\x -> map (\y ->
                    let (ul, ur, dl, dr) = unsafe hoodQuadrants hoods[x,y]
                    let ul_p = ((x*2)+offset+0, (y*2)+offset+0)
                    let ur_p = ((x*2)+offset+1, (y*2)+offset+0)
                    let dl_p = ((x*2)+offset+0, (y*2)+offset+1)
                    let dr_p = ((x*2)+offset+1, (y*2)+offset+1)
                    in hoodFromQuadrants
                       (if line_dist ul_p from to < r32 r then new #nothing else ul)
                       (if line_dist ur_p from to < r32 r then new #nothing else ur)
                       (if line_dist dl_p from to < r32 r then new #nothing else dl)
                       (if line_dist dr_p from to < r32 r then new #nothing else dr))
         (iota h)) (iota w)
  in {generation=gen, hoods=hoods', width=ww, height=wh}


entry insertable_elements : []element_type = [ #oil
                                             , #water
                                             , #salt_water
                                             , #sand
                                             , #salt
                                             , #stone
                                             , #fire
                                             , #torch
                                             , #plant
                                             , #spout
                                             , #metal
                                             , #lava
                                             , #napalm
                                             , #turnip
                                             , #wall
                                             ]

entry element_name(x: element_type): []i32 =
  match x
  case #nothing         -> "nothing"
  case #steam_water     -> "steam"
  case #steam_condensed -> "condensate"
  case #oil             -> "oil"
  case #water           -> "water"
  case #salt_water      -> "salt water"
  case #sand            -> "sand"
  case #salt            -> "salt"
  case #stone           -> "stone"
  case #fire            -> "fire"
  case #torch           -> "torch"
  case #plant           -> "plant"
  case #spout           -> "spout"
  case #metal           -> "metal"
  case #lava            -> "lava"
  case #napalm          -> "napalm"
  case #turnip          -> "random"
  case #wall            -> "wall"

entry element_at ({generation=gen,hoods,width=ww,height=wh}: ext_game_state)
                 (ul_x: f32) (ul_y: f32) (s: f32) (sw: i32) (sh: i32) (b1: i32) (b2: i32) =
  let (x,y) = screen_point_to_world_point (ul_x,ul_y) s (sw,sh) (ww,wh) (b1,b2)
  let offset = gen % 2
  in worldIndex offset hoods (x,y)
