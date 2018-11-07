type element_type = #nothing
                  | #steam_water
                  | #steam_condensed
                  | #oil
                  | #water
                  | #salt_water
                  | #sand
                  | #salt
                  | #stone
                  | #fire
                  | #torch
                  | #plant
                  | #spout
                  | #metal
                  | #lava
                  | #turnip
                  | #wall
                  | #napalm

type element = (element_type, u8)
type weight = u8
type weight_env = u8

let new (x : element_type) : element = (x, 0)

let elems: []element = [ new #nothing
                        , new #steam_water
                        , new #steam_condensed
                        , new #oil
                        , new #water
                        , new #salt_water
                        , new #sand
                        , new #salt
                        , new #stone
                        , new #fire
                        , new #torch
                        , new #plant
                        , new #spout
                        , new #metal
                        , new #lava
                        , new #turnip
                        , new #wall
                        , new #napalm
                        ]

let num_elems: i32 = length elems

let fire_end : u8 = 10

let isInsertable' (x: element_type) : bool =
  x != #steam_water && x != #steam_condensed && x != #nothing

let isInsertable (x: element) : bool = isInsertable' x.1

let isWall' (x: element_type) : bool =
  x == #torch || x == #plant || x == #spout || x == #metal || x == #wall

let isWall (x: element) : bool = isWall' x.1

let isFluid' (x: element_type) : bool =
  x == #steam_water || x == #steam_condensed || x == #oil ||
  x == #water || x == #salt_water ||
  x == #lava || x == #napalm

let isFluid (x: element) : bool = isFluid' x.1

let weight (x: element) : weight =
  match x.1
  case #nothing         -> 2
  case #steam_water     -> 0
  case #steam_condensed -> 0
  case #oil             -> 6
  case #water           -> 7
  case #salt_water      -> 8
  case #sand            -> 10
  case #salt            -> 10
  case #stone           -> 11
  case #fire            -> 0
  case #torch           -> 23
  case #plant           -> 24
  case #spout           -> 25
  case #metal           -> 26
  case #lava            -> 7
  case #turnip          -> 28
  case #wall            -> 29
  case #napalm          -> 6

let age (r: i32) (x: element): element =
  match x.1
  case #fire            ->
    if x.2 >= fire_end then new #nothing
    else if r < 5000 then (#fire, x.2 + 1) else x
  case #steam_water     -> if r < 100 then new #water else new #steam_water
  case #steam_condensed -> if r < 500 then new #water else new #steam_condensed
  case #napalm          -> if r < 10 then new #nothing else new #napalm
  case #turnip          -> unsafe (elems[r%num_elems].1, u8.i32 r%fire_end)
  case _                -> x
