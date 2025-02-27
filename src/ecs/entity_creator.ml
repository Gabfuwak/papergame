open Types
open World
open Entity
open Position
open Movable
open Controllable
open Drawable
open Vector

let create_player world x y =
  let id = Entity.create () in
  let position = { pos = Vector.create x y } in
  let movable = { velocity = Vector.create 0.0 0.0; force = Vector.create 0.0 0.0 } in
  let controllable = { speed = 5.0 } in
  
  Hashtbl.add world.state.position_store id position;
  Hashtbl.add world.state.movable_store id movable;
  Hashtbl.add world.state.controllable_store id controllable;
  

  let black = Color (Gfx.color 0 0 0 255) in
  
  let texture = black in
  let drawable = { screen_pos = Vector.create 0.0 0.0; height = 100; width = 100; tex = texture } in

  Hashtbl.add world.state.drawable_store id drawable;
  
  id
