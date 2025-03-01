type texture =
  | Color of Gfx.color
  | Image of Gfx.surface
  | Animation of {
      frames: Gfx.surface array;
      framerate: float;
      mutable current_frame: int;
      mutable accumulated_time: float;
    }

type atlas_region = {
  x: int;
  y: int;
  width: int;
  height: int;
}

type atlas_item =
  | StaticTile of atlas_region
  | AnimationItem of {
      base_region: atlas_region;
      frames: int;
      framerate: float;
    }
