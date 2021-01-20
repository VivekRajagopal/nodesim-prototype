namespace NodeSim.Entities

open NodeSim.Rendering
open NodeSim.Environment

type Entity =
    { Coordinate: Coordinate
      IsLocked: bool }

module Entity =
    let draw entity = Util.setStroke
