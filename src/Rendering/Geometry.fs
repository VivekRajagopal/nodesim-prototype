namespace NodeSim.Rendering

module Geometry =
    let drawRectangle stroke fill =
        Util.setStroke stroke >> Util.setFill fill
