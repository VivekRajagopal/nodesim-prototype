namespace NodeSim.Environment

type Coordinate =
    { x: int
      y: int }
    static member (+)(c1: Coordinate, c2: Coordinate) = { x = c1.x + c2.x; y = c1.y + c2.y }
    static member (-)(c1: Coordinate, c2: Coordinate) = { x = c1.x - c2.x; y = c1.y - c2.y }
