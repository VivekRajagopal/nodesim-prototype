namespace NodeSim

open Browser.Dom

module World =

  open Browser.Types
  open Fable.Core.JsInterop
  open FSharp.Core

  let worldSizeX, worldSizeY =
    Config.Width / Config.CellSize |> int, Config.Height / Config.CellSize |> int

  type Board = bool [] []

  // let Board = Array2D.create worldSizeX worldSizeY false

  let private emptyBoard: Board =
    [ 0 .. worldSizeX - 1 ]
    |> List.map (fun _i -> Array.create worldSizeY false)
    |> List.toArray

  let createBoard() =
    let board = emptyBoard
    let rng = System.Random()
    for ix in 0 .. (Array.length board) - 1 do
      for iy in 0 .. (Array.length board.[ix]) - 1 do
        board.[ix].[iy] <- rng.NextDouble() > 0.04
    board

  let setCellValue (board: Board) (coordX, coordY) value = board.[coordX].[coordY] <- value

  let getKernel (board: Board) (coordX, coordY) =
    let left = max 0 (coordX - 1)
    let right = min (board.Length - 1) (coordX + 1)

    let top = max 0 (coordY - 1)
    let bottom = min (board.[coordX].Length - 1) (coordY + 1)


    board.[left..right] |> Array.collect (fun col -> col.[top..bottom])

  let (|Survive|Spawn|Die|StayDead|) (liveNeighbourCount, isCellAlive) =
    match isCellAlive with
    | true ->
        if liveNeighbourCount = 2 || liveNeighbourCount = 3 then Survive else Die
    | false ->
        if liveNeighbourCount = 3 then Spawn else StayDead

  let computeCellValue cellValue kernel =
    let offset =
      if cellValue then 1 else 0

    let liveNeighbourCells =
      kernel
      |> Array.filter id
      |> Array.length
      |> (fun x -> x - offset)

    match (liveNeighbourCells, cellValue) with
    | Survive
    | Spawn -> true
    | _ -> false

  let update (board: Board): Board =
    let board' = Array.copy board
    for ix in 0 .. (Array.length board) - 1 do
      for iy in 0 .. (Array.length board.[ix]) - 1 do
        let cellValue = getKernel board (ix, iy) |> computeCellValue board.[ix].[iy]
        cellValue |> setCellValue board (ix, iy)

    board'

  let pointToScreenSpace (x, y) =
    (x
     |> float
     |> (*) Config.CellSize,
     y
     |> float
     |> (*) Config.CellSize)

  let draw (ctx: CanvasRenderingContext2D) (board: Board) =
    for ix in 0 .. (Array.length board) - 1 do
      for iy in 0 .. (Array.length board.[ix]) - 1 do
        let (px, py) = pointToScreenSpace (ix, iy)

        let fillStyle =
          match board.[ix].[iy] with
          | true -> "black"
          | false -> "white"
        ctx.fillStyle <- !^fillStyle

        ctx.fillRect (px, py, px + Config.CellSize, py + Config.CellSize)
