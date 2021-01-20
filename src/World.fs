namespace NodeSim

module World =

    open Browser.Types
    open Fable.Core.JsInterop
    open FSharp.Core
    open NodeSim.Environment

    let worldSizeX, worldSizeY =
        Config.Width / Config.CellSize |> int, Config.Height / Config.CellSize |> int

    type Cell =
        | Empty
        | Prey
        | Predator

    type Board = Cell [] []

    // let Board = Array2D.create worldSizeX worldSizeY false

    let private emptyBoard: Board =
        [ 0 .. worldSizeX - 1 ]
        |> List.map (fun _i -> Array.create worldSizeY Empty)
        |> List.toArray

    let createBoard () =
        let board = emptyBoard
        let rng = System.Random()

        for ix in 0 .. (Array.length board) - 1 do
            for iy in 0 .. (Array.length board.[ix]) - 1 do
                board.[ix].[iy] <-
                    if rng.NextDouble() > 0.65 then
                        Prey
                    else
                        Predator

        board

    let setCellValue (board: Board) (coordX, coordY) value = board.[coordX].[coordY] <- value

    let getKernel (board: Board) (coordX, coordY) =
        let left = max 0 (coordX - 1)
        let right = min (board.Length - 1) (coordX + 1)

        let top = max 0 (coordY - 1)

        let bottom =
            min (board.[coordX].Length - 1) (coordY + 1)

        board.[left..right]
        |> Array.collect (fun col -> col.[top..bottom])

    let (|Survive|Spawn|Die|StayDead|) (liveNeighbourCount, isCellAlive) =
        match isCellAlive with
        | true ->
            if liveNeighbourCount = 2 || liveNeighbourCount = 3 then
                Survive
            else
                Die
        | false ->
            if liveNeighbourCount = 3 then
                Spawn
            else
                StayDead

    type Count =
        { Empty: int
          Prey: int
          Predator: int }

    let computeCellValue (cellValue: Cell) (kernel: Cell []) =
        let counts =
            kernel
            |> Array.fold
                (fun state item ->
                    match item with
                    | Empty -> { state with Empty = state.Empty + 1 }
                    | Prey -> { state with Prey = state.Prey + 1 }
                    | Predator ->
                        { state with
                              Predator = state.Predator + 1 })
                { Empty = 0; Prey = 0; Predator = 0 }

        match cellValue with
        | Empty ->
            if counts.Prey = 3 then
                Prey
            else if counts.Predator = 3 then
                Predator
            else
                Empty
        | Prey ->
            if counts.Predator >= 4 || counts.Prey >= 3 then
                Empty
            else
                Prey
        | Predator ->
            if counts.Predator >= 3 then
                Empty
            else
                Predator

    let update (board: Board): Board =
        let board' = Array.copy board

        for ix in 0 .. (Array.length board) - 1 do
            for iy in 0 .. (Array.length board.[ix]) - 1 do
                let cellValue =
                    getKernel board (ix, iy)
                    |> computeCellValue board.[ix].[iy]

                cellValue |> setCellValue board (ix, iy)

        board'

    let coordinateToScreenSpace coordinate =
        (coordinate.x |> float |> (*) Config.CellSize, coordinate.y |> float |> (*) Config.CellSize)

    let draw (ctx: CanvasRenderingContext2D) (board: Board) =
        for ix in 0 .. (Array.length board) - 1 do
            for iy in 0 .. (Array.length board.[ix]) - 1 do
                let (px, py) =
                    coordinateToScreenSpace { x = ix; y = iy }

                let fillStyle =
                    match board.[ix].[iy] with
                    | Prey -> "cyan"
                    | Predator -> "orangered"
                    | _ -> "transparent"

                ctx.fillStyle <- !^fillStyle

                ctx.fillRect (px, py, px + Config.CellSize, py + Config.CellSize)
