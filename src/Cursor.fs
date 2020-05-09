namespace NodeSim

open Fable.Core.JS

module Cursor =

  open Browser.Types
  open Fable.Core.JsInterop

  let roundDown f =
    f
    |> int
    |> float

  let draw (ctx: CanvasRenderingContext2D) (x, y) =
    let x' =
      (x / Config.CellSize)
      |> roundDown
      |> (*) Config.CellSize

    let y' =
      (y / Config.CellSize)
      |> roundDown
      |> (*) Config.CellSize

    // ctx.beginPath()
    // ctx.arc (x, y, 4., 0., Math.PI * 2.)
    // ctx.fill()
    ctx.fillStyle <- !^"#f037"
    ctx.fillRect (x', y', Config.CellSize, Config.CellSize)
