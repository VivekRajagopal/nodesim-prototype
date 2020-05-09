namespace NodeSim

module Grid =

  open Browser.Types
  open Fable.Core.JsInterop

  let drawLine (ctx: CanvasRenderingContext2D) fromPoint toPoint =
    ctx.beginPath()
    ctx.moveTo fromPoint
    ctx.lineTo toPoint
    ctx.stroke()

  let draw (ctx: CanvasRenderingContext2D) =
    ctx.strokeStyle <- !^"#666"

    [ 0 .. (Config.CellSize |> int) .. (Config.Width |> int) ]
    |> Seq.iter (fun i ->
         let x = float i
         drawLine ctx (x, 0.) (x, Config.Height))

    [ 0 .. (Config.CellSize |> int) .. (Config.Height |> int) ]
    |> Seq.iter (fun i ->
         let y = float i
         drawLine ctx (0., y) (Config.Width, y))
