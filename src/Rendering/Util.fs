namespace NodeSim.Rendering

open Browser.Types
open Fable.Core.JsInterop

module Util =
    let setStroke (color: string) (ctx: CanvasRenderingContext2D) =
        ctx.strokeStyle <- !^color
        ctx

    let setFill (color: string) (ctx: CanvasRenderingContext2D) =
        ctx.fillStyle <- !^color
        ctx

    let coordinateToScreenSpace coordinate =
        (coordinate.x |> float |> (*) Config.CellSize, coordinate.y |> float |> (*) Config.CellSize)
