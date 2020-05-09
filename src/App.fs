namespace NodeSim

module App =

  open Fable.Core.JsInterop
  open Fable.Import
  open Browser.Types
  open Browser.Dom

  type UiState =
    { MousePos: float * float }

  let uiState: UiState ref = ref { MousePos = (0., 0.) }

  let requestAnimationFrame = window.requestAnimationFrame >> ignore

  let mutable myCanvas = document.getElementById "myCanvas" :?> HTMLCanvasElement // myCanvas is defined in public/index.html

  let ctx = myCanvas.getContext_2d()

  ctx.font <- "bold 48px serif"

  myCanvas.width <- Config.Width
  myCanvas.height <- Config.Height

  myCanvas.onmousemove <- (fun e -> uiState := { MousePos = (e.x, e.y) })

  let mutable board = World.createBoard()

  let drawBg (ctx: CanvasRenderingContext2D) =
    ctx.fillStyle <- !^"#eee"
    ctx.fillRect (0., 0., Config.Width, Config.Height)

  let seconds() = System.DateTime.Now.TimeOfDay.TotalSeconds

  let ms fps = 1. / (fps |> float)

  let rec step prevUpdated ts =
    let prevUpdated' =
      if seconds() - prevUpdated > (ms 20) then
        board <- World.update board
        seconds()
      else
        prevUpdated

    let (mposx, mposy) = (!uiState).MousePos

    drawBg ctx
    Grid.draw ctx
    World.draw ctx board
    Cursor.draw ctx (mposx, mposy)

    prevUpdated'
    |> step
    |> requestAnimationFrame

  seconds()
  |> step
  |> requestAnimationFrame

  printfn "done!"
