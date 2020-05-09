namespace NodeSim

open Fable.Core.Util

module JsInterop =
  type INumber =
    abstract toFixed: float -> string

  let Number: INumber = jsNative
