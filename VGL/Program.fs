namespace Sketch

open System
open Sketch


module Program =
    open Sketch
    open Utilities
    open D2D1BackEnd
//    module bo = Body
//    module br = Brushes
//
    module s = Sketch
    module t = Transformation
    module i = Image
    module w = D2D1Wrapper
    module b = D2D1BackEnd
    module u = Utilities
    type T1 =   |A of int*int
                |B of string*int

//    printfn "Image List: %A" Tests.workingImgs
    let dm = D2D1Wrapper.deviceManager (D2D1Wrapper.debug  |> w.coords true) 
//    let dm = D2D1Wrapper.deviceManager (D2D1Wrapper.debug ) 

//    do  printfn "%A" (match dm.RenderTarget with
//                        |D2D1Wrapper.Hwnd(form) -> form.RenderForm.Width,form.RenderForm.Height)
    do  dm.draw (Tests.set1 |> i.debugView dm 5 1)
//    do  dm.draw (Tests.set1)
//    do  dm.draw [Tests.text1;Tests.text2;Tests.text3]
//  