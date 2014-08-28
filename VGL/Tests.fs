namespace Sketch
open Sketch
open D2D1BackEnd
open Sketch.Utilities

module Tests =
    module im = Image   
    module tr = Transformation
    module st = Stroke
    module sh = Shape
    module te = Text
    let koala = im.loadImage @"C:\Users\eugen\Pictures\Koala.jpg" 
    let yellowFlowers = im.loadImage @"C:\Users\eugen\Pictures\yf.jpg" 
    let redFlowers = im.loadImage @"C:\Users\eugen\Pictures\rf.jpg"
    let triangle1 = sh.triangleIsoc 100 50 st.NavySilver (tr.position 200 200)
    let byTriangle = sh.triangleIsoc 100 50 st.BlueRed (tr.position 200 200) |> im.Figure
    let rgTriangle = sh.triangleIsoc 100 50 st.MagentaRed (tr.position 200 200) |> im.Figure
    let brightTriangle1 = im.brightness (V(1.0,0.1)) (V(0.,1.)) rgTriangle
    let brightTriangle2 = im.brightness (V(1.0,0.9)) (V(0.,1.)) rgTriangle
    let dirBlurTriangle = im.dirBlurDef 1. 5.2 rgTriangle
    let dirBlurKoala = im.dirBlurDef 1. 5.2 koala
    let compyRFlowers = im.composite false (0.5,0.5,1.,0.5) yellowFlowers redFlowers yellowFlowers.Transformation
    let mountainPath = Utilities.Lines([V(481.,146.);V(449.,181.);V(433.,159.);V(401.,214.);V(381.,199.);V(323.,263.);V(575.,263.)])
    let mountainPathImg = im.Figure{Shape=(sh.Path(Path.path (V(575.,263.)) [mountainPath] Utilities.FigureBegin.Filled Utilities.FigureEnd.Closed FillMode.Winding)); Stroke = st.BlackOM; Transformation=tr.position 600 600}
    let sunPath = Path.path (V(270.,255.)) [(Path.arc Utilities.ArcSize.Small (V(440.,255.)) 0.0 85 85 Utilities.SweepDirection.Clockwise)] Utilities.FigureBegin.Filled Utilities.FigureEnd.Closed FillMode.Winding
    let sunPathImg = im.path st.RedF sunPath (tr.Zero)
    let path3 = im.path (st.outline Color.Red Stroke.Medium) (Path.path (V(299.,182.)) [Utilities.CBezier(V(299.,182.),V(294.,176.),V(285.,178.)); Utilities.CBezier(V(276.,179.),V(272.,173.),V(272.,173.))] Utilities.FigureBegin.Hollow Utilities.FigureEnd.Open FillMode.Alternate) (tr.scale 10. (tr.position 200 200)) 
    let compTriangles = im.composite false (0.5,0.5,1.,0.5) brightTriangle1 dirBlurTriangle brightTriangle1.Transformation
    let text1 = im.text "Das ist ein grandioser Test1." Text.arial 30 (Some(V(300.,200.))) (tr.position 500 500)
    let text2 = im.text "Das ist ein grandioser Test2." Text.arial 60 None (tr.position 500 400)  
    let innerText1 = te.text "HuhuhuAAdxcsdf \n ewrso~~!§(%$$§&&%/" te.defFormat None st.RedF |> te.wordWrapping false |> te.size 80 |> te.font te.comicSans
    let text3 = im.Text(innerText1, tr.position 1000 700)
//    let star1 = sh.line Vector2.Zero (V(10.0,30.0)) (st.outlined Color.Blue 6.) |> im.Figure
    let set1 = [koala;
                yellowFlowers;
                yellowFlowers |> im.rotate 0.5;
//                star1;
                redFlowers;
                byTriangle;
                rgTriangle;
                brightTriangle1;
                brightTriangle2;
                dirBlurKoala;
                dirBlurTriangle;
                compyRFlowers;
                mountainPathImg;
                sunPathImg;
                compTriangles;
                text1;
                text2;
                text3] 
//                        |> List.mapi (fun i e ->    im.position (((i % 3)) * 600 + 150) (((i / 3)) * 300+150) e,
//                                                    (im.text (Printf.sprintf "%A" e) Text.timesNewRoman 15 None (tr.position (((i % 3)) * 600 + 450) (((i / 3)) * 300 + 150)))
//                                                    )   |> List.unzip
//                                                        |> fun (imgs,dus) -> imgs @ dus