namespace Sketch

open SharpDX.Windows
open System
open D2D1Wrapper
open Sketch
open SharpDX
open SharpDX.Windows

module D2D1BackEnd =
    type Utilities.BorderMode with
        member x.ToD2D1 = match x with
                            |Utilities.BorderMode.Hard -> Direct2D1.BorderMode.Hard
                            |_ -> Direct2D1.BorderMode.Soft

    type Drawing.Size with
        member x.ToSketch = Utilities.V(float x.Width,float x.Height)

    type SharpDX.Size2F with
        member x.ToSketch = Utilities.V(float x.Width,float x.Height)



    type Utilities.InterpolationMode with        
        member x.ToD2D1 = match x with
                            |Utilities.InterpolationMode.Ansiotropic -> Direct2D1.InterpolationMode.Anisotropic
                            |Utilities.InterpolationMode.Cubic -> Direct2D1.InterpolationMode.Cubic
                            |Utilities.InterpolationMode.HighQualityCubic -> Direct2D1.InterpolationMode.HighQualityCubic
                            |Utilities.InterpolationMode.Linear -> Direct2D1.InterpolationMode.Linear
                            |Utilities.InterpolationMode.MultiSampleLinear -> Direct2D1.InterpolationMode.MultiSampleLinear
                            |Utilities.InterpolationMode.NearestNeighbour -> Direct2D1.InterpolationMode.NearestNeighbor
                                                         
    type Utilities.Vector2 with
        member x.ToD2D1 = match x with
                            |Sketch.Utilities.V(x,y) -> new SharpDX.Vector2(float32 x,float32 y)
    type SharpDX.Vector2 with
        member x.ToSketch = Utilities.V(float x.X,float x.Y)

    type Utilities.ArcSize with
        member x.ToD2D1 = match x with
                            |Utilities.Large  -> Direct2D1.ArcSize.Large
                            |_      -> Direct2D1.ArcSize.Small

    type Utilities.SweepDirection with
        member x.ToD2D1 = match x with
                            |Utilities.Clockwise  -> Direct2D1.SweepDirection.Clockwise
                            |_          -> Direct2D1.SweepDirection.CounterClockwise

    type Utilities.FillMode with
        member x.ToD2D1 = match x with
                            |Utilities.Winding -> Direct2D1.FillMode.Winding
                            |_       -> Direct2D1.FillMode.Alternate
    type Utilities.Channel with
        member x.ToD2D1 = match x with
                          |Utilities.Channel.A -> Direct2D1.ChannelSelector.A
                          |Utilities.Channel.B -> Direct2D1.ChannelSelector.B
                          |Utilities.Channel.G -> Direct2D1.ChannelSelector.G
                          |Utilities.Channel.R -> Direct2D1.ChannelSelector.R

    type Utilities.FigureBegin with
        member x.ToD2D1 = match x with
                            |Utilities.Filled -> Direct2D1.FigureBegin.Filled
                            |_      -> Direct2D1.FigureBegin.Hollow

    type Utilities.FigureEnd with
        member x.ToD2D1 = match x with
                            |Utilities.Closed -> Direct2D1.FigureEnd.Closed
                            |_      -> Direct2D1.FigureEnd.Open

    type Utilities.Combinator with
        member x.ToD2D1 = match x with
                            |Utilities.Union      -> Direct2D1.CombineMode.Union
                            |Utilities.Xor        -> Direct2D1.CombineMode.Xor
                            |Utilities.Intersect  -> Direct2D1.CombineMode.Intersect
                            |Utilities.Exclude    -> Direct2D1.CombineMode.Exclude

    type Utilities.DirectionalBlurOptimization with
        member x.ToD2D1 = match x with
                            |Utilities.DirectionalBlurOptimization.Balanced -> Direct2D1.DirectionalBlurOptimization.Balanced
                            |Utilities.DirectionalBlurOptimization.Quality -> Direct2D1.DirectionalBlurOptimization.Quality
                            |Utilities.DirectionalBlurOptimization.Speed -> Direct2D1.DirectionalBlurOptimization.Speed

    module Color =
        let ColortoD2D1 (r,g,b,a) = Color4(float32 r,float32 g,float32 b,float32 a)

    type Sketch.Brush.RadialGradientProperties with
            member x.ToD2D1 = new Direct2D1.RadialGradientBrushProperties(Center=x.Center.ToD2D1,GradientOriginOffset=x.GradientOriginOffset.ToD2D1,RadiusX=float32 x.RadiusX, RadiusY=float32 x.RadiusY)

    module Transformation = 
        let toD2D1Matrix (center: Utilities.Vector2) (t:Transformation.T) = 
                let rm = Matrix3x2.Rotation(float32 (t.Rotation * Math.PI), center.ToD2D1)
                let tm = Matrix3x2.Translation((t.Position - center).ToD2D1)
                let sm = Matrix3x2.Scaling(float32 t.ScaleX, float32 t.ScaleY, t.Position.ToD2D1)
                rm * tm * sm

    let (|ArcSegment|CBezierSegment|QBezierSegment|LineSegment|LinesSegment|QBeziersSegment|CBeziersSegment|) =
        let toQBezier (v1: Utilities.Vector2 ,v2: Utilities.Vector2) =
            let mutable b = new Direct2D1.QuadraticBezierSegment()
            do  b.Point1 <- v1.ToD2D1 
            b.Point2 <- v2.ToD2D1
            b   
        let toCBezier (v1: Sketch.Utilities.Vector2,v2: Sketch.Utilities.Vector2,v3: Sketch.Utilities.Vector2) =
            let mutable b = new Direct2D1.BezierSegment()
            do  b.Point1 <- v1.ToD2D1
                b.Point2 <- v2.ToD2D1
                b.Point3 <- v3.ToD2D1
            b
        function
        |Utilities.Arc(ad) -> 
            let mutable size = new Size2F()
            do  size.Height <- float32 ad.Height
                size.Width  <- float32 ad.Width
            let mutable arcSegment = new Direct2D1.ArcSegment()
            do  arcSegment.ArcSize <- ad.Size.ToD2D1
                arcSegment.Point <- ad.Point.ToD2D1
                arcSegment.RotationAngle <- float32 ad.Rotation
                arcSegment.Size <- size
                arcSegment.SweepDirection <- ad.SweepDirection.ToD2D1
            ArcSegment(arcSegment)
        |Utilities.CBezier(v1,v2,v3) -> CBezierSegment(toCBezier (v1,v2,v3))
        |Utilities.CBeziers(vs) -> CBeziersSegment(vs |> List.map toCBezier |> List.toArray)
        |Utilities.QBezier(v1,v2) -> QBezierSegment(toQBezier (v1,v2))
        |Utilities.QBeziers(vs) -> QBeziersSegment(vs |> List.map toQBezier |> List.toArray)
        |Utilities.LinePath(v1) -> LineSegment(v1)
        |Utilities.Lines(ls) -> LinesSegment(ls |> List.toArray)

    type Direct2D1.Geometry with 
        member x.Center = x.GetBounds().Center
    


    type Text.FontStretch with
        member x.ToD2D1 = 
            match x with
            |Text.FontStretch.Condensed -> DirectWrite.FontStretch.Condensed
            |Text.FontStretch.Expanded -> DirectWrite.FontStretch.Expanded
            |Text.FontStretch.ExtrExpanded -> DirectWrite.FontStretch.ExtraExpanded
            |Text.FontStretch.ExtraCondensed -> DirectWrite.FontStretch.ExtraCondensed
            |Text.FontStretch.Medium -> DirectWrite.FontStretch.Medium
            |Text.FontStretch.Normal -> DirectWrite.FontStretch.Normal
            |Text.FontStretch.SemiCondensed -> DirectWrite.FontStretch.SemiCondensed
            |Text.FontStretch.SemiExpanded -> DirectWrite.FontStretch.SemiExpanded
            |Text.FontStretch.UltraExpanded -> DirectWrite.FontStretch.UltraExpanded
            |Text.FontStretch.Ultracondensed -> DirectWrite.FontStretch.UltraCondensed
            |Text.FontStretch.Undefined -> DirectWrite.FontStretch.Undefined
            
    type Text.FontStyle with
        member x.ToD2D1 = 
            match x with
            |Text.Italic -> DirectWrite.FontStyle.Italic
            |Text.FontStyle.Normal -> DirectWrite.FontStyle.Normal
            |Text.FontStyle.Olique -> DirectWrite.FontStyle.Oblique
            
    type Text.FontWeight with
        member x.ToD2D1 =
            match x with
            |Text.FontWeight.Black -> DirectWrite.FontWeight.Black
            |Text.FontWeight.Bold -> DirectWrite.FontWeight.Bold
            |Text.FontWeight.DemiBold -> DirectWrite.FontWeight.DemiBold
            |Text.FontWeight.ExtraBlack -> DirectWrite.FontWeight.ExtraBlack
            |Text.FontWeight.ExtraBold -> DirectWrite.FontWeight.ExtraBold
            |Text.FontWeight.ExtraLight -> DirectWrite.FontWeight.ExtraLight
            |Text.FontWeight.Heavy -> DirectWrite.FontWeight.Heavy
            |Text.FontWeight.Light -> DirectWrite.FontWeight.Light
            |Text.FontWeight.Medium -> DirectWrite.FontWeight.Medium
            |Text.FontWeight.Normal -> DirectWrite.FontWeight.Normal
            |Text.FontWeight.Regular -> DirectWrite.FontWeight.Regular
            |Text.FontWeight.SemiBold -> DirectWrite.FontWeight.SemiBold
            |Text.FontWeight.SemiLight -> DirectWrite.FontWeight.SemiLight
            |Text.FontWeight.Thin -> DirectWrite.FontWeight.Thin
            |Text.FontWeight.UltraBlack -> DirectWrite.FontWeight.UltraBlack
            |Text.FontWeight.UltraBold -> DirectWrite.FontWeight.UltraBold
            |Text.FontWeight.UltraLight -> DirectWrite.FontWeight.UltraLight

    type Text.ParagraphAlignment with
        member x.ToD2D1 =
            match x with
            |Text.ParagraphAlignment.Center -> DirectWrite.ParagraphAlignment.Center
            |Text.ParagraphAlignment.Far -> DirectWrite.ParagraphAlignment.Far
            |Text.ParagraphAlignment.Near -> DirectWrite.ParagraphAlignment.Near

    type Text.TextAlignment with
        member x.ToD2D1 =
            match x with
            |Text.TextAlignment.Center -> DirectWrite.TextAlignment.Center
            |Text.TextAlignment.Justified -> DirectWrite.TextAlignment.Justified
            |Text.TextAlignment.Leading -> DirectWrite.TextAlignment.Leading
            |Text.TextAlignment.Trailing -> DirectWrite.TextAlignment.Trailing

    module Text =
        let toD2D1 (dm: DeviceManager) (fm: Text.Format) = 
            let res = new DirectWrite.TextFormat(dm.Factories.DirectWrite, fm.FontFamiliyName,fm.FontWeight.ToD2D1, fm.FontStyle.ToD2D1,fm.FontStretch.ToD2D1,float32 fm.FontSize)
            do  res.IncrementalTabStop <- float32 fm.IncrementalStop
                res.ParagraphAlignment <- fm.ParagraphAlignment.ToD2D1
                res.ReadingDirection <- if fm.ReadFromLeft then DirectWrite.ReadingDirection.LeftToRight
                                                           else DirectWrite.ReadingDirection.RightToLeft
                res.TextAlignment <- fm.TextAlignment.ToD2D1
                res.WordWrapping <- if fm.WordWrap  then DirectWrite.WordWrapping.Wrap
                                                    else DirectWrite.WordWrapping.NoWrap
            res
    module Shape =
        open Shape
        let rec geometry (f: Direct2D1.Factory1) sh = 
            match sh with
            |{Shape = Ellipse(w,h)} ->  new Direct2D1.EllipseGeometry(f,new Direct2D1.Ellipse(SharpDX.Vector2.Zero, float32 w, float32 h)) :> Direct2D1.Geometry
            |{Shape = Circle(r)} -> geometry f {sh with Shape = Ellipse(r,r)}
            |{Shape = Rectangle(w,h)} -> new Direct2D1.RectangleGeometry(f,new RectangleF(0.0f, 0.0f, float32 w, float32 h)) :> Direct2D1.Geometry
            |{Shape = Triangle(p1,p2,p3)}   -> 
                let pg = new Direct2D1.PathGeometry1(f)
                let sink = pg.Open()
                do  sink.SetFillMode(Direct2D1.FillMode.Winding)
                    sink.BeginFigure(p1.ToD2D1, Direct2D1.FigureBegin.Filled)
                    sink.AddLines([|p2.ToD2D1; p3.ToD2D1|])
                    sink.EndFigure(Direct2D1.FigureEnd.Closed)
                    sink.Close()  
                pg :> Direct2D1.Geometry
            |{Shape = RoundedRectangle(w,h,rx,ry)} ->
                let r = new Direct2D1.RoundedRectangle(RadiusX = float32 rx, RadiusY = float32 ry, Rect = new RectangleF(0.0f,0.0f,float32 w, float32 h))
                new Direct2D1.RoundedRectangleGeometry(f,r) :> Direct2D1.Geometry
            |{Shape = Path(pd)} -> //possible error here
                let pg = new Direct2D1.PathGeometry1(f)
                let sink = pg.Open()
                do  sink.SetFillMode(pd.Fill.ToD2D1)
                    sink.BeginFigure(pd.StartPoint.ToD2D1, pd.Begin.ToD2D1)
                    pd.Segments |> List.iter (function 
                                                    |ArcSegment ad -> sink.AddArc(ad)
                                                    |CBezierSegment b -> sink.AddBezier(b)
                                                    |QBezierSegment b -> sink.AddQuadraticBezier(b)
                                                    |LineSegment p -> sink.AddLine(p.ToD2D1)
                                                    |LinesSegment ps -> sink.AddLines(ps |> Array.map (fun p -> p.ToD2D1))
                                                    |QBeziersSegment bs -> sink.AddQuadraticBeziers(bs)
                                                    |CBeziersSegment bs -> sink.AddBeziers(bs)
                                                )
                    sink.EndFigure(pd.End.ToD2D1)
                    sink.Close()
                pg :> Direct2D1.Geometry 
            |{Shape = Line(p)} -> 
                let pg = new Direct2D1.PathGeometry1(f)
                let sink = pg.Open()
                do  sink.BeginFigure(Vector2.Zero, Direct2D1.FigureBegin.Hollow)
                    sink.AddLine(p.ToD2D1)
                    sink.EndFigure(Direct2D1.FigureEnd.Open)
                    sink.Close()
                pg :> Direct2D1.Geometry
            |{Shape = Group(ss,fm)} ->
                let gs = ss |> List.map (fun s -> 
                    let g = geometry f s
                    let trShape = setTransformation (Transformation.moveVR -g.Center.ToSketch s.Transformation) s
                    new Direct2D1.TransformedGeometry(f, g, (Transformation.toD2D1Matrix (g.Center.ToSketch) trShape.Transformation)) :> Direct2D1.Geometry) |> List.toArray //I have my doubts
                new Direct2D1.GeometryGroup(f,fm.ToD2D1, gs) 
                :> Direct2D1.Geometry
            |{Shape = Combination(c,s1,s2)} ->
                let pg = new Direct2D1.PathGeometry1(f)
                let sink = pg.Open()
                let g1 = geometry f s1
                let g2 = geometry f s2
                let trs1 = setTransformation (Transformation.moveVR -g1.Center.ToSketch s1.Transformation) s1
                let trs2 = setTransformation (Transformation.moveVR -g2.Center.ToSketch s2.Transformation) s2
                let trg1 = new Direct2D1.TransformedGeometry(f,g1,Transformation.toD2D1Matrix (g1.Center.ToSketch) trs1.Transformation)
                let trg2 = new Direct2D1.TransformedGeometry(f, g2, Transformation.toD2D1Matrix (g2.Center.ToSketch) trs1.Transformation)
                do  trg1.Combine(trg2,c.ToD2D1, sink)
                    sink.Close()
                pg 
                :> Direct2D1.Geometry
             
        let draw (dm: DeviceManager) sh = 
            let adjust center = Transformation.toD2D1Matrix center sh.Transformation
            let convBrush =
                    function
                    |Sketch.Brush.Solid(c) -> new Direct2D1.SolidColorBrush(dm.Context,Color.ColortoD2D1 c) :> Direct2D1.Brush
                    |Sketch.Brush.RadialGradient((pr: Brush.RadialGradientProperties),(stops)) -> 
                        let gradStops = new Direct2D1.GradientStopCollection(dm.Context, stops |> List.map (fun (c,p) -> new Direct2D1.GradientStop(Position=float32 p, Color = Color.ColortoD2D1 c)) |> List.toArray)
                        new Direct2D1.RadialGradientBrush(dm.Context,pr.ToD2D1, gradStops) :> Direct2D1.Brush
            let context = dm.Context
            let fixedStroke = dm.FixedStrokeStyle
            let rec drawIO sh =
                match sh with
                |{Shape = Ellipse(w,h)} ->
                    let ell = new Direct2D1.Ellipse(Vector2.Zero, float32 w, float32 h) 
                    let tm = adjust <| Utilities.V(float w/2.0,float h/2.0) 
                    tm,(fun (oBrush,width) -> context.DrawEllipse(ell,oBrush,width,fixedStroke)), fun fBrush -> context.FillEllipse(ell,fBrush)
                |{Shape = Circle(r)} -> drawIO {sh with Shape=Ellipse(r,r)}
                |{Shape = Rectangle(w,h)} -> 
                    let r = new RectangleF(0.0f, 0.0f, float32 w, float32 h)
                    let tm = adjust <| Utilities.V(float w/2.0,float h/2.0)
                    tm,(fun (oBrush,width) -> context.DrawRectangle(r,oBrush,width,fixedStroke)), fun fBrush -> context.FillRectangle(r,fBrush)
                |{Shape = RoundedRectangle(w,h,rx,ry)} -> 
                    let r = new Direct2D1.RoundedRectangle(RadiusX = float32 rx, RadiusY = float32 ry, Rect = new RectangleF(0.0f,0.0f,float32 w, float32 h))
                    let tm = adjust <| Utilities.V(float w/2.0,float h/2.0)  
                    tm,(fun (oBrush,width) -> context.DrawRoundedRectangle(r,oBrush,width,fixedStroke)), fun fBrush -> context.FillRoundedRectangle(r,fBrush)
                |{Shape = Line(_)} -> 
                    let geom = geometry dm.Factory sh 
                    let tm = adjust <| Utilities.Vector2.Zero
                    tm,(fun (oBrush,width) -> context.DrawGeometry(geom,oBrush, width, fixedStroke)), fun _ -> ()
                |{Shape = Triangle(_)} |{Shape = Group(_)}|{Shape = Combination(_)}|{Shape = Path(_)} ->
                    let geom = geometry dm.Factory sh
                    let tm = adjust <|  geom.Center.ToSketch
                    tm,(fun (oBrush,width) -> context.DrawGeometry(geom, oBrush, width, fixedStroke)), (fun fBrush -> context.FillGeometry(geom, fBrush))
            let tm,f1,f2 = drawIO sh
            match sh.Stroke with
            |{Stroke.Outline=Some(oBr,w);Stroke.Fill=Some(fBr)} ->
                let oBrush = convBrush oBr
                let fBrush = convBrush fBr
                let width = float32 w
                fun () ->   context.Transform <- tm
                            f1 (oBrush, width)
                            f2 fBrush
                            context.Transform <- Matrix3x2.Identity
            |{Outline=Some(oBr,w);Fill=None} ->
                let oBrush = convBrush oBr
                let width = float32 w
                fun () ->   context.Transform <- tm
                            f1 (oBrush, width)
                            context.Transform <- Matrix3x2.Identity
            |{Outline=None;Fill=Some(fBr)} -> 
                let fBrush = convBrush fBr
                fun () ->   context.Transform <- tm
                            f2 fBrush
                            context.Transform <- Matrix3x2.Identity
            |_  -> fun () -> ()

                    

    type Orientation = X|Y

    let coordLinesDir (form:RenderForm) o dist w c = //change brush!!
        let res = form.Size.ToSketch
        match o with
        |X -> [for h in 0.0 .. dist ..  res.Y -> Image.Figure(Shape.line (Utilities.V(0.0,h)) (Utilities.V(res.X,h)) c w)]
        |Y -> [for v in 0.0 .. dist .. res.X -> Image.Figure(Shape.line (Utilities.V(v,0.0)) (Utilities.V(v,res.Y)) c w)]

    let coordLines (form: RenderForm) scaleX scaleY w c =
        (coordLinesDir form X scaleX w c ) @ (coordLinesDir form Y scaleY w c )

    let coordsDraws (form: RenderForm) = 
        [10.0,1, (0,0,0,0.2) ;100.0,2, (0,0,0,0.5);500.0,3, Sketch.Color.Black]
        |> List.map (fun (sc,th,c) -> coordLines form sc sc th c)   
        |> List.reduce (fun a b -> a @ b)

    module Image =
        let rec baseEffect dm =
            function
                |Image.AffineTransformation(bm,im,s,t,_) -> 
                    let effect = new Direct2D1.Effects.AffineTransform2D(dm.Context)
                    effect.BorderMode <- bm.ToD2D1
                    effect.InterpolationMode <- im.ToD2D1
                    effect.TransformMatrix <- Transformation.toD2D1Matrix (Utilities.V(0.,0.)) t //what center?
                    effect :> Direct2D1.Effect
                |Image.Brightness(bp,wp,_) -> 
                    let effect = new Direct2D1.Effects.Brightness(dm.Context)
                    do  effect.BlackPoint <- bp.ToD2D1
                        effect.WhitePoint <- wp.ToD2D1
                    effect :> Direct2D1.Effect
                |Image.DirectionalBlur(angle,bordermode,optimization,sd,_) -> 
                    let effect = new Direct2D1.Effects.DirectionalBlur(dm.Context)
                    do  effect.Angle <- float32 angle
                        effect.BorderMode <- bordermode.ToD2D1
                        effect.Optimization <- optimization.ToD2D1
                        effect.StandardDeviation <- float32 sd
                    effect:> Direct2D1.Effect
                |Image.Composite(clamp,coeffs,_,_) -> 
                    let effect = new Direct2D1.Effects.ArithmeticComposite(dm.Context)
                    let x,y,z,w = coeffs
                    do  effect.ClampOutput <- clamp
                        effect.Coefficients <- SharpDX.Vector4(float32 x,float32 y,float32 z,float32 w)
                    effect :> Direct2D1.Effect
//                |Image.DisplacementMap(scale,yChannel,xChannel,eff,_) -> not working
//                    let effect = new Direct2D1.Effects.DisplacementMap(dm.Context)
//                    let ieffect = baseEffect dm eff
//                    do  effect.Scale <- float32 scale
//                        effect.XChannelSelect <- xChannel.ToD2D1
//                        effect.YChannelSelect <- yChannel.ToD2D1
//                        effect.SetInputEffect(1,ieffect)
//                    effect :> Direct2D1.Effect

        let rec bitmap dm img =
            match img with
            |Image.Bitmap(name,_) -> 
                dm.loadImageFile name
            |Image.Cache(image,tr) -> bitmap dm image
            |Image.Figure(sh:Shape.T) -> 
                let geom = Shape.geometry dm.Factory sh
                let thickness = match sh.Stroke with
                                |{Outline=Some(_,w)} -> w
                                |_                   -> 0
                let bm = dm.createBitmap (int (geom.GetBounds().Width) + 2 * thickness) (int (geom.GetBounds().Height) + 2 * thickness)
                let center = Transformation.positionV ((Shape.geometry dm.Factory sh).Center.ToSketch + Utilities.V(float thickness, float thickness))
                let draws  = Shape.draw dm (Shape.setTransformation center sh)
                let original = dm.Context.Target
                do  dm.Context.BeginDraw() 
                    dm.Context.Target <- bm
                    draws ()
                    dm.Context.Target <- original
                    dm.Context.EndDraw()
                bm
            |Image.Text(text,_) -> 
                let format = Text.toD2D1 dm text.Format
                let layout = new DirectWrite.TextLayout(dm.Factories.DirectWrite,text.Text, format,0.f,0.f)
                let brush = new Direct2D1.SolidColorBrush(dm.Context,Color4.Black)
                let width, heigth = 
                    match text.Rectangle with
                    |Some(v) -> float32 v.X, float32 v.Y
                    |None   -> layout.Metrics.Width,layout.Metrics.Height
                let bm = dm.createBitmap (int width) (int heigth)
                let original = dm.Context.Target
                do  dm.Context.BeginDraw()
                    dm.Context.Target <- bm
                    dm.Context.Transform <- Matrix3x2.Identity
                    dm.Context.DrawText(text.Text, Text.toD2D1 dm text.Format, new RectangleF(0.f,0.f,width,heigth), brush ) 
                    dm.Context.Target <- original
                    dm.Context.EndDraw()
                bm
            |Image.Effect(e,_) ->                 
                match e with
                |_ -> effect dm e

        and effect dm eff = //create different codepaths for effect drawing and bitmap drawing. wasteful caching atm. x.drawimage(d2d1.Effect)
            let draw (eff: Direct2D1.Effect) (sm :Direct2D1.Bitmap1) =
                    let bm = dm.createBitmap (int sm.Size.Width) (int sm.Size.Height)
                    let original = dm.Context.Target
                    dm.Context.BeginDraw()
                    dm.Context.Target <- bm
                    dm.Context.DrawImage(eff)
                    dm.Context.Target <- original
                    dm.Context.EndDraw()
                    bm        
            match eff with
                |Image.AffineTransformation(bm,im,s,t,img) -> 
                    let be = baseEffect dm eff
                    let sourceBM = bitmap dm img
                    be.SetInput(0,sourceBM,Bool(true))
                    draw be sourceBM
                |Image.Brightness(bp,wp,img) -> 
                    let be = baseEffect dm eff
                    let sourceBM = bitmap dm img
                    be.SetInput(0,sourceBM,Bool(true))
                    draw be sourceBM
                |Image.DirectionalBlur(angle,bordermode,optimization,sd,img) -> 
                    let be = baseEffect dm eff
                    let sourceBM = bitmap dm img
                    be.SetInput(0,sourceBM,Bool(true))
                    draw be sourceBM
                |Image.Composite(clamp,coeffs,img1,img2) -> 
                    let be = baseEffect dm eff
                    let source1 = bitmap dm img1
                    let source2 = bitmap dm img2
                    be.SetInput(0,source1,Bool(true))
                    be.SetInput(1,source2,Bool(true))
                    draw be source1
//                |Image.DisplacementMap(s,y,x,e,img) ->
//                    let be = baseEffect dm eff
//                    let sourceBM = bitmap dm img
//                    be.SetInput(0,sourceBM,Bool(true))
//                    draw be sourceBM

        let drawBitmap dm tr (bm: Direct2D1.Bitmap1) =
                let center = Utilities.V(float bm.Size.Width / 2.0, float bm.Size.Height / 2.0)
                let tm = Transformation.toD2D1Matrix center tr
                fun () -> 
                    dm.Context.Transform <- tm 
                    dm.Context.DrawImage(bm) 
                    dm.Context.Transform <- Matrix3x2.Identity
                       
        let draw (dm: DeviceManager) img =
            match img with
            |Image.Figure(shape) -> Shape.draw dm shape
            |Image.Cache(image,tr) -> 
                Image.setTransformation tr image
                |> bitmap dm
                |> drawBitmap dm tr
            |Image.Bitmap(name,tr) -> 
                dm.loadImageFile name 
                |> drawBitmap dm tr
            |Image.Text(text,tr) -> //change to drawtext, not cached bitmap.  
                let bm = bitmap dm img 
                let center = Utilities.V(float bm.Size.Width / 2.0, float bm.Size.Height / 2.0)
                let tm = Transformation.toD2D1Matrix center tr
                fun () -> 
                    dm.Context.Transform <- tm 
                    dm.Context.DrawImage(bm) 
                    dm.Context.Transform <- Matrix3x2.Identity
            |Image.Effect(effect,tr) -> 
                let bm = bitmap dm img 
                let center = Utilities.V(float bm.Size.Width / 2.0, float bm.Size.Height / 2.0)
                let tm = Transformation.toD2D1Matrix center tr
                fun () -> 
                    dm.Context.Transform <- tm 
                    dm.Context.DrawImage(bm) 
                    dm.Context.Transform <- Matrix3x2.Identity

        let resize dm width height margin (img: Image.T) = 
            let factor = bitmap dm img |> fun b -> min (float32 (width - margin) / (b.Size.Width * float32 img.Transformation.ScaleX)) (float32 (height - margin) / (b.Size.Height * float32 img.Transformation.ScaleY)) |> float
            Image.scale factor img

        let debugView (dm: DeviceManager) n ind (imgs: List<Image.T>) = //add interactivity and/or time based shifts
            let form = match dm.RenderTarget with
                        |Hwnd(form) -> form.RenderForm
            let width,height = form.Size.Width ,form.Size.Height
            let WidthImg,heightImg = width /(2*n),height /n
            //http://stackoverflow.com/questions/3999584/f-split-sequence-into-sub-lists-on-every-nth-element
            let splitEach n s =
                seq {
                    let r = ResizeArray<_>()
                    for x in s do
                        r.Add(x)
                        if r.Count = n then
                            yield r.ToArray()
                            r.Clear()
                    if r.Count <> 0 then
                        yield r.ToArray()
                }
            let lines = coordLines form (float heightImg) (float WidthImg * 2.0) 2 (Sketch.Color.Black |> Sketch.Color.veryTransparent)
            let splitImgs = splitEach (n*n) imgs |> Seq.map (fun ls -> List.ofSeq ls) |> List.ofSeq 
            List.nth splitImgs (ind % splitImgs.Length) |> List.mapi (fun i img ->  Image.position (WidthImg/2 + i % n * WidthImg * 2) (heightImg/2 + i/n * heightImg) img,
                                                                                    Image.text (Printf.sprintf "%A" img) Text.timesNewRoman 15 None (Transformation.position (WidthImg + WidthImg / 2 + i % n * WidthImg * 2) (heightImg/2 + i/n * heightImg)) 
                                                                          ) |> List.unzip
                                                                            |> fun (imgs,dus) -> imgs @ dus
                                                                            |> List.map (fun i -> resize dm WidthImg heightImg 12 i)
                                                                            |> fun ls -> ls @ lines
            
    type DeviceManager with 
        member x.draw (bs: List<Image.T>) =
            let drawFunctions = bs |> List.map (fun i -> Image.draw x i)
            match x.RenderTarget with
            |Hwnd(form) ->  let draws = match x.Coordinates with
                                            |true -> (coordsDraws form.RenderForm) |> List.map (fun i -> Image.draw x i) 
                                                                                   |> fun is -> is @ drawFunctions
                                            |false -> [] @ drawFunctions
                            form.Loop
                                (fun () ->
                                    x.Context.Clear(Nullable(Color4.White))
                                    draws |> List.iter (fun f -> f ())
                                    )

    let drawDef (bs: List<Image.T>) =
        let dm = D2D1Wrapper.deviceManager D2D1Wrapper.debug
        dm.draw bs
             
             



 //maybe useful later            
//                        let glyphRun = new DirectWrite.GlyphRun()
//                        let fontFace = glyphRun.FontFace
//                        let collection = dm.factories.directwrite.getsystemfontcollection(bool(false))
//                        let ind = collection.findfamilyname text.format.fontfamiliyname |> snd
//                        let fontfamily =  collection.getfontfamily ind
//                        let font = fontFamily.GetFirstMatchingFont(text.Format.FontWeight.ToD2D1, text.Format.FontStretch.ToD2D1, text.Format.FontStyle.ToD2D1)
//                        let face = new DirectWrite.FontFace(font)
//                        let glyphIndices = face.GetGlyphIndices (text.Text.ToCharArray() |> Array.map (fun c -> int c))
//                        let pg = new Direct2D1.PathGeometry1(dm.Factory)
//                        let sink = pg.Open()
//                        do  face.GetGlyphRunOutline(float32 text.Format.FontSize, glyphIndices, null, null, false, not text.Format.ReadFromLeft, sink)
//                            sink.Close()
//                        (pg.GetBounds().Width) + 2.f, (pg.GetBounds().Height) + float32 text.Format.FontSize * 0.16f //magic numbers 4evah!!!