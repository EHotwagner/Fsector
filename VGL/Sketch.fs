namespace Sketch

module Utilities =
    type Vector2 = V of float*float with
      static member Zero : Vector2 = V(0.0,0.0)
      static member ( + ) (V(x1,y1),V(x2,y2)) = V(x1+x2,y1+y2) 
      static member ( + ) (V(x,y),k:float) = V(x+k,y+k)
      static member ( + ) (k:float,v:Vector2) = v + k
      static member ( ~- ) (V(x,y)) = V(-x,-y)
      static member ( - ) (v1,v2:Vector2) = v1 + (-v2)
      static member ( - ) (v:Vector2,k:float) = v + (-k)
      static member ( - ) (k:float,v:Vector2):Vector2 = k + (-v)
      static member ( * ) (V(x1,y1),V(x2,y2)):Vector2 = V(x1*x2,y1*y2) 
      static member ( * ) (V(x,y),f:float):Vector2 = V(x*f,y*f)
      static member ( * ) (f:float,V(x,y)):Vector2 = V(x*f,y*f)
      static member ( / ) (v:Vector2,f:float<'b>):Vector2 = v * (1.0 / f)
      member x.Length : float = 
        match x with
        |V(x,y) -> sqrt(x * x + y * y)
      static member Distance(v1:Vector2,v2:Vector2) = (v1-v2).Length
      static member Normalize(v:Vector2):Vector2 = v / v.Length
      member this.Normalized = this / this.Length
      static member Dot(V(x1,y1),V(x2,y2)) = x1 * x2 + y1 * y2
      member x.X = match x with
                    |V(x,_) -> x
      member x.Y = match x with
                    |V(_,y) -> y
    type FillMode =
            |Winding
            |Alternate

    type DirectionalBlurOptimization = 
            |Balanced
            |Quality
            |Speed

    type Channel =
            |R
            |G
            |B
            |A

    type InterpolationMode =
            |Ansiotropic
            |Cubic
            |HighQualityCubic
            |Linear
            |MultiSampleLinear
            |NearestNeighbour

    type BorderMode = 
            |Hard
            |Soft

    type ArcSize =
        |Large
        |Small

    type SweepDirection =
        |Clockwise
        |CounterClockwise

    type ArcData = {Size: ArcSize; Point: Vector2; Rotation: float; Height: int; Width: int; SweepDirection: SweepDirection}

    type Segment = 
        |Arc of ArcData
        |CBezier of Vector2 * Vector2 * Vector2
        |CBeziers of List<Vector2*Vector2*Vector2>
        |QBezier of Vector2 * Vector2
        |LinePath of Vector2
        |Lines of List<Vector2>
        |QBeziers of List<Vector2*Vector2>

    type FigureBegin = 
        |Filled
        |Hollow

    type FigureEnd =
        |Closed 
        |Open

    type Combinator =
        |Union 
        |Xor 
        |Intersect 
        |Exclude 

module Sketch =
    open System
    open Utilities
    let (|Float|) i = float i
    module Color = 
        type T = int*int*int*float 
        let White = (255,255,255,1.0)
        let Black = (0,0,0,1.0)
        let Red = (255,0,0,1.0)
        let Lime = (0,255,0,1.0)
        let Green = (0,128,0,1.0)
        let Blue = (0,0,255,1.0)
        let Yellow = (255,255,0,1.0)
        let Cyan = (0,255,255,1.0)
        let Magenta = (255,0,255,1.0)
        let Silver = (192,192,192,1.0)
        let Gray = (128,128,128,1.0)
        let Maroon = (128,0,0,1.0)
        let Olive = (128,128,0,1.0)
        let Purple = (128,0,128,1.0)
        let Teal = (0,128,128,1.0)
        let Navy = (0,0,128,1.0)

        let veryTransparent (r,g,b,_) = r,g,b,0.25
        let semiTransparent (r,g,b,_) = r,g,b,0.5
        let veryOpague (r,g,b,_) = r,g,b,0.75
        let opague (r,g,b,_) = r,g,b,1.0
              
    module Brush = 
        type RadialGradientProperties = {Center: Vector2; GradientOriginOffset: Vector2; RadiusX:int; RadiusY:int}
        type T = 
            |Solid of Color.T
            |RadialGradient of RadialGradientProperties*List<Color.T*float>

    module Stroke =
        let VeryThick = 6
        let Thick = 4
        let Medium = 3
        let Thin = 2
        let VeryThin = 1
        type T =
            {Outline: Option<Brush.T*int>;Fill:Option<Brush.T>}
        let outline c w = {Outline=Some(Brush.Solid(c),w);Fill=None}
        let chOutline (st:T) c w = {st with Outline = Some(Brush.Solid(c),w)}
        let fill c = {Outline=None;Fill=Some(Brush.Solid(c))}
        let chFill (st:T) c = {st with Fill=Some(Brush.Solid(c))}
        let full co w ci = {Outline=Some(Brush.Solid(co),w);Fill=Some(Brush.Solid(ci))}
        let thickness w st = 
            match st with
            |{Outline=Some(b,ow)} -> {st with Outline=Some(b,w)}
            |_  -> st

        let BlackOM = outline Color.Black Medium
        let RedOM = outline Color.Red Medium
        let BlackF= fill Color.Black
        let RedF= fill Color.Red 
        let YellowGreen = full Color.Yellow Medium Color.Green
        let GreenYellow = full Color.Green Medium Color.Yellow
        let RedBlue = full Color.Red Medium Color.Blue
        let BlueRed = full Color.Blue Medium Color.Red
        let BlackWhite = full Color.Black Medium Color.White
        let WhiteBlack = full Color.White Medium Color.Black
        let MagentaRed = full Color.Magenta Medium Color.Red
        let RedMagenta = full Color.Red Medium Color.Magenta
        let SilverNavy = full Color.Silver Medium Color.Navy
        let NavySilver = full Color.Navy Medium Color.Silver

    module Path = //only works for filled path!!!

        type T = {StartPoint: Vector2; Segments: List<Segment>; Begin: FigureBegin; End: FigureEnd; Fill: FillMode}     
        let path sp ss fb fe fm = {StartPoint = sp; Segments = ss; T.Begin = fb; T.End = fe; T.Fill = fm}
        let arc size point rot height width sweep = Arc({Size = size; Point = point; Rotation = rot; Height = height;Width= width; SweepDirection= sweep})

    module Transformation =
        type T = {Position: Vector2; Rotation: float; ScaleX: float; ScaleY: float} 
        let transformation p r s = {Position = p; Rotation = r; ScaleX = s; ScaleY = s}
        let add t2 (t1: T) = {Position = t1.Position + t2.Position; Rotation = t1.Rotation + t2.Rotation; ScaleX = t1.ScaleX + t2.ScaleX; ScaleY = t1.ScaleY + t2.ScaleY}
        let scale s (t:T) = {t with ScaleX = t.ScaleX * s; ScaleY = t.ScaleY * s}
        let scaleXR s (t: T) = {t with ScaleX = t.ScaleX * s}
        let scaleXA s (t:T) = {t with ScaleX = t.ScaleX}
        let scaleYR s (t: T) = {t with ScaleY = t.ScaleY * s}
        let scaleYA s (t:T) = {t with ScaleY = t.ScaleY}
        let rotate s (t:T) = {t with Rotation = t.Rotation + s}
        let moveVA p (t:T) = {t with Position = p}
        let moveVR p (t:T) = {t with Position = t.Position + p}
        let moveA x y (t:T) = moveVA (V(float x,float y)) t
        let moveR x y (t:T) = moveVR (V(float x,float y)) t
        let turn p (t:T) = {t with Rotation = t.Rotation + Math.PI}
        let Zero = {Position = Vector2.Zero; Rotation = 0.0; ScaleX = 1.0; ScaleY = 1.0}
        let position x y = {Zero with Position = V(float x,float y)}
        let positionV v = {Zero with Position = v}

    module Shape = 
        type Data =
            |Line of Vector2 
            |Ellipse of width:int * height:int 
            |Circle of radius:int 
            |Triangle of Vector2*Vector2*Vector2 //should not be shape since no correspondence in d2d1...
            |Rectangle of width:int * height:int 
            |RoundedRectangle of width:int * height:int * radiusX:int * radiusY:int 
            |Path of Path.T 
            |Group of List<T>*FillMode:FillMode
            |Combination of Combinator*T*T
        and T = {Shape:Data;Stroke:Stroke.T;Transformation:Transformation.T} 

        let setTransformation tr (s:T) = {s with Transformation=tr}
        let setBrush sr (s:T) = {s with Stroke=sr}
        let setShape (d) (s:T) = {s with Shape=d}
        let line sp ep (col: Color.T) (w:int) = {Shape = Line(ep - sp);Stroke = {Outline=Some(Brush.Solid(col),w);Fill=None};Transformation = Transformation.Zero |> Transformation.moveVA sp} 
        let ellipse w h sr tr = {Shape = Ellipse(w,h); Stroke=sr;Transformation=tr}
        let cirlce r sr tr = {Shape=Circle(r);Stroke=sr;Transformation=tr}
        let triangle p1 p2 p3 sr tr = {Shape=Triangle(p1,p2,p3);Stroke=sr;Transformation=tr}
        let triangleIsoc (Float a) (Float h) sr tr = {Shape=Triangle(Vector2.Zero, V(a, 0.0),V(a/2.0,h));Stroke=sr;Transformation=tr}
        let rectangle w h sr tr = {Shape=Rectangle(w,h);Stroke=sr;Transformation=tr}
        let rrectangle w h rx ry sr tr = {Shape=RoundedRectangle(w,h,rx,ry);Stroke=sr;Transformation=tr}
        let union (s1:T) s2 = {Shape= Combination(Union,s1,s2); Stroke=s1.Stroke; Transformation= {s1.Transformation with Position = (s1.Transformation.Position + s2.Transformation.Position) / 2.0}}
        let xor s1 s2 = {Shape= Combination(Xor,s1,s2); Stroke=s1.Stroke; Transformation= {s1.Transformation with Position = (s1.Transformation.Position + s2.Transformation.Position) / 2.0}}
        let intersect s1 s2 = {Shape= Combination(Intersect,s1,s2); Stroke=s1.Stroke; Transformation= {s1.Transformation with Position = (s1.Transformation.Position + s2.Transformation.Position) / 2.0}}
        let exclude s1 s2 = {Shape= Combination(Exclude,s1,s2); Stroke=s1.Stroke; Transformation= {s1.Transformation with Position = (s1.Transformation.Position + s2.Transformation.Position) / 2.0}}            
        let group fm ss sr = {Shape= Group(ss,fm);Stroke=sr;Transformation=Transformation.Zero}

        let starThin spikes size sr = 
            [1..spikes] |> List.mapi (fun i s -> {Shape=Line(V(5.0,float size));Stroke=sr;Transformation=Transformation.Zero |> Transformation.rotate (2.0 / (float spikes) * float i) } ) |> ignore
            line Vector2.Zero (V(20.,20.0)) Color.Black 5 
//                        |> (fun _ -> {Shape=Line(V(10.,10.));Brush=br;Transformation=Transformation.Zero})
//                        |> List.reduce (fun s1 s2 -> union s1 s2)

            
            

    module Text =
        type FontStretch = 
            |Condensed
            |Expanded
            |ExtraCondensed
            |ExtrExpanded
            |Medium
            |Normal
            |SemiCondensed
            |SemiExpanded
            |Ultracondensed
            |UltraExpanded
            |Undefined
        type FontStyle =
            |Italic
            |Normal
            |Olique
        type FontWeight =
            |Black
            |Bold
            |DemiBold
            |ExtraBlack
            |ExtraBold
            |ExtraLight
            |Heavy
            |Light
            |Medium
            |Normal
            |Regular
            |SemiBold
            |SemiLight
            |Thin
            |UltraBlack
            |UltraBold
            |UltraLight
        type ParagraphAlignment =
            |Center
            |Far
            |Near
        type TextAlignment =
            |Center
            |Justified
            |Leading
            |Trailing

            //implement FontCollections
        type Format = { 
                        FontFamiliyName:string; 
                        FontSize: int; 
                        FontWeight: FontWeight; 
                        FontStyle: FontStyle; 
                        FontStretch: FontStretch; 
                        IncrementalStop: float; 
                        ParagraphAlignment: ParagraphAlignment; 
                        ReadFromLeft: bool; 
                        TextAlignment: TextAlignment; 
                        WordWrap: bool}

        type T = {Text:string;Format: Format; Rectangle: Option<Utilities.Vector2>; Stroke:Stroke.T}
        let text s f size st = {Text=s;Format=f;Rectangle=size;Stroke=st}
        let format f (t:T) = {t with Format=f} 
        let setText st (t:T) = {t with Text=st} 
        let font fo (t:T) = {t with Format = {t.Format with FontFamiliyName = fo}}
        let size si (t:T) = {t with Format = {t.Format with FontSize = si}}
        let style st t = {t with Format = {t.Format with FontStyle = st}}
        let paragraphAlignment al t = {t with Format = {t.Format with ParagraphAlignment = al}}
        let incementalStop is t = {t with Format = {t.Format with IncrementalStop = is}}
        let weight w t = {t with Format = {t.Format with FontWeight = w}}
        let stretch st t = {t with Format = {t.Format with FontStretch = st}}
        let readFromLeft b t = {t with Format = {t.Format with ReadFromLeft = b}}
        let textAlignment al t = {t with Format = {t.Format with TextAlignment = al}}
        let wordWrapping b t = {t with Format = {t.Format with WordWrap = b}}

        let arial = "Arial"
        let angsanaUPC = "AngsanaUPC"
        let arabic = "Arabic Typesetting"
        let batang = "Batang"
        let cambria = "Cambria"
        let comicSans = "Comic Sans MS"
        let consolas = "Consolas"
        let constantia = "Constantia"
        let courierNew = "Courier New"
        let georgia = "Georgia"
        let impact = "Impact"
        let lucidaConsole = "Lucida Console"
        let symbol = "Symbol"
        let timesNewRoman = "Times New Roman"
        let trebuchet = "Trebuchet MS"
        let verdana = "Verdana"
        let webDings = "Webdings"
        let wingDings = "Wingdings"
        let defFormat = {   FontFamiliyName = arial; 
                            FontSize = 16; 
                            FontWeight = FontWeight.Normal; 
                            FontStyle=FontStyle.Normal;
                            FontStretch=FontStretch.Normal;
                            IncrementalStop=8.;  
                            ParagraphAlignment=ParagraphAlignment.Center; 
                            ReadFromLeft=true;
                            TextAlignment=TextAlignment.Center;
                            WordWrap=false}

    module Image = 
        type EffectData =   
            |AffineTransformation of BorderMode*InterpolationMode*float*Transformation.T * Source:T
            |Brightness of BlackPoint:Vector2*WhitePoint:Vector2 * Source:T
            |DirectionalBlur of Angle:float*BorderMode*DirectionalBlurOptimization*StandardDeviation:float*Source:T
            |Composite of  ClampOutPut:bool*Coefficients:(float*float*float*float)*Source1:T*Source2:T
//            |DisplacementMap of Scale:float*YChannel:Channel*XChannel:Channel * Input:T1 * Source:T

        and T =  
            |Bitmap of string * Transformation.T 
            |Cache of T * Transformation.T //need seperate trans because unclear what happens when tr changes.
            |Text of Text.T * Transformation.T
            |Figure of Shape.T 
            |Effect of EffectData*Transformation.T
            with
            member x.Transformation =
                match x with
                |Bitmap(_,tr) -> tr
                |Figure(s) -> s.Transformation
                |Text(_,tr) -> tr
                |Effect(_,tr) -> tr
                |Cache(_,tr) -> tr
        let rec setTransformation tr = 
            function
            |Bitmap(bm,_) -> Bitmap(bm,tr)
            |Figure(s) -> Figure(Shape.setTransformation tr s)
            |Text(t,_) -> Text(t,tr)
            |Effect(eff,_) -> Effect(eff,tr)
            |Cache(img,_) -> Cache(img,tr)

        let position x y (img:T) = setTransformation (Transformation.moveA x y img.Transformation ) img
        let cache img = //could be clever. Beware. 
            match img with
            |Bitmap(_) -> img
            |Cache(_) -> img
            |Text(_) -> Cache(img,img.Transformation)
            |Figure(_) -> Cache(img,img.Transformation)
            |Effect(_) -> Cache(img,img.Transformation) //redundant at the moment.

        let text (s:string) (font: string) fontSize rectangle tr = Text((Text.text s Text.defFormat rectangle  Stroke.BlackOM |> Text.font font |> Text.size fontSize), tr) 
        
        let loadImage s = Bitmap(s,Transformation.Zero)
        let scale s (img:T) = setTransformation (Transformation.scale s img.Transformation) img
        let scaleX s (img:T) = setTransformation (Transformation.scaleXR s img.Transformation) img
        let scaleY s (img:T) = setTransformation (Transformation.scaleYR s img.Transformation) img
        ///in PI * Rad
        let rotate s (img:T) = setTransformation (Transformation.rotate s img.Transformation) img
        let affineTransform bm im shar tr img = Effect(AffineTransformation(bm,im,shar,tr,img),img.Transformation)
        let affineDef tr img = affineTransform BorderMode.Hard InterpolationMode.Cubic 0.8 tr img
        let brightness bp wp img = Effect(Brightness(bp,wp,img),img.Transformation)
        let dirBlur an bm op sd img = Effect(DirectionalBlur(an,bm,op,sd,img),img.Transformation)
        let dirBlurDef an sd img = dirBlur an BorderMode.Soft DirectionalBlurOptimization.Quality sd img
        let composite clamp coeffs img1 img2 tr = Effect(Composite(clamp,coeffs, img1,img2),tr)
//        let displacement s x y sEffect img = Effect(DisplacementMap(s,x,y,sEffect,img),img.Transformation)
        let path sr pa tr = Figure{Shape = Shape.Path(pa);Stroke=sr;Transformation=tr}
//    let createRandomBody (minSize:int) (maxSize:int) (Corner1:Vector2) (Corner2: Vector2) =
//        let rnd = new System.Random()
//        let rshape = 
//            function
//            |0  -> 
//   