namespace Sketch
open SharpDX
open SharpDX.Windows
module D2D1Wrapper =
    type Devices = {D2D1: Direct2D1.Device1; D3D11: Direct3D11.Device; DXGI: DXGI.Device1}
    type Factories = {D2D1: Direct2D1.Factory2; DXGI: DXGI.Factory2; WIC: WIC.ImagingFactory2; DirectWrite: DirectWrite.Factory1}

    type Screen = //put into config
        FullScreen
        |Windowed of int*int

    type Target = Form of Name:string*Screen:Screen
    type HwndForm = {RenderForm: RenderForm; SwapChain: DXGI.SwapChain1; Loop: ((unit -> unit)-> unit)}
    type RenderTarget = Hwnd of HwndForm
    
    type Configuration = {Target: Target; Debug: bool; Antialiasing: Direct2D1.AntialiasMode; Coordinates: bool} //ugly mixture between d2d1 types and custom, seperate!

    let debug = {Target=Form("Test",FullScreen);Debug=true;Antialiasing=Direct2D1.AntialiasMode.PerPrimitive; Coordinates=true}
    let release = {Target=Form("Release",FullScreen);Debug=false;Antialiasing=Direct2D1.AntialiasMode.PerPrimitive; Coordinates=false}
    let coords b (cfg: Configuration) = {cfg with Coordinates = b}

    type DeviceManager = {  Devices: Devices;  //Godlike Object, unintended problems might lurk around the corner. don't have a more elegant solution yet.
                            Factories: Factories; 
                            Factory: Direct2D1.Factory1; 
                            Coordinates: bool
                            RenderTarget: RenderTarget;
                            loadImageFile: string -> Direct2D1.Bitmap1;
                            Context: Direct2D1.DeviceContext; 
                            createBitmap: int -> int -> Direct2D1.Bitmap1; 
                            FillBrush: Direct2D1.SolidColorBrush; 
                            FixedStrokeStyle: Direct2D1.StrokeStyle1}

    let deviceManager (cfg: Configuration) =
        let fac = new Direct2D1.Factory2(Direct2D1.FactoryType.SingleThreaded)
        let directWriteFac = new DirectWrite.Factory1()
        let dpi = fac.DesktopDpi
        let targetBMProperties = new Direct2D1.BitmapProperties1(new Direct2D1.PixelFormat(DXGI.Format.B8G8R8A8_UNorm, Direct2D1.AlphaMode.Premultiplied), dpi.Height, dpi.Width, Direct2D1.BitmapOptions.CannotDraw ||| Direct2D1.BitmapOptions.Target)
        let drawBMPoperties = new Direct2D1.BitmapProperties1(new Direct2D1.PixelFormat(DXGI.Format.B8G8R8A8_UNorm, Direct2D1.AlphaMode.Premultiplied), dpi.Height, dpi.Width, Direct2D1.BitmapOptions.Target)

        let d3device = match cfg.Debug with
                        |true   -> new Direct3D11.Device(Direct3D.DriverType.Hardware,  Direct3D11.DeviceCreationFlags.BgraSupport ||| Direct3D11.DeviceCreationFlags.Debug) //not sure if or flags are working correctly. 
                        |false  -> new Direct3D11.Device(Direct3D.DriverType.Hardware,  Direct3D11.DeviceCreationFlags.BgraSupport)

        let defDevice = d3device.QueryInterface<Direct3D11.Device1>()

        let dxgiDevice2 = defDevice.QueryInterface<DXGI.Device2>()

        let d2dDevice = new Direct2D1.Device1(fac, dxgiDevice2)
        let d2dContext = new Direct2D1.DeviceContext(d2dDevice, Direct2D1.DeviceContextOptions.None)

        let dxgiAdapter = dxgiDevice2.Adapter
        let dxgiFactory2 = dxgiAdapter.GetParent<DXGI.Factory2>()

        let imageFac = new WIC.ImagingFactory2()

        let mutable strokeProperties = new Direct2D1.StrokeStyleProperties1()
        do  strokeProperties.TransformType <- Direct2D1.StrokeTransformType.Fixed
        let mutable fixedStroke = new Direct2D1.StrokeStyle1(fac,strokeProperties)
        let originalTarget = d2dContext.Target
        let mutable solidColorBrush = new Direct2D1.SolidColorBrush(d2dContext,Color4.White)
        let mutable fillBrush = new Direct2D1.SolidColorBrush(d2dContext,Color4.White)

        let devices = {D2D1 = d2dDevice; D3D11 = defDevice; DXGI = dxgiDevice2}
        let factories = {D2D1 = fac; DXGI = dxgiFactory2; WIC = imageFac;DirectWrite = directWriteFac}

        let loadImageFile (name:string) = 
            let decoder = new WIC.BitmapDecoder(factories.WIC, name, WIC.DecodeOptions.CacheOnLoad)
            let converter = new WIC.FormatConverter(factories.WIC)
            converter.Initialize(decoder.GetFrame(0),WIC.PixelFormat.Format32bppPBGRA,WIC.BitmapDitherType.None, null, 0.0, WIC.BitmapPaletteType.Custom)
            Direct2D1.Bitmap1.FromWicBitmap(d2dContext, converter)

        match cfg.Target with //place to add other rendering paths.
        |Form(name,screen) -> 
            let form = new RenderForm()
            do  form.Size <- System.Windows.Forms.Screen.PrimaryScreen.Bounds.Size
            form.WindowState <- System.Windows.Forms.FormWindowState.Maximized
            form.FormBorderStyle <- System.Windows.Forms.FormBorderStyle.None
            let scDescription = ref (new DXGI.SwapChainDescription1(
                                            Width = 0,
                                            Height = 0,
                                            Format = DXGI.Format.B8G8R8A8_UNorm,
                                            Stereo = Bool(false),
                                            SampleDescription = new DXGI.SampleDescription(1,0),
                                            Usage = DXGI.Usage.RenderTargetOutput,
                                            BufferCount = 2,
                                            Scaling = DXGI.Scaling.None,
                                            SwapEffect = DXGI.SwapEffect.FlipSequential
                                            ))
            let swapChain = new DXGI.SwapChain1(factories.DXGI, devices.D3D11, form.Handle, scDescription, System.Nullable(), null)
            let bb = swapChain.GetBackBuffer<DXGI.Surface>(0) 
            let createBitmap w h = new Direct2D1.Bitmap1(d2dContext, Size2(w,h), drawBMPoperties)
            let curriedLoop  (f: unit -> unit) = 
                let present g () =
                    d2dContext.BeginDraw() 
                    g ()
                    d2dContext.EndDraw()
                    swapChain.Present(0,DXGI.PresentFlags.None)
                RenderLoop.Run(form, (present f))
            let target = new Direct2D1.Bitmap1(d2dContext,bb, targetBMProperties)
            let renderTarget = Hwnd{RenderForm=form; SwapChain=swapChain;Loop=curriedLoop}
            do d2dContext.Target <- target
            {Devices=devices;
            Coordinates=cfg.Coordinates;
            Factories=factories;
            Factory= factories.D2D1; 
            RenderTarget=renderTarget;
            loadImageFile=loadImageFile;
            Context=d2dContext;
            FillBrush=fillBrush;
            createBitmap=createBitmap;
            FixedStrokeStyle=fixedStroke}
