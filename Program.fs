// Learn more about F# at http://fsharp.org

open System
open Aardvark.Base
open FSharp.Charting
open System.Windows.Forms

module DFT =
    let inline private sinc (v : float) =
        if Fun.IsTiny v then
            1.0
        else
            let p = Constant.Pi * v
            sin v / v

    let forwardN (n : int) (input : float[]) =
        let omega = Constant.PiTimesTwo / float input.Length

        Array.init n (fun k ->
            if k = 0 then
                ComplexD (Array.average input)
            else
                let mutable sum = ComplexD.Zero
                let dphi = omega
                let dk = float k * dphi
                let mutable phi = 0.0
                for i in 0 .. input.Length-1 do
                    let a = input.[i] * ComplexD(cos phi, -sin phi) * dphi
                    phi <- phi + dk
                    sum <- sum + a
                sum / Constant.Pi
        )

    let backwardN (sigma : float) (n : int) (input : ComplexD[]) =
        let omega = Constant.PiTimesTwo / float n
        Array.init n (fun k ->
            let phi = float k * omega
            let mutable sum = input.[0].Real
            for k in 1 .. input.Length-1 do
                let cn = input.[k]
                let angle = float k * phi
                let a = (cn.Real * cos angle - cn.Imag * sin angle)
                if sigma > 0.0 then sum <- sum + (sinc (float k / float input.Length) ** sigma) * a
                else sum <- sum + a
            sum
        )

    let interpolate (sigma : float) (input : ComplexD[]) (t : float)  =
        let omega = Constant.PiTimesTwo 
        let mutable sum = input.[0].Real
        let phi = t *omega
        for k in 1 .. input.Length-1 do
            let cn = input.[k]
            let angle = float k * phi
            let a = (cn.Real * cos angle - cn.Imag * sin angle)
            if sigma > 0.0 then sum <- sum + (sinc (float k / float input.Length) ** sigma) * a
            else sum <- sum + a

        sum


module Excel =
    let inputTemp =
        [|
            15.0601300
            15.0601300
            15.0601300
            15.0601300
            13.9301300
            13.9301300
            13.9301300
            13.9301300
            13.0201300
            13.0201300
            13.0201300
            13.0201300
            12.2801300
            12.2801300
            12.2801300
            12.2801300
            11.7201300
            11.7201300
            11.7201300
            11.7201300
            11.3701300
            11.3701300
            11.3701300
            11.3701300
            11.4101300
            11.4101300
            11.4101300
            11.4101300
            12.8201300
            12.8201300
            12.8201300
            12.8201300
            15.1601300
            15.1601300
            15.1601300
            15.1601300
            17.6901300
            17.6901300
            17.6901300
            17.6901300
            19.9801300
            19.9801300
            19.9801300
            19.9801300
            21.8301300
            21.8301300
            21.8301300
            21.8301300
            23.2301300
            23.2301300
            23.2301300
            23.2301300
            23.2201300
            23.2201300
            23.2201300
            23.2201300
            24.8501300
            24.8501300
            24.8501300
            24.8501300
            25.1501300
            25.1501300
            25.1501300
            25.1501300
            25.2901300
            25.2901300
            25.2901300
            25.2901300
            25.0501300
            25.0501300
            25.0501300
            25.0501300
            24.3501300
            24.3501300
            24.3501300
            24.3501300
            23.1701300
            23.1701300
            23.1701300
            23.1701300
            21.6101300
            21.6101300
            21.6101300
            21.6101300
            19.8401300
            19.8401300
            19.8401300
            19.8401300
            18.0601300
            18.0601300
            18.0601300
            18.0601300
            16.4401300
            16.4401300
            16.4401300
            16.4401300
        |]

    let rectangle =
        Array.init 96 (fun i ->
            if i >= 45 && i <= 75 then 25.0
            else 15.0
        )


    let sampled =
        [|
            14.9117909842
            15.2025210963
            14.9126781570
            14.0804649507
            13.7899386451
            14.0500084299
            13.8268935775
            13.1210790924
            12.9106868602
            13.1344197131
            12.9153345179
            12.3650934630
            12.2105772222
            12.3491889706
            12.2013500420
            11.8032960439
            11.6488353516
            11.7681532795
            11.6905899432
            11.3976105651
            11.3330767107
            11.4106970243
            11.3474690378
            11.3926914829
            11.4776676677
            11.2953529712
            11.5656798292
            12.6261944135
            13.0525165454
            12.5539043408
            13.1084064252
            14.8622904823
            15.4629888016
            14.8484463112
            15.4832886875
            17.3632568469
            18.0049840494
            17.3975683750
            17.9660462299
            19.7049395087
            20.2621639065
            19.7047614775
            20.2235639832
            21.6297136131
            22.0076723620
            21.6378802328
            22.0537279253
            23.0083758605
            23.3803969426
            23.2034486889
            23.1522461523
            23.3076650149
            23.2372309253
            23.0437587711
            23.5103488992
            24.5603014973
            25.0358401535
            24.7955685157
            24.8278488418
            25.1675746821
            25.1824693836
            25.0833226270
            25.2042067567
            25.2802036167
            25.2637691386
            25.3219447875
            25.2723223572
            25.0636641978
            25.0148661015
            25.1222746433
            24.9501423128
            24.4566412831
            24.2478886049
            24.4578722589
            24.2182360778
            23.3336825038
            22.9861800103
            23.3561294304
            22.9894755297
            21.7947866929
            21.4070701311
            21.8346354364
            21.3766676184
            20.0663813402
            19.6257672624
            20.0522671290
            19.6181972401
            18.2921944328
            17.8308371786
            18.2728111847
            17.8656038345
            16.6277584088
            16.2479050610
            16.6361843047
            16.2527960238
            15.2272600064
        |]

    let fourier17 =
        [|
            ComplexD(18.6055466666667000000, 0.0)
            ComplexD(-3.25530676628306000000, 6.17269004142937000000)
            ComplexD(0.50765087560301400000 , -0.06866771448729060000 )
            ComplexD(-0.27325970768843900000, -0.48177979437139100000 )
            ComplexD(-0.08382478936841090000, 0.18134448418915000000)
            ComplexD(0.04812349065157450000 , -0.03252371238696870000 )
            ComplexD(0.04986527184587980000 , 0.02026244274483080000)
            ComplexD(-0.07675017219606480000, -0.04919385569306010000 )
            ComplexD(0.06098770398786340000 , 0.03634160603177580000)
            ComplexD(-0.05532631071580340000, 0.00644827318562506000)
            ComplexD(0.06069218408491550000 , -0.03385650983234720000 )
            ComplexD(-0.04832957683031250000, 0.04172000907454770000)
            ComplexD(0.01979166666667260000 , -0.04778131008863090000 )
            ComplexD(0.00398902956884273000 , 0.05434704266754070000)
            ComplexD(-0.01379084497440660000, -0.04858854553351210000 )
            ComplexD(0.02128316945761660000 , 0.02689876688557160000)
            ComplexD(-0.03562499999999470000, -0.00902109795610145000 )
        |]

[<Struct>]
type Spectral<'d when 'd :> INatural> =
    val mutable public Coefficients : ComplexD[]

    static member FromCoefficients(coefficients : ComplexD[]) : Spectral<'d> =
        if coefficients.Length <> Peano.typeSize<'d> then failwithf "[Spectral] invalid coefficients: %d" coefficients.Length
        Spectral<'d>(Coefficients = coefficients)

    static member Zero =
        Spectral<'d>.FromCoefficients(Array.zeroCreate Peano.typeSize<'d>)

    static member (+) (l : Spectral<'d>, r : Spectral<'d>) =
        (l.Coefficients, r.Coefficients)
        ||> Array.map2 (+)
        |> Spectral<'d>.FromCoefficients
        
    static member (-) (l : Spectral<'d>, r : Spectral<'d>) =
        (l.Coefficients, r.Coefficients)
        ||> Array.map2 (-)
        |> Spectral<'d>.FromCoefficients

    member x.Sample(value : float, ?sigma : float) =
        let sigma = defaultArg sigma 0.0
        DFT.interpolate sigma x.Coefficients value
        
    member x.GetValues(cnt : int, ?sigma : float) =
        let sigma = defaultArg sigma 0.0
        DFT.backwardN sigma cnt x.Coefficients
        
    new(data : float[]) =
        { Coefficients = DFT.forwardN Peano.typeSize<'d> data }


type Size = N<32>

let testFourier () =
    let NData = 1.0

    let input = Excel.rectangle

    let dft = Spectral<Size> input
    let inv = dft.GetValues input.Length

    let data (data : float[]) =
        data |> Array.mapi (fun i v ->
            let tc = float i / float data.Length
            let x = NData * tc
            x, v
        )

    let spectral (sigma : float) (dft : Spectral<_>) =
        let samples = 1024
        Array.init samples (fun i ->
            let tc = float i / float samples
            let t = tc
            tc * NData, dft.Sample(t, sigma)
        )

        //Chart.Line(sampled , Name = sprintf "%s" name)
        //|> Chart.WithStyling(BorderWidth = w)
   
    let interval (f : float) =
        10.0 ** (round (log10 (f / 24.0)))
    
    let chart = 
        Chart.Combine [
            //Chart.Line(spectral (Spectral<17 N>.FromCoefficients Excel.fourier17), Name = "mysyn(excel)")
            //|> Chart.WithStyling(BorderWidth = 7)
            
            Chart.Line(spectral 0.0 dft, Name = "mysyn")
            |> Chart.WithStyling(BorderWidth = 8)
            
            Chart.Line(spectral 2.0 dft, Name = "mysyn(2)")
            |> Chart.WithStyling(BorderWidth = 6)

            Chart.Line(spectral 4.0 dft, Name = "mysyn(4)")
            |> Chart.WithStyling(BorderWidth = 4)
            
            Chart.Line(spectral 8.0 dft, Name = "mysyn(8)")
            |> Chart.WithStyling(BorderWidth = 2)

            //Chart.Line(data Excel.sampled, Name = "excelsyn")
            Chart.Point(data inv, Name = "idft(dft)", MarkerSize = 7)

            Chart.Point(data input, Name = "input", MarkerSize = 4, Color = Drawing.Color.Black)

        ]
        |> Chart.WithLegend(true)
        |> Chart.WithXAxis(
            Min = 0.0, 
            Max = NData, 
            MajorTickMark = ChartTypes.TickMark(Interval = interval NData)
        )
   
    let f = chart.ShowChart()
    Application.Run f









[<EntryPoint; STAThread>]
let main argv =
    testFourier()
    0 
