﻿// Learn more about F# at http://fsharp.org

open System
open Aardvark.Base
open FSharp.Charting
open System.Windows.Forms

module DFT =
    let forwardN (n : int) (input : ComplexD[]) =
        let omega = Constant.PiTimesTwo / float input.Length

        Array.init n (fun k ->
            if k = 0 then
                Array.average input
            else
                let mutable sum = ComplexD.Zero
                let dphi = omega
                let dk = float k * dphi
                let mutable phi = 0.0
                for i in 0 .. input.Length-1 do
                    let input = if i >= input.Length then input.[0] else input.[i]
                    let a = input * ComplexD(cos phi, -sin phi) * dphi
                    phi <- phi + dk
                    sum <- sum + a
                sum / Constant.Pi
        )

    let backwardN (n : int) (input : ComplexD[]) =
        let omega = Constant.PiTimesTwo / float n
        Array.init n (fun k ->
            let phi = float k * omega
            let mutable sum = input.[0]
            for n in 1 .. input.Length-1 do
                let cn = input.[n]
                let angle = float n * phi
                let ca = cos angle
                let sa = sin angle
                let a = 
                    0.5 * (
                        cn * ComplexD(ca, sa) +
                        cn.Conjugated * ComplexD(ca, -sa)
                    )

                sum <- sum + a
            sum
        )

    let interpolateN (n : int) (input : ComplexD[]) (t : float)  =
        let omega = Constant.PiTimesTwo / float n
        let mutable sum = input.[0]
        let phi = t * float n * omega
        for n in 1 .. input.Length-1 do
            let cn = input.[n]
            let angle = float n * phi
            let ca = cos angle
            let sa = sin angle
            let a = 
                0.5 * (
                    cn * ComplexD(ca, sa) +
                    cn.Conjugated * ComplexD(ca, -sa)
                )

            sum <- sum + a
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

let testFourier () =
    let N = 17
    let NData = Excel.inputTemp.Length

    let dft = DFT.forwardN N (Array.map ComplexD Excel.inputTemp) 
    let inv = DFT.backwardN NData dft


    let samples = 1024

    let invChart (w : int) (name : string) (dft : ComplexD[]) =
        let sampled =
            Array.init samples (fun i ->
                let tc = ((float i) / float samples)
                let t = tc
                tc * float NData, DFT.interpolateN NData dft t
            )

        Chart.Line(sampled |> Seq.map (fun (x, c) -> x, c.Real), Name = sprintf "%s" name)
        |> Chart.WithStyling(BorderWidth = w)
   
    
    let chart = 
        let excelData =
            Excel.inputTemp |> Seq.mapi (fun i c -> 
                let tc = (float i + 0.5) / float Excel.inputTemp.Length
                let x = tc * float NData
                x, c
            ) 
        Chart.Combine [
            invChart 7 "mysyn(excel)" Excel.fourier17
            invChart 3 "mysyn" dft

            Chart.Line(Excel.sampled, Name = "excelsyn")
            Chart.Point(inv |> Seq.mapi (fun i c -> float i, c.Real), Name = "idft(dft)", MarkerSize = 7)
        ]
        |> Chart.WithLegend(true)
        |> Chart.WithXAxis(
            Min = 0.0, 
            Max = float NData, 
            MajorTickMark = ChartTypes.TickMark(Interval = ceil (float NData / 24.0))
        )
   
    let f = chart.ShowChart()
    Application.Run f









[<EntryPoint; STAThread>]
let main argv =
    testFourier()
    0 