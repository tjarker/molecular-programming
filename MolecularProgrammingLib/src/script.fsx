#if INTERACTIVE
// A relative path to MolecularProgrammingLib.dll
#r @"../bin/Debug/net6.0/MolecularProgrammingLib.dll"
#r @"../bin/Debug/net6.0/DrawingTreesLib.dll"
#r "nuget: FParsec"
#r "nuget: FsCheck"
#r "nuget: Plotly.NET"
#endif


open CrnTypes
open CrnString
open CrnParser

printfn "Molecular Programming Library"

let integerSqrt =
    "crn = {
        conc[one ,1], 
        conc[n, 10 ],
        step[{
            add[z,one,znext ],
            mul[znext, znext ,zpow],
            cmp[zpow,n]
        }],
        step[{
            ifLT[{ ld[ znext , z ]}],
            ifGE[{ ld[z ,out ]}]
        }]
    };"

printfn "%s" (integerSqrt |> parse |> crnToString)
