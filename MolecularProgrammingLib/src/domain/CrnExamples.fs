(*
    Date: XX.06.2023
    Author: 
*)
module CrnExamples

open CrnTypes

let counter =
    "crn={
        conc[c,3 ], 
        conc[ cInitial ,3 ],
        conc[one ,1], 
        conc[zero ,0],
        step[{
            sub[c,one,cnext ],
            cmp[c,zero]
        }],
        step[{
            ifGT[{ 
                ld[cnext ,c] 
            }],
            ifLE[{ 
                ld[ cInitial ,c] 
            }]
        }]
    };"

let piApprox =
    "crn={
        conc[ four , 4],
        conc[ divisor1 , 1],
        conc[ divisor2 , 3],
        conc[ pi , 0],
        step[{
            div[ four , divisor1 , factor1 ],
            add[ divisor1 , four , divisor1Next ],
            div[ four , divisor2 , factor2 ],
            add[ divisor2 , four , divisor2Next ],
            sub[ factor1 , factor2 , factor ],
            add[ pi , factor , piNext]  
        }],
        step[{
            ld[ divisor1Next , divisor1 ],
            ld[ divisor2Next , divisor2 ],
            ld[piNext, pi ]
        }]
    };"

let eulerApprox =
    "crn = {
        conc[e, 1], 
        conc[element, 1],
        conc[ divisor , 1], 
        conc[one, 1],
        conc[ divisorMultiplier , 1],
        step[{
            div[element, divisor , elementNext],
            add[ divisor , one, divisorNext ],
            add[e, elementNext, eNext]
        }],
        step[{
            ld[elementNext, element ],
            ld[ divisorNext , divisor ],
            ld[eNext, e]
        }]
    };"

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

let gcd =
    "crn = {
        conc[a, 9 ],
        conc[b, 15 ],
        step[{
            ld[a, atmp],
            ld[b, btmp],
            cmp[a,b]
        }],
        step[{
            ifGT[{ sub[atmp,btmp,a] }],
            ifLT[{ sub[btmp,atmp,b] }]
        }]
    };"

let examples = [ counter; piApprox; eulerApprox; integerSqrt; gcd ]

let crn1 =
    [ ([ Species "A"; Species "B" ], [ Species "A"; Species "B"; Species "C" ], 1.0)
      ([ Species "C" ], [], 1.0) ]

let crn2 = [ ([ Species "A"; Species "B" ], [], 1.0) ]

let crn3 =
    [ ([ Species "A" ], [ Species "A"; Species "C" ], 1.0)
      ([ Species "B" ], [ Species "B"; Species "C" ], 1.0)
      ([ Species "C" ], [], 1.0) ]

let crn4 =
    [ ([ Species "A" ], [ Species "C" ], 1.0)
      ([ Species "B" ], [ Species "C" ], 1.0) ]
