/////////////////////////////////////////////////////////////////////////////////////////////////////////////////
(* Model for CRN++ grammar *)

type Species = Species of string

type Module =
    | Ld of Species * Species
    | Add of Species * Species * Species
    | Sub of Species * Species * Species
    | Mul of Species * Species * Species
    | Div of Species * Species * Species
    | Sqrt of Species * Species
    | Cmp of Species * Species

type Computation =
    | Mod of Module
    | Rxn of Species list * Species list * int

type Conditional =
    | IfGT of Computation list
    | IfGE of Computation list
    | IfEQ of Computation list
    | IfLT of Computation list
    | IfLE of Computation list

type Command =
    | Comp of Computation
    | Cond of Conditional

type Root =
    | Conc of Species * int
    | Step of Command list

type CRN = CRN of Root list
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////




















/////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/// general parser builders

(* Parser for CRN++ *)
#r "nuget: FParsec"
open FParsec

let token p = p .>> spaces


let symbol s = token (pstring s)

let pInteger: Parser<int32, unit> = token pint32

let pIdentifier: Parser<string, unit> =
    let charOrDigit c = isLetter c || isDigit c
    token (many1Satisfy2L isLetter charOrDigit "identifier")

let pPair l d r (p0, p1) constr =
    parse {
        let! _ = symbol l
        let! t0 = p0
        let! _ = symbol d
        let! t1 = p1
        let! _ = symbol r
        return constr (t0, t1)
    }

let pTriple l d r (p0, p1, p2) constr =
    parse {
        let! _ = symbol l
        let! t0 = p0
        let! _ = symbol d
        let! t1 = p1
        let! _ = symbol d
        let! t2 = p2
        let! _ = symbol r
        return constr (t0, t1, t2)
    }


let rec pListOpt d parser xs =
    parse {
        let! _ = symbol d
        let! x = parser
        return! pListOpt d parser (xs @ [ x ])
    }
    <|> preturn xs

let pList delim parser =
    parse {
        let! elem = parser
        return! (pListOpt delim (token parser) [ elem ])
    }

let pListBlock l d r parser constr =
    parse {
        let! _ = symbol l
        let! xs = pList d parser
        let! _ = symbol r
        return constr (xs)
    }

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////






















///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/// CRN parsers
let pSpecies =
    parse {
        let! x = pIdentifier
        return Species x
    }

let pModule =
    pPair "ld[" "," "]" (pSpecies, pSpecies) Ld
    <|> pPair "sqrt[" "," "]" (pSpecies, pSpecies) Sqrt
    <|> pPair "cmp[" "," "]" (pSpecies, pSpecies) Cmp
    <|> pTriple "add[" "," "]" (pSpecies, pSpecies, pSpecies) Add
    <|> pTriple "sub[" "," "]" (pSpecies, pSpecies, pSpecies) Sub
    <|> pTriple "mul[" "," "]" (pSpecies, pSpecies, pSpecies) Mul
    <|> pTriple "div[" "," "]" (pSpecies, pSpecies, pSpecies) Div

let pRxn =
    pTriple "rxn[" "," "]" (pList "+" pSpecies, pList "+" pSpecies, pInteger) Rxn

let pComputation =
    pRxn
    <|> parse {
        let! m = pModule
        return (Mod m)
    }

let pConditional =
    pListBlock "ifGT[{" "," "}]" pComputation IfGT
    <|> pListBlock "ifGE[{" "," "}]" pComputation IfGE
    <|> pListBlock "ifEQ[{" "," "}]" pComputation IfEQ
    <|> pListBlock "ifLT[{" "," "}]" pComputation IfLT
    <|> pListBlock "ifLE[{" "," "}]" pComputation IfLE


let pCommand =
    parse {
        let! cmd = pComputation
        return Comp cmd
    }
    <|> parse {
        let! con = pConditional
        return Cond con
    }

let pStep = pListBlock "step[{" "," "}]" pCommand Step

let pConc = pPair "conc[" "," "]" (pSpecies, pInteger) Conc

let pRoot = pStep <|> pConc

let pCRN = 
    parse {
        let! _ = symbol "crn" 
        let! _ = symbol "="
        let! crn = pListBlock "{" "," "};" pRoot CRN
        return crn
    }

let resToCRN (x: ParserResult<CRN, unit>) =
    match x with
    | Success(crn, _, _) -> crn
    | Failure(errorMsg, _, _) -> failwith errorMsg

let parse str = run (pCRN .>> eof) str |> resToCRN
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////





















///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/// Drawing trees converter

#r @"DrawingTreesLib.dll"

open DesignTrees
open DrawTrees

let speciesToTree (Species id) = Node(id, [])

let moduleToTree =
    function
    | Ld(s1, s2) -> Node("ld", [ speciesToTree s1; speciesToTree s2 ])
    | Add(s1, s2, s3) -> Node("add", [ speciesToTree s1; speciesToTree s2; speciesToTree s3 ])
    | Sub(s1, s2, s3) -> Node("sub", [ speciesToTree s1; speciesToTree s2; speciesToTree s3 ])
    | Mul(s1, s2, s3) -> Node("mul", [ speciesToTree s1; speciesToTree s2; speciesToTree s3 ])
    | Div(s1, s2, s3) -> Node("div", [ speciesToTree s1; speciesToTree s2; speciesToTree s3 ])
    | Sqrt(s1, s2) -> Node("sqrt", [ speciesToTree s1; speciesToTree s2 ])
    | Cmp(s1, s2) -> Node("cmp", [ speciesToTree s1; speciesToTree s2 ])

let exprToTree =
    function
    | [] -> failwith "Expression cannot be empty"
    | Species s0 :: es -> List.fold (fun rest (Species s) -> Node("+", [ Node(s, []); rest ])) (Node(s0, [])) es

let ComputationcommandToTree =
    function
    | Mod(modu) -> moduleToTree modu
    | Rxn(e1, e2, n) -> Node($"rxn: {n}", [ (exprToTree e1); exprToTree e2 ])

let conditionalCommandToTree =
    function
    | IfGT(cmds) -> Node("ifGT", List.map ComputationcommandToTree cmds)
    | IfGE(cmds) -> Node("ifGE", List.map ComputationcommandToTree cmds)
    | IfEQ(cmds) -> Node("ifEQ", List.map ComputationcommandToTree cmds)
    | IfLT(cmds) -> Node("ifLT", List.map ComputationcommandToTree cmds)
    | IfLE(cmds) -> Node("ifLE", List.map ComputationcommandToTree cmds)

let CommandsToTree =
    function
    | Comp(cmd) -> ComputationcommandToTree cmd
    | Cond(con) -> conditionalCommandToTree con

let rootToTree =
    function
    | Conc(sp, n) -> Node("conc", [ speciesToTree sp; Node(n.ToString(), []) ])
    | Step(cmds) -> Node("step", List.map CommandsToTree cmds)

let crnToTree (CRN(rootList)) =
    Node("CRN", List.map rootToTree rootList)

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////










let counter =
    "crn={
        conc[c,10 ], 
        conc[ cInitial ,20 ],
        conc[one ,1], 
        conc[zero ,0],
        step[{
            sub[c,one,cnext ],
            cmp[c,zero],
            rxn[a+b+c+d,a+b+e+f,2]
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
        conc[n, 31 ],
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
        conc[a, 243 ],
        conc[b, 9350 ],
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

printfn "Counter:\n%O" (parse counter)
printfn "Pi Approximation:\n%O" (parse piApprox)
printfn "Eulers Number Approximation:\n%O" (parse eulerApprox)
printfn "Integer Sqrt:\n%O" (parse integerSqrt)
printfn "GCD:\n%O" (parse gcd)

//drawTree (counter |> parse |> resToCRN |> crnToTree) 8 "counter.svg" None


let moduleToString = function
    | Ld(Species sp1,Species sp2) -> $"ld[{sp1},{sp2}]"
    | Add(Species sp1,Species sp2,Species sp3) -> $"add[{sp1},{sp2},{sp3}]"
    | Sub(Species sp1,Species sp2,Species sp3) -> $"sub[{sp1},{sp2},{sp3}]"
    | Mul(Species sp1,Species sp2,Species sp3) -> $"mul[{sp1},{sp2},{sp3}]"
    | Div(Species sp1,Species sp2,Species sp3) -> $"div[{sp1},{sp2},{sp3}]"
    | Sqrt(Species sp1,Species sp2) -> $"sqrt[{sp1},{sp2}]"
    | Cmp(Species sp1,Species sp2) -> $"cmp[{sp1},{sp2}]"

let speciesListToString xs = xs |> List.map (fun (Species name) -> name ) |> String.concat "+ " 

let computationToString = function
    | Mod(modu) -> moduleToString modu
    | Rxn(sps1,sps2,n) ->
        let lhs = speciesListToString sps1
        let rhs = speciesListToString sps2
        $"rxn[{lhs},{rhs},{n}]"

let conditionalToString = function 
    | IfGT xs -> "ifGT[{" + (List.map computationToString xs |> String.concat ", ") + "}]"
    | IfGE xs -> "ifGE[{" + (List.map computationToString xs |> String.concat ", ") + "}]"
    | IfEQ xs -> "ifEQ[{" + (List.map computationToString xs |> String.concat ", ") + "}]"
    | IfLT xs -> "ifLT[{" + (List.map computationToString xs |> String.concat ", ") + "}]"
    | IfLE xs -> "ifLE[{" + (List.map computationToString xs |> String.concat ", ") + "}]"
    
let commandToString = function 
    | Comp comp -> computationToString comp
    | Cond cond -> conditionalToString cond

let rootToString = function
    | Conc(Species sp, n) -> $"conc[{sp}, {n}]"
    | Step cs -> 
        let str = cs |> List.map commandToString |> String.concat ", "
        $"step[{{{str}}}]"

let crnToString (CRN roots) = 
    let rs = roots |> List.map rootToString |> String.concat ", "
    $"crn = {{{rs}}};"

let removeWhiteSpace str = str |> String.collect (fun c -> if c = ' ' or c = '\n' or c = '\t' or c = '\r' then "" else string c)


//////////////////////////////////////////////////////////////////////////////////////////////

#r "nuget: FsCheck"

open FsCheck



let charsSeqGen c1 c2 =
    seq {
        for c in c1..c2 do
            yield gen { return c }
    }

let charGen =
    Gen.frequency
        [ (1, gen { return! Gen.oneof (charsSeqGen 'a' 'z') })
          (1, gen { return! Gen.oneof (charsSeqGen 'A' 'Z') }) ]

let stringGen =
    gen {
        let! i = Gen.choose (1, 4)
        let! cs = Gen.listOfLength i charGen
        let ss = List.map string cs
        return String.concat "" ss
    }



let speciesGen env = 
    gen {
        let! i = Gen.choose (0, List.length env - 1)
        return env.[i]
    }

let pairGen env (g0,g1) constr = 
    gen {
        let! a = g0 env
        let! b = g1 env
        return constr(a,b)
    }

let tripleGen env (g0,g1,g2) constr = 
    gen {
        let! a = g0 env
        let! b = g1 env
        let! c = g2 env
        return constr(a,b,c)
    }

let listGen env g constr =
    gen {
        let! xs = Gen.nonEmptyListOf (g env)
        return constr(xs)
    }

let moduleGen env = 
    let twoArgs = List.map (pairGen env (speciesGen,speciesGen)) [Ld;Sqrt;Cmp]
    let threeArgs = List.map (tripleGen env (speciesGen,speciesGen,speciesGen)) [Add;Sub;Mul;Div]
    Gen.oneof (twoArgs @ threeArgs)

let computationGen env = 
    Gen.map Mod (moduleGen env)

let conditionalGen env = 
    let conGens = List.map (listGen env computationGen) [IfGT;IfGE;IfEQ;IfLT;IfLE]
    Gen.oneof conGens 

let commandGen env = 
    Gen.oneof [
        Gen.map Comp (computationGen env);
        Gen.map Cond (conditionalGen env)
    ]

let concGen =
    Gen.map2 (fun name value -> Conc(Species name,value)) stringGen (Gen.choose(0,1000))

let stepGen env =
    gen {
        let! len = Gen.choose(1,20)
        let! cmds = Gen.listOfLength len (commandGen env)
        return Step(cmds)
    }

let crnGen = 
    gen {
        let! concs = Gen.nonEmptyListOf concGen
        let species = List.map (function
            | Conc(sp, value) -> sp
            | _ -> failwith "expected only concentrations") concs
        let! steps =  Gen.nonEmptyListOf (stepGen species)
        return CRN(concs @ steps)
    }

type CrnGenerator =

    static member CRN() =
        { new Arbitrary<CRN>() with
            override x.Generator = crnGen
            override x.Shrinker t = Seq.empty }

Arb.register<CrnGenerator>()

let prog = Gen.sample 3 1 crnGen |> List.head
printfn "%O" prog
drawTree (prog |> crnToTree) 6 "prog.svg" None



let prop prog = 
    let str = prog |> crnToString
    str = (str |> parse |> crnToString)

Check.Quick prop