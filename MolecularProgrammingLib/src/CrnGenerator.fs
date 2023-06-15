module CrnGenerator

open FsCheck
open CrnTypes


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

let pairGen env (g0, g1) constr =
    gen {
        let! a = g0 env
        let! b = g1 env
        return constr (a, b)
    }

let tripleGen env (g0, g1, g2) constr =
    gen {
        let! a = g0 env
        let! b = g1 env
        let! c = g2 env
        return constr (a, b, c)
    }

let listGen env g constr =
    gen {
        let! xs = Gen.nonEmptyListOf (g env)
        return constr (xs)
    }

let moduleGen env =
    let twoArgs = List.map (pairGen env (speciesGen, speciesGen)) [ Ld; Sqrt; Cmp ]

    let threeArgs =
        List.map (tripleGen env (speciesGen, speciesGen, speciesGen)) [ Add; Sub; Mul; Div ]

    Gen.oneof (twoArgs @ threeArgs)

let computationGen env = Gen.map Mod (moduleGen env)

let conditionalGen env =
    let conGens = List.map (listGen env computationGen) [ IfGT; IfGE; IfEQ; IfLT; IfLE ]
    Gen.oneof conGens

let commandGen env =
    Gen.oneof [ Gen.map Comp (computationGen env); Gen.map Cond (conditionalGen env) ]

let concGen =
    Gen.map2 (fun name value -> Conc(Species name, value)) stringGen (Gen.choose (0, 1000) |> Gen.map float)

let stepGen env =
    gen {
        let! len = Gen.choose (1, 20)
        let! cmds = Gen.listOfLength len (commandGen env)
        return Step(cmds)
    }

let crnGen =
    gen {
        let! concs = Gen.nonEmptyListOf concGen

        let species =
            List.map
                (function
                | Conc(sp, value) -> sp
                | _ -> failwith "expected only concentrations")
                concs

        let! steps = Gen.nonEmptyListOf (stepGen species)
        return CRN(concs @ steps)
    }

type CrnGenerator =

    static member CRN() =
        { new Arbitrary<CRN>() with
            override x.Generator = crnGen
            override x.Shrinker t = Seq.empty }

    static member initialize() = Arb.register<CrnGenerator> ()
