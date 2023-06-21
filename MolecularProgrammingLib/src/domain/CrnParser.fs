(*
    Date: XX.06.2023
    Author: 
*)
module CrnParser

open CrnTypes
open FParsec

let token p = p .>> spaces

let symbol s = token (pstring s)

let pInteger: Parser<int32, unit> = token pint32

let pFloat: Parser<float, unit> = token pfloat

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
    } <|> preturn []

let pListBlock l d r parser constr =
    parse {
        let! _ = symbol l
        let! xs = pList d parser
        let! _ = symbol r
        return constr (xs)
    }

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
    pTriple "rxn[" "," "]" (pList "+" pSpecies, pList "+" pSpecies, pFloat) Rxn

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

let pConc = pPair "conc[" "," "]" (pSpecies, pFloat) Conc

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
