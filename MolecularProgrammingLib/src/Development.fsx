(* Model for CRN++ grammar *)

type Species = Species of string

type Expr = Species list

type Module =
    | Ld of Species * Species
    | Add of Species * Species * Species
    | Sub of Species * Species * Species
    | Mul of Species * Species * Species
    | Div of Species * Species * Species
    | Sqrt of Species * Species
    | Cmp of Species * Species

type Command =
    | Mod of Module
    | Rxn of Expr * Expr * int

type Conditional =
    | IfGT of Command list
    | IfGE of Command list
    | IfEQ of Command list
    | IfLT of Command list
    | IfLE of Command list

type AllCommand =
    | Cmd of Command
    | Cond of Conditional

type Root =
    | Conc of Species * int
    | Step of AllCommand list

type CRN = CRN of Root list


(* Parser for CRN++ *)
#r "nuget: FParsec"
open FParsec

let token p = p .>> spaces

let symbol s = token (pstring s)

let pinteger: Parser<int32, unit> = token pint32

let ident: Parser<string, unit> =
    let charOrDigit c = isLetter c || isDigit c
    token (many1Satisfy2L isLetter charOrDigit "identifier")

let pSpecies =
    parse {
        let! x = ident
        return Species x
    }

let pModule2 name (constr: (Species * Species) -> Module) =
    parse {
        let! _ = symbol (name + "[")
        let! sp1 = pSpecies
        let! _ = symbol ","
        let! sp2 = pSpecies
        let! _ = symbol "]"
        return constr (sp1, sp2)
    }

let pModule3 name (constr: (Species * Species * Species) -> Module) =
    parse {
        let! _ = symbol (name + "[")
        let! sp1 = pSpecies
        let! _ = symbol ","
        let! sp2 = pSpecies
        let! _ = symbol ","
        let! sp3 = pSpecies
        let! _ = symbol "]"
        return constr (sp1, sp2, sp3)
    }

let pModule =
    pModule2 "ld" Ld
    <|> pModule3 "add" Add
    <|> pModule3 "sub" Sub
    <|> pModule3 "mul" Mul
    <|> pModule3 "div" Div
    <|> pModule2 "sqrt" Sqrt
    <|> pModule2 "cmp" Cmp


let rec pExprOpt sp =
    parse {
        let! _ = symbol "+"
        let! sp2 = pSpecies
        return! pExprOpt (sp @ [ sp2 ])
    }
    <|> preturn sp

let rec pListOpt delim parser elems =
    parse {
        let! _ = symbol delim
        let! elem = parser
        return! pListOpt delim parser (elems @ [ elem ])
    }
    <|> preturn elems

let pList delim parser =
    parse {
        let! elem = parser
        return! (pListOpt delim (token parser) [ elem ])
    }

let pExpr = pList "+" pSpecies

let pRxn =
    parse {
        let! _ = symbol "rxn["
        let! expr1 = pExpr
        let! _ = symbol ","
        let! expr2 = pExpr
        let! _ = symbol ","
        let! n = pinteger
        let! _ = symbol "]"
        return Rxn(expr1, expr2, n)
    }

let pCommand =
    pRxn
    <|> parse {
        let! m = pModule
        return (Mod m)
    }



let pConditionalHelp name constr =
    parse {
        let! _ = symbol (name + "[{")
        let! cmds = pList "," pCommand
        let! _ = symbol "}]"
        return (constr cmds)
    }

let pConditional =
    pConditionalHelp "ifGT" IfGT
    <|> pConditionalHelp "ifGE" IfGE
    <|> pConditionalHelp "ifEQ" IfEQ
    <|> pConditionalHelp "ifLT" IfLT
    <|> pConditionalHelp "ifLE" IfLE


let pAllCommands =
    parse {
        let! cmd = pCommand
        return Cmd cmd
    }
    <|> parse {
        let! co = pConditional
        return Cond co
    }

let pStep =
    parse {
        let! _ = symbol "step[{"
        let! cmds = pList "," pAllCommands
        let! _ = symbol "}]"
        return Step cmds
    }

let pConc =
    parse {
        let! _ = symbol "conc["
        let! sp = pSpecies
        let! _ = symbol ","
        let! init = pinteger
        let! _ = symbol "]"
        return Conc(sp, init)
    }

let pRoot = pStep <|> pConc


let pCRN =
    parse {
        let! _ = symbol "crn={"
        let! roots = pList "," pRoot
        let! _ = symbol "}"
        return CRN roots
    }

let parse str = run (pCRN .>> eof) str


let counter =
    "crn={conc[c,10 ], conc[ cInitial ,20 ],conc[one ,1], conc[zero ,0],step[{sub[c,one,cnext ],cmp[c,zero],rxn[a+b+c+d,a+b+e+f,2]}],step[{
ifGT[{ ld[cnext ,c] }],ifLE[{ ld[ cInitial ,c] }]}]}"

parse counter

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

let rec exprToTree =
    function
    | [ Species x ] -> Node(x, [])
    | Species(s) :: xs -> Node(s, [ exprToTree xs ])
    | _ -> failwith "Invalid expression"

let commandToTree =
    function
    | Mod(modu) -> moduleToTree modu
    | Rxn(e1: Expr, e2: Expr, n) -> Node($"rxn: {n}", [ (exprToTree e1); exprToTree e2 ])

let conditionalToTree =
    function
    | IfGT(cmds) -> Node("ifGT", List.map commandToTree cmds)
    | IfGE(cmds) -> Node("ifGE", List.map commandToTree cmds)
    | IfEQ(cmds) -> Node("ifEQ", List.map commandToTree cmds)
    | IfLT(cmds) -> Node("ifLT", List.map commandToTree cmds)
    | IfLE(cmds) -> Node("ifLE", List.map commandToTree cmds)

let allCommandsToTree =
    function
    | Cmd(cmd) -> commandToTree cmd
    | Cond(con) -> conditionalToTree con

let rootToTree =
    function
    | Conc(sp, n) -> Node("conc", [ speciesToTree sp; Node(n.ToString(), []) ])
    | Step(cmds) -> Node("step", List.map allCommandsToTree cmds)

let crnToTree (CRN(rootList)) =
    Node("CRN", List.map rootToTree rootList)

let resToCRN (x: ParserResult<CRN, unit>) =
    match x with
    | Success(crn, _, _) -> crn
    | Failure(errorMsg, _, _) -> failwith errorMsg


drawTree (counter |> parse |> resToCRN |> crnToTree) 8 "counter.svg" None
