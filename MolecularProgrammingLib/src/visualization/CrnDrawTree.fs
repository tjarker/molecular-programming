(*
    Date: XX.06.2023
    Author: 
*)
module CrnDrawTree

open CrnTypes
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
