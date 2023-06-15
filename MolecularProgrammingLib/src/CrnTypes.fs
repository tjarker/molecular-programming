module CrnTypes

type Species = Species of string

type Module =
    | Ld of Species * Species
    | Add of Species * Species * Species
    | Sub of Species * Species * Species
    | Mul of Species * Species * Species
    | Div of Species * Species * Species
    | Sqrt of Species * Species
    | Cmp of Species * Species

type Reaction = Species list * Species list * float

type Computation =
    | Mod of Module
    | Rxn of Reaction

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
    | Conc of Species * float
    | Step of Command list

type CRN = CRN of Root list
