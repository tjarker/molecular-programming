module CrnParser

open FParsec
open CrnTypes

val parse: string -> CRN
