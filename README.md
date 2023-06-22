# Molecular Programming Language parser in F#
Project 2 for the course 02257 Applied Functional Programming

[![Build and test F# library](https://github.com/tjarker/molecular-programming/actions/workflows/fsharp-pipeline.yml/badge.svg)](https://github.com/tjarker/molecular-programming/actions/workflows/fsharp-pipeline.yml)

[Link to Overleaf](https://www.overleaf.com/project/6486eb5405c574a06c9b1a86)

## Project structure ##
The project is structures as a .NET class library called `MolecularProgrammingLib`.

The F# source files are included in the [`src`](MolecularProgrammingLib/src/) directory and organized into three subdirectories:
- [`domain`](MolecularProgrammingLib/src/domain/):
  - [`CrnTypes.fs`](MolecularProgrammingLib/src/domain/CrnTypes.fs): F# type declaration of the main CRN++ syntactic elements.
  - [`CrnParser.fs`](MolecularProgrammingLib/src/domain/CrnParser.fs): Parser for CRN++ programs into F# type representations.
  - [`CrnTypeChecker.fs`](MolecularProgrammingLib/src/domain/CrnTypeChecker.fs): Module used to test whether CRN satisfies *well-formed* properties.
  - [`CrnProperties.fs`](MolecularProgrammingLib/src/domain/CrnTypeChecker.fs): Module used to validate other properties on CRNs, such as comparing interpreted and compiled output reactions and to check the commutative property on step commands.
  - [`CrnGenerator.fs`](MolecularProgrammingLib/src/domain/CrnGenerator.fs): Custom generator for `FsCheck` that constructs *well-formed* CRNs.
  - [`CrnExamples.fs`](MolecularProgrammingLib/src/domain/CrnExamples.fs): Sample CRN++ programs from the paper [[1]](#1).
- [`script.fsx`](MolecularProgrammingLib/src/script.fsx): Demonstration of the implemented functionality.

## References ##
<a id="1">[1]</a> 
M. Vasic, D. Soloveichik, S. Khurshid (2020). 
*CRN++: Molecular Programming Language*
Springer Nature B.V., 2020, 391-407.