# Molecular Programming Language parser in F#
Project 2 for the course 02257 Applied Functional Programming

[![Build and test F# library](https://github.com/tjarker/molecular-programming/actions/workflows/fsharp-pipeline.yml/badge.svg)](https://github.com/tjarker/molecular-programming/actions/workflows/fsharp-pipeline.yml)

[Link to Overleaf](https://www.overleaf.com/project/6486eb5405c574a06c9b1a86)

## Project structure ##
The project is structures as a .NET class library called `MolecularProgrammingLib`.

The F# source files are included in the [`src`](MolecularProgrammingLib/src/) directory and organized into three subdirectories and a standalone script:
- [`domain`](MolecularProgrammingLib/src/domain/)
  - [`CrnTypes.fs`](MolecularProgrammingLib/src/domain/CrnTypes.fs): F# type declaration of the main CRN++ syntactic elements.
  - [`CrnParser.fs`](MolecularProgrammingLib/src/domain/CrnParser.fs): Parser for CRN++ programs into F# type representations.
  - [`CrnTypeChecker.fs`](MolecularProgrammingLib/src/domain/CrnTypeChecker.fs): Module used to test whether CRN satisfies *well-formed* properties.
  - [`CrnProperties.fs`](MolecularProgrammingLib/src/domain/CrnTypeChecker.fs): Module used to validate other properties on CRNs, such as comparing interpreted and compiled output reactions and to check the dependency ordering of step commands.
  - [`CrnGenerator.fs`](MolecularProgrammingLib/src/domain/CrnGenerator.fs): Custom generator for `FsCheck` that constructs *well-formed* CRNs.
  - [`CrnExamples.fs`](MolecularProgrammingLib/src/domain/CrnExamples.fs): Sample CRN++ programs from the paper [[1]](#1).
- [`reactions`](MolecularProgrammingLib/src/reactions/)
  - [`CrnInterpreter.fs`](MolecularProgrammingLib/src/reactions/CrnInterpreter.fs): Evaluates the input CRN program and generates a sequence of output states.
  - [`CrnCompiler.fs`](MolecularProgrammingLib/src/reactions/CrnCompiler.fs): Computes a sequence of output states based on networks of chemical reactions [[1]](#1).
  - [`CrnSimulator`](MolecularProgrammingLib/src/reactions/CrnSimulator.fs): Module used to simulate a list of chemical reactions and plot the results.
- [`visualization`](MolecularProgrammingLib/src/visualization/)
  - [`CrnDrawTree.fs`](MolecularProgrammingLib/src/visualization/CrnDrawTree.fs): Represents CRN programs as *Drawing Tree*.
  - [`CrnString.fs`](MolecularProgrammingLib/src/visualization/CrnString.fs): Shows a string representation of CRN programs.
  - [`CrnVisualizer.fs`](MolecularProgrammingLib/src/visualization/CrnVisualizer.fs): Visualizing chemical reactions using `Plotly.NET`.
- [`script.fsx`](MolecularProgrammingLib/src/script.fsx): Demonstration of the implemented functionality.

Furthermore, F# test files can be found in the [`test`](MolecularProgrammingLib/test/) directory and includes:
- [`CrnUnitTests.fs`](MolecularProgrammingLib/test/CrnUnitTests.fs): Unit tests used to properties on the sample CRN programs in [`CrnExamples.fs`](MolecularProgrammingLib/src/domain/CrnExamples.fs).

## Building the project ##
**Software requirements**: .NET SDK 6.0 or higher

Open a CLI tool of your choice and navigate to the [`MolecularProgrammingLib``](MolecularProgrammingLib/) root directory and build the project with

`> dotnet build`
 
The modules are now ready to be imported in another project or `.fsx` script file.

### Running the tests ###
Next, you can run the tests with

`> dotnet test`

## Running the script ##
The [`script.fsx`](MolecularProgrammingLib/src/script.fsx) program can be run either through an interactive terminal or by executing:

`> dotnet run`

## References ##
<a id="1">[1]</a> 
M. Vasic, D. Soloveichik, S. Khurshid (2020). 
*CRN++: Molecular Programming Language*
Springer Nature B.V., 2020, 391-407.