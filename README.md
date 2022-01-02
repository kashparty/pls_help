# Propositional Logic Simplification Helper: pls_help

Haskell code to do things in propositional logic. Basically just an experiment 
for me. Even though this is called a "Simplification Helper", I no longer aim
for it to do any minimisation step-by-step using simplification rules. Instead,
it *helps* the user to do that themselves by showing them what the end result
should be (the minimal possible formula) using the Quine-McCluskey algorithm.

Currently supports:
- [x] Formula parsing
- [x] Formula evaluation
- [x] Formula minimisation
- [x] Truth table generation
- [x] Satisfiability/validity checking
