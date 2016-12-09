The short Haskell programs provides a simple function of `serialise` to convert a register machine, an angle-bracketed expression or an instruction to an integer. In order to decode a serialised representation, use the provided functions `decodeRegMachine`, `decodeInstruction`, `decodeSingleBracketExpr`, `decodeDoubleBracketExpr` or `decodeList`.

For example, a register machine defined as follows 

```
L0: R0- -> L0, L2
L1: Halt
```

can be serialised using the following expression

```
> let regMachine = RegMachine [Sub 0 0 2, Halt]
> serialise regMachine
786432
```

To decode the register machine:

```
> decodeRegMachine 786432
L0: R0- -> L0, L2
L1: Halt
```