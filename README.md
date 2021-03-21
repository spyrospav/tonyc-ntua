# compilers-ntua
**tonyc** is a compiler made for Tony, an educational programming language for Compilers course at ECE, NTUA.

### Team
 * [Dimitris Galanis](https://github.com/DominusTea)
 * [Spyros Pavlatos](https://github.com/spyrospav)

### Installation

To install **tonyc** you should run 

```
make
make install
```

### Usage

To compile your programm with **tonyc** simply run 

```tonyc { -i | -f} {-o} <filename>```

When `-i` flag is specified, the program is read from `stdin` and the LLVM IR code is printed to `stdout`. Similarly, if the `-f` flag is specified, the program is read from `stdin` and the Assembly code is printed to `stdout`. If none of the above flags are specified, the compiler will produce `<filename>.imm` (LLVM IR code) and `<filename>.asm` (Assembly code), as well the final executable `<filename>.out>`. Finally, if the `-o` flag is specified, the compiler will produce optimized code.
