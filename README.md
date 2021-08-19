# compilers-ntua

**tonyc** is a compiler made for Tony, an educational programming language for Compilers course at ECE, NTUA. More information about our compiler's architecture and the choices we've made constructing it, can be found in our written [report](docs/report.pdf).
### Team
 * [Dimitris Galanis](https://github.com/DominusTea)
 * [Spyros Pavlatos](https://github.com/spyrospav)

### Installation and build

To install **tonyc** dependencies and build the compiler you should run

```
make depend
make
```

`make depend` automatically installs the following (needed) packages :
  * `gcc`
  * `llvm-10`
  * `clang-10`

### Usage

To compile your program with **tonyc** simply run

```
tonyc { -i | -f} {-O} <filename>.tony
```

When `-i` flag is specified, the program is read from `stdin` and the LLVM IR code is printed to `stdout`. Similarly, if the `-f` flag is specified, the program is read from `stdin` and the Assembly code is printed to `stdout`. If none of the above flags are specified, the compiler will produce `<filename>.imm` (LLVM IR code) and `<filename>.asm` (Assembly code), as well as the final executable `<filename>.out>`. Finally, if the `-O` flag is specified, the compiler will produce optimized code.
