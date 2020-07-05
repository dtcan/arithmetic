# arithmetic
An interpreter for a simple arithmetic language, describing addition and multiplication of integers.

## Build & Run
Clone the repository, then build and run using [stack](https://docs.haskellstack.org/).
```
git clone https://github.com/dtcan/arithmetic
stack build
stack exec arithmetic-exe
```
This will open a command-line interface where you can type arithmetic expressions that use addition and multiplication. The expression will then be evalutated.
```
> 1+1
2
> 1*2+3+4
9
```
You can type multiple expressions per line, separated by semicolon. These expression will be evaluated in succession.
```
> 1;2;2+1;2*2;2*2+1;2*2+2
1
2
3
4
5
6
```