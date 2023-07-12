# Babylon tower - School project

School project for the 2022/23 FLP course at FIT VUT.
The project is written in SWI Prolog.

The solution uses IDS algorithm to solve the problem of Babylon tower.
The problem is to find a sequence of moves that will result in a tower with all marbles in the correct order.

## Requirements and compilation

`swipl` compiler is required.
To compile the project, run `make`.

## Usage

The input is read from stdin.
Run the `flp22-log` executable.


## Example

Input:
```
A1 B1 C1 D1 E1 F1 
C2 D2 E2 F2 E3 B2 
A2 B3 C3 D3 E4 F3 
A3 B4 C4 D4 ** F4
```

```bash
./flp22-log < in/in_4
```

Output:
```
A1 B1 C1 D1 E1 F1 
C2 D2 E2 F2 E3 B2 
A2 B3 C3 D3 E4 F3 
A3 B4 C4 D4 ** F4 

A1 B1 C1 D1 E1 F1 
C2 D2 E2 F2 ** B2 
A2 B3 C3 D3 E3 F3 
A3 B4 C4 D4 E4 F4 

A1 B1 C1 D1 E1 F1 
** B2 C2 D2 E2 F2 
A2 B3 C3 D3 E3 F3 
A3 B4 C4 D4 E4 F4 

A1 B1 C1 D1 E1 F1 
A2 B2 C2 D2 E2 F2 
A3 B3 C3 D3 E3 F3 
** B4 C4 D4 E4 F4
```

## Experiment results

| Input | Solution | Length | Time |
|-------|--------|-------|-----|
| in_1  | 1      | 1     | 5 min |
| in_2              | 1      | 1     | 0.02s |
| in_3              | 1      | 2     | 0.04s |
| in_4 | 1   | 4     | 0.10s |
| in_5              | 1      | 2     | 0.03s |
| in_6              | 1      | 1     | 0.02s |
| in_7              | 1      | 7     | 0.16s |
| in_8              | 1      | 8     | 22.6s |
