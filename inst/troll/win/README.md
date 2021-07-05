## TROLL

This README file summarize the different executable available in windows.
They rely on 3 parameters:

- **abc**: `Output_ABC` l. 45 of `main.cpp`, activate ABC outputs. Beware, the use of reduced output is mandatory for ABC to work.
- **reduced**: `_OUTPUT_reduced` l. 100 of `main.cpp`, activate reduced outputs
- **forest**: `_FromData` l. 101 of `main.cpp`, activate initialization from data

Resulting in 5 binaries used in `rcontroll`:

- `TROLL_abc.exe`: abc=1, reduced=1, forest=0
- `TROLL_full.exe`: abc=0, reduced=0, forest=0
- `TROLL_full_forest.exe`: abc=0, reduced=0, forest=1
- `TROLL_reduced.exe`: abc=0, reduced=1, forest=0
- `TROLL_reduced_forest.exe`: abc=0, reduced=1, forest=1

TROLL code: TROLL_consolidated_v3.0 `main.cpp`

Command `g++ main.cpp -O3 -o TROLL.exe -lgsl -lgslcblas -Wall`
