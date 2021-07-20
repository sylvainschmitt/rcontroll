## TROLL

This README file summarize the different executable available.
They rely on 4 parameters:

- **reduced**: `_OUTPUT_reduced` l. 100 of `main.cpp`, activate reduced outputs
- **abc**: `Output_ABC` l. 45 of `main.cpp`, activate ABC outputs. Beware, the use of reduced output is mandatory for ABC to work.
- **forest**: `_FromData` l. 101 of `main.cpp`, activate initialization from data
- **random**: `_NONRANDOM` l. 93 of `main.cpp`, activate fixed seed

Resulting in 8 binaries compiled using the `rcontroll::compile_troll` function:

```
library(rcontroll)
compile_troll(full = T, abc = F, forest = F, random = T) # full
compile_troll(full = T, abc = F, forest = T, random = T) # full forest
compile_troll(full = F, abc = F, forest = F, random = T) # reduced random
compile_troll(full = F, abc = F, forest = F, random = F) # reduced non-random
compile_troll(full = F, abc = F, forest = T, random = T) # reduced forest random
compile_troll(full = F, abc = F, forest = T, random = F) # reduced forest non-random
compile_troll(full = F, abc = T, forest = F, random = T) # abc
compile_troll(full = F, abc = T, forest = T, random = T) # abc forest
```
