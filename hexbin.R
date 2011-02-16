library(lattice)
library(hexbin)
data(NHANES)
hexbinplot(Hemoglobin ~ TIBC | Sex, data = NHANES,
   aspect = 0.85, type = "g")
