install.packages("psych")
library(psych)
library(readxl)

dataset <- read_excel(file.choose())
dataset
head(dataset)
dim(dataset)
fit <- princomp(dataset, cor = TRUE)
summary(fit)
loadings(fit)
plot(fit, type = "lines")
fit_rotate4 <- principal(dataset, nfactors = 4, rotate = "varimax")
fit_rotate4



fit_rotate5 <- principal(dataset, nfactors = 5, rotate = "varimax")
fit_rotate5     
fit_rotate6 <- principal(dataset, nfactors = 6, rotate = "varimax")
fit_rotate6     

install.packages("writexl")
library(writexl)

library(writexl)

output <- data.frame(
  Item = rownames(fit_rotate6$loadings),
  unclass(fit_rotate6$loadings),
  h2 = fit_rotate6$communality,
  u2 = fit_rotate6$uniquenesses,
  com = fit_rotate6$complexity
)

h2_u2_com <- data.frame(
  Item = rownames(fit_rotate6$loadings),
  H2 = fit_rotate6$communality,
  U2 = fit_rotate6$uniquenesses,
  COM = fit_rotate6$complexity
)

write.csv(h2_u2_com, "H2_U2_COM.csv", row.names = FALSE)


write_xlsx(output, "fit_rotate6new.xlsx")

write_xlsx(as.data.frame(unclass(fit_rotate4$loadings)),
           "fit_rotate4h2u2.xlsx")






rownames(fit_rotate4$loadings)

variance4 <- data.frame(
  Factor = colnames(fit_rotate4$Vaccounted),
  SS_Loading = fit_rotate4$Vaccounted["SS loadings", ],
  Proportion_Variance = fit_rotate4$Vaccounted["Proportion Var", ],
  Cumulative_Variance = fit_rotate4$Vaccounted["Cumulative Var", ],
  Proportion_Explained = fit_rotate4$Vaccounted["Proportion Explained", ],
  Cumulative_Proportion = fit_rotate4$Vaccounted["Cumulative Proportion", ]
)


write.csv(variance4, "Variance_4_Factors.csv", row.names = FALSE)


# 5 Factors
variance5 <- data.frame(
  Factor = colnames(fit_rotate5$Vaccounted),
  SS_Loading = fit_rotate5$Vaccounted["SS loadings", ],
  Proportion_Variance = fit_rotate5$Vaccounted["Proportion Var", ],
  Cumulative_Variance = fit_rotate5$Vaccounted["Cumulative Var", ],
  Proportion_Explained = fit_rotate5$Vaccounted["Proportion Explained", ],
  Cumulative_Proportion = fit_rotate5$Vaccounted["Cumulative Proportion", ]
)


write.csv(variance5, "Variance_5_Factors.csv", row.names = FALSE)


# 6 Factors
variance6 <- data.frame(
  Factor = colnames(fit_rotate6$Vaccounted),
  SS_Loading = fit_rotate6$Vaccounted["SS loadings", ],
  Proportion_Variance = fit_rotate6$Vaccounted["Proportion Var", ],
  Cumulative_Variance = fit_rotate6$Vaccounted["Cumulative Var", ],
  Proportion_Explained = fit_rotate6$Vaccounted["Proportion Explained", ],
  Cumulative_Proportion = fit_rotate6$Vaccounted["Cumulative Proportion", ]
)


write.csv(variance6, "Variance_6_Factors.csv", row.names = FALSE)





colnames(dataset)
factor1_data<-dataset[ ,c(
  "This.brand.is.enjoyable.to.use",
  "Leaves.a.pleasant.smell.on.my.hair",
  "Makes.my.life.easier",
  "Helps.me.achieve.the.look.I.want",
  "This.brand.helps.me.feel.more.confident",
  "Keeps.my.hair.cleaner.for.longer",
  "Cleans.my.hair.without.stripping",
  "Detangles.my.hair.better.than.other.brands",
  "Has.the.best.conditioners.I.can.get",
  "Moisturizes.my.hair.better.than.other.brands"
)]
colnames(factor1_data)
head(factor1_data)

library(psych)

fit_factor1_2 <- fa(
  factor1_data,
  nfactors = 2,
  rotate = "varimax",
  fm = "minres"
)

print(fit_factor1_2$loadings,cutoff = 0)
fit_factor1_2$Vaccounted

Factor1_Solution <- data.frame(
  Item = rownames(fit_factor1_2$loadings),
  MR1 = fit_factor1_2$loadings[, 1],
  MR2 = fit_factor1_2$loadings[, 2]
)
Factor1_Solution

Factor1_Variance <- as.data.frame(
  fit_factor1_2$Vaccounted
)

Factor1_Variance

library(openxlsx)
write.xlsx(
  list(
    Factor1_Solution = Factor1_Solution,
    Factor1_Variance = Factor1_Variance
  ),
  "Factor1_Output.xlsx",
  rowNames = FALSE
)


factor4_data <- dataset[, c(
  "Repairs.hair.damage",
  "Strengthens.my.hair.bonds",
  "Prevents.hair.color.fade",
  "Prevents.hair.loss",
  "Helps.my.hair.grow",
  "Provides.vitamins.to.your.hair",
  "Improves.my.hair.health"
)]

colnames(factor4_data)
dim(factor4_data)
head(factor4_data)

library(psych)

fit_factor4_2 <- fa(
  factor4_data,
  nfactors = 2,
  rotate = "varimax",
  fm = "minres"
)

print(fit_factor4_2$loadings, cutoff = 0)
fit_factor4_2$Vaccounted

Factor4_Solution <- data.frame(
  Item = rownames(fit_factor4_2$loadings),
  MR1 = fit_factor4_2$loadings[, 1],
  MR2 = fit_factor4_2$loadings[, 2]
)

Factor4_Solution

Factor4_Variance <- as.data.frame(
  fit_factor4_2$Vaccounted
)

Factor4_Variance

library(openxlsx)

write.xlsx(
  list(
    Factor4_Solution = Factor4_Solution,
    Factor4_Variance = Factor4_Variance
  ),
  "Factor4_Output.xlsx",
  rowNames = TRUE
)

