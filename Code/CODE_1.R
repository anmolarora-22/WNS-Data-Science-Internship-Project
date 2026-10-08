anmol_study<-read.csv(file.choose())
anmol_study


nrow(anmol_study)
ncol(anmol_study)
is.na(anmol_study)
colSums(is.na(anmol_study))
sum(is.na(anmol_study))
View(anmol_study)
dim(anmol_study)
str(anmol_study)


table(anmol_study$Provides.superior.scalp.care)  #frequency for individual
frequency<-lapply(anmol_study, table)            #frequency for all the columns
frequency
install.packages("writexl")
library(writexl)
write_xlsx(as.data.frame(frequency), "freq_beforeimp.xlsx")


mean(anmol_study$Provides.superior.scalp.care)      #mean for individual
sapply(anmol_study, mean, na.rm = TRUE)             #mean for all the columns
anmol_study[] <- lapply(anmol_study, function(x) {    #imputation 
  x[is.na(x)] <- mean(x, na.rm = TRUE)
  x
})


median(anmol_study$Provides.superior.scalp.care)      #median for individual
sapply(anmol_study, median, na.rm = TRUE)             #median for all the columns
anmol_study[] <- lapply(anmol_study, function(x) {    #imputation 
  x[is.na(x)] <- median(x, na.rm = TRUE)
  x
})


get_mode(anmol_study$Keeps.my.hair.cleaner.for.longer)  # mode for one column
anmol_mode <- sapply(anmol_study, get_mode)             #mode for all
anmol_mode

get_mode <- function(x) {
  ux <- unique(x[!is.na(x)])                             #creating mode function
  ux[which.max(tabulate(match(x, ux)))]
}

anmol_study[] <- lapply(anmol_study, function(x) {       #replacing mode with missing values
  x[is.na(x)] <- get_mode(x)
  x
})

install.packages("writexl")
library(writexl)
write_xlsx(as.data.frame(anmol_study), "modeimp.xlsx")


sd(anmol_study$Prevents.hair.color.fade)                # sd for individual
sapply(anmol_study, sd, na.rm = TRUE)                   # sd for all 

row_sd <- apply(anmol_study, 1, sd, na.rm = TRUE)       # sd for row level
row_sd

write_xlsx(as.data.frame(row_sd), "sdrowlevel.xlsx")


cor_matrix <- cor(anmol_study, use = "pairwise.complete.obs", method = "pearson")
cor_matrix

write_xlsx(as.data.frame(cor_matrix), "cormatrixx.xlsx")
