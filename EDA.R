data <- read.csv(file.choose())
data
View(data)
ncol(data)
nrow(data)
str(data)
dim(data)
summary(data)
head(data)
tail(data)

install.packages("psych")
library(psych)
describe(data)

#Missing Values 
is.na(data)
colSums(is.na(data))
sum(is.na(data))
#data <- na.omit(data)    #To remove missing values but we will perform imputation
#data

#Duplicates
duplicated(data)
sum(duplicated(data))
#data <- data[!duplicated, ]    To remove duplicates 
#data
#unique(data) To remove duplicates 

#Histograms
#Histograms for individual variable
hist(data$Keeps.my.hair.cleaner.for.longer,
     main = "provides superior scalp care",
     xlab = "responses",
     ylab = "frequency",
     col = c("blue", "red"),
     border = "pink",
     ylim = c(0, 6000)
     
     )
legend("top",
       legend = c("blue = 0", "red = 1"),
       fill = c("blue", "red"),
       horiz = TRUE,
       bty = "n")



#Histograms for all 
par(mfrow = c(5, 6), mar = c(3, 3, 2, 1))

for (i in 1:26) {
  hist(data[, i],
       main = names(data)[i],
       xlab = "responses",
       ylab = "frequency",
       breaks = c(-0.5, 0.5, 1.5),
       col = c("blue", "red"),
       border = "pink",
       ylim = c(0, 6000),
       cex.main = 0.7)
  
  legend("top",
         legend = c("blue = 0", "red = 1"),
         fill = c("blue", "red"),
         horiz = TRUE,
         bty = "n",
         cex = 0.5)
}




dev.off()
graphics.off()
par(mfrow = c(1, 1))

#Frequency
table(data$Provides.superior.scalp.care)   #frequency for individual

frequency <- lapply(data, table)          #frequency for all at once
frequency

#Mean, Median and Mode

mean(data$Provides.superior.scalp.care)      #mean for individual

sapply(data, mean, na.rm = TRUE)             #mean for all the columns

data[] <- lapply(data, function(x) {         #imputation 
  x[is.na(x)] <- mean(x, na.rm = TRUE)
  x
})


median(data$Provides.superior.scalp.care)      #median for individual

sapply(data, median, na.rm = TRUE)             #median for all the columns

data[] <- lapply(data, function(x) {           #imputation 
  x[is.na(x)] <- median(x, na.rm = TRUE)
  x
})


get_mode(data$Keeps.my.hair.cleaner.for.longer)  # mode for one column

anmol_mode <- sapply(data, get_mode)             #mode for all
anmol_mode

get_mode <- function(x) {
  ux <- unique(x[!is.na(x)])                     #creating mode function
  ux[which.max(tabulate(match(x, ux)))]
}

data[] <- lapply(data, function(x) {       #replacing mode with missing values
  x[is.na(x)] <- get_mode(x)
  x
})


install.packages("writexl")
library(writexl)

write_xlsx(as.data.frame(frequency), "freq_bimp0000000.xlsx")
write_xlsx(as.data.frame(data), "mode_imp0000.xlsx")

frequency_aimp <- lapply(data, table)
frequency_aimp

write_xlsx(as.data.frame(frequency_aimp), "freq_afimp0000000000.xlsx")


#Standard deviation

sd(data$Provides.superior.scalp.care)     #Standard deviation for individual
sapply(data, sd, na.rm = True)            #Standard deviation for all

row_sd <- apply(data, 1, sd, na.rm = True)   #Standard deviation across respondents(row level)
row_sd

#Correlation 

cor_matrix <- cor(data, use = "pairwise.complete.obs", method = "pearson")
cor_matrix