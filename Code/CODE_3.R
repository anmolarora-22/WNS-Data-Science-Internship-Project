
hist(a$Keeps.my.hair.cleaner.for.longer,
     col = c("red","blue"),
     border = "black",
     main = "Keeps.my.hair.cleaner.for.longer",
     xlab = "Response",
     ylab = "Frequency",
     ylim = c(0,6000),
     xaxt = "n")

legend("top",
       legend = c("Red = 0", "Blue = 1"),
       fill = c("red", "blue"),
       horiz = TRUE,
       bty = "n")


