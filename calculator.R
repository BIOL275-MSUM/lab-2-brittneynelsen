
# This is an R script containing the code used in Lab 2 Intro to RStudio

# ASSIGNMENT:
#   1. Run the code below to make sure it works
#   2. Edit the code to complete the self assessment
#   3. Copy chunks of the final code to your R Markdown document calculator.Rmd

k <- c(1, 2, 10, 4.7, 5.0)
k

length(k)  # sample size
median(k)  # median value
mean(k)    # mean value
sd(k)      # standard deviation

n <- length(k) # sample size
n

sem <- sd(k) / sqrt(n) # standard error of the mean
sem

mean(k) + 1.96 * sem                             # upper limit
mean(k) - 1.96 * sem                             # lower limit
c(mean(k) + 1.96 * sem, mean(k) - 1.96 * sem)    # both limits combined
