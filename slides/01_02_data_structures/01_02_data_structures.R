source('common.R')

insureds <- c('Hair by Brian', 'Rich Family Farm', 'R Programmers R Us')
premium <- c(1000, 2000, 3000)
claim_count <- c(0L, 1L, 0L)
claim_desc <- c(NA, "Brian gave Adam's goat a bad haircut", NA)

pies <- seq(from = 0, by = pi, length.out = 5)
i <- 1:5
year <- 2000:2004

nums <- 1:100
nums[20]
nums[20:30]
nums[c(10, 20, 30, 40)]

i <- 5:9
i[c(TRUE, FALSE, FALSE, FALSE, TRUE)]
i[i > 7]

b <- i > 7
b
i[b]

N <- 10e3
B0 <- 5
B1 <- 1.5

e <- rnorm(N, mean = 0, sd = 1)
X1 <- sample(-10:10, size = N, replace = TRUE)

Y <- B0 + B1 * X1 + e

x <- 1:10
y <- 5:15
x %in% y

x = 1:50
sum(x)
mean(x)
max(x)
length(x)
var(x)

claims_data <- data.frame(insureds, premium, claim_count, claim_desc)
print(claims_data)

policies <- data.frame(
  pol_number = 1:10,
  prem = c(100, 110, 90, 120, 115, 60, 300, 30, 100, 95),
  age = c(21, 22, 32, 54, 34, 19, 70, 54, 42, NA)
)

teams  <- c("Toronto", "New York", "Baltimore", "Tampa Bay", "Boston")
wins   <- c(93, 87, 81, 80, 78)
losses <- c(69, 75, 81, 82, 84)

alEast <- data.frame(Team = teams, W = wins, L = losses)

alEast[1, ]
alEast[, 2]
alEast[, c('W', 'L')]
alEast[3]
alEast[c(T, T, F, F, F), ]
alEast[alEast$Team == 'New York', ]

x <- list()
typeof(x)
x[[1]] <- c("Hello", "there", "this", "is", "a", "list")
x[[2]] <- c(pi, exp(1))
summary(x)
str(x)

y <- list()
y$Artist <- c("Lou Reed", "Patti Smith")
y$Age <- c(45, 63)
names(y)
