#############################EX1
#############################
set.seed(123)
a <- runif(1)
print(a)
if(a > 0.5) {
  print("large")
} else {
  print("small")
}
#################################
vector <- rpois(10, lambda = 2)
print(vector)
counter_small <- 0
for(i in 1:10)
{
  if(vector[i] >= 5) {
    print("some big")
    break
  }
  if(vector[i] <= 3) {
    counter_small <- counter_small + 1
    if(counter_small == 10) {
      print("all small")
    }
  } else {
    print("nothing special")
  }
}


#################################
# set.seed(123)
A <- matrix(sample(c(0,1), 25, replace = TRUE), nrow = 5, ncol = 5)
counter_col <- 0
counter_row <- 0
counter_diag <- 0

for(i in 1:5){
  if(colSums(A)[i] == 5) {
    counter_col <- 1
  }
  if(rowSums(A)[i] == 5) {
    counter_row <- 1
  }
  if(diag(A)[i] == 1) {
    counter_diag <- counter_diag + 1
  }
}
if(counter_diag == 2 || counter_row == 1 || counter_col == 1) {
  print("Bingo!")
} else {
  print("Sorry, you lost!")
}

#################################
Bingo <- function(p) {
  A <- matrix(sample(c(0,1), p^2, replace = TRUE), nrow = p, ncol = p)
  counter_col <- 0
  counter_row <- 0
  counter_diag <- 0
  
  for(i in 1:p){
    if(colSums(A)[i] == p){
      counter_col <- 1
    }
    if(rowSums(A)[i] == p){
      counter_row <- 1
    }
    if(diag(A)[i] == 1){
      counter_diag <- counter_diag + 1
    }
  }
  if(counter_diag == 2 || counter_row == 1 || counter_col == 1){
    print("Bingo!")
  }else{
    print("Sorry, you lost!")
  }
}

#############################EX2
#############################
a <- integer()
for (i in 1:100) {
  a[i] <- i
}
###########################################
sum <- 0
for (i in 1:100) {
  sum <- a[i] + sum
}
print(sum)
###########################################
vector = c()
for(i in 1:10) { 
  for(j in 1:i) {
    vector <- c(vector, i) 
  }
}
###########################################
vector = c()
for (i in 3:50) {
  if(i %% 2 == 1) {
    vector <- c(vector, i)
  }
  if(i %% 2 == 0) {
    vector <- c(vector, i, i)
  }
}
###########################################question
num1 = as.character(readline(prompt = "what is your name?"))
a<-unlist(strsplit(num1, split=""))
a<-toupper(a)
for (i in 1:length(a)) {
  b[i]=grep(a[i], LETTERS)
  print(b[i])
}
###########################################
ReplaceGame <- function(n) {
  vector <- c()
  for (i in 1:n) {
    vector <- c(vector, i)
  }
  for (j in 1:n) {
    if(vector[j] %% 3 == 0) {
      print("Fizz")
    }
    if(vector[j] %% 5 == 0) {
      print("Buzz")
    }
    if(vector[j] %% 3 == 0 && vector[j] %% 5 == 0) {
      print("FizzBuzz")
    } else {
      print(vector[j])
    }
  }
}
###########################################
fib <- function(n) {
  fib <- c(1,1)
  for(i in 3:n){fib[i]=fib[i-1]+fib[i-2]}
  return(fib)
}
fib(100)

#############################EX3
#############################question
collatz <- function(n) {
  vector <- c(n)
  # while (n == 1) {
  #   stop("Note! You cannot proceed further when your input is already 1. The total steps are 0; The largest value encountered is 1")
  # }
  while (n!= 1) {
    if(n %% 2 == 0) {
      n <- n/2
      vector <- c(vector, n)
    } else {
      n <- n*3 + 1
      vector <- c(vector, n)
    }
  }
  print(vector)
  sprintf("%s %d", "The total steps are:", length(vector)-1)
  sprintf("%s %d", "The largest value encountered is:", max(vector))
}
###########################################question
vector <- c(1)
while (abs(mean(vector) - 1/2) >= 0.001) {
  a <- runif(1)
  vector <- c(vector, a)
}
# print(vector)
sprintf("%s %d %s", "There are", length(vector), "elements in the vector")
###########################################
GoldenRatio=(1+sqrt(5))/2
i=3
fib=c(1,1)
while(abs(fib[i-1]/fib[i-2]-GoldenRatio)>0.001){
  fib[i] <- fib[i-1]+fib[i-2]
  i=i+1
}
print(fib)
length(fib)
print(i)
print(fib[i-1]/fib[i-2])
print(abs(fib[i-1]/fib[i-2] - GoldenRatio))

#############################EX4
#############################
selfDiag <- function(mat) {
  a <- c()
  for( i in 1:nrow(mat)){ 
    for(j in 1:ncol(mat)){
      if(i == j) { 
        a[i] <- mat[i,j]
      }
    }
  }
  print(a)
}
###########################################baocuo fanwei
longestCollatz <- function(n) {
  store <- list()
  vector <- c()
  j <- 1
  for (i in 2:n) {
    vector <- c(i)
    while (i != 1) {
      if(i %% 2 == 0) {
        i <- i/2
        vector <- c(vector, i)
      } else {
        i <- i*3 + 1
        vector <- c(vector, i)
      }
    }
    store[[j]] <- vector
    j <- j + 1
    vector <- c()
  }
  # return(store)
  final <- list()
  sequence <- c()
  a <- c()
  for (m in 1:length(store)) {
    a[m] <- length(store[[m]])
    sequence <- store[[which(a == max(a))]]
    final <- list(index = which(a == max(a))+1, length = length(sequence), sequence = sequence)
  }
  return(final)
}
    
###########################################whether definition (0,0)

factorial <- function(x){
  y <- 1
  for(i in 1:x){
    y <-y*((1:x)[i])
  }
  if(x == 0) {
    return(1)
  }
  return(y)
}

BinCoef <- function(n, k) {
  factorial(n)/(factorial(k)*factorial(n-k))
}
###########################################haven't look for details
## find a solution with: you might want to Vectorize your binomial coefficient function for this subquestion
pascalTriangle <- function(h) {
  for(i in 0:(h-1)) {
    s <- ""
    for(k in 0:(h-i)) s <- paste(s, "  ", sep="")
    for(j in 0:i) {
      s <- paste(s, sprintf("%3d ", BinCoef(i, j)), sep="")
    }
    print(s)
  }
}

#############################EX5
#############################
col <- list()
for (i in 1:10) {
  col[[i]] <- sample(c(1,0), 100, replace = TRUE, prob = c(1/i,(1-1/i)))
  mat <- cbind(mat, col[[i]])
}
mat <- mat[,2:11]
apply(mat, 2, mean)
apply(mat, 2, sd)
###########################################
col2 <- list()
# b <- c()
# counter <- 1
for (i in 1:10) {
  counter <- 1
  b <- c()
  while (counter <= 10) {
    a <- sample(c(1,0), 1, replace = TRUE, prob = c(1/i, (1-1/i)))
    if(a == 1) {
      counter <- counter + 1
      b <- c(b, a)
    }
    if(a == 0) {
      b <- c(b, a)
    }
  }
  col2[[i]] <- b
}

###########################################
a <- lapply(col2, mean)
print(a)
str(a)
b <- sapply(col2, mean)
print(b)
str(b)
###########################################
extractVector <- function(v) {
  v <- v[1:10]
  return(v)
}
lapply(col2, extractVector)
###########################################addition apply?
mapply(function, ...)

a <- col2[[1]]
sapply(a, mean)
