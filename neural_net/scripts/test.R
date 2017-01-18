samp <- c(sample(1:50,25), sample(51:100,25), sample(101:150,25))
x=1:4
y=5
traindata=iris[samp,]
testdata=iris[-samp,]
hidden=6
maxit=2000
display=50
abstol=1e-2
lr = 1e-2
reg = 1e-3
display = 100
random.seed = 1


# to make the case reproducible.
set.seed(random.seed)

# total number of training set
N <- nrow(traindata)

# extract the data and label
# don't need atribute 
X <- unname(data.matrix(traindata[,x]))
Y <- traindata[,y]
if(is.factor(Y)) { Y <- as.integer(Y) }
# updated: 10.March.2016: create index for both row and col
Y.len   <- length(unique(Y))
Y.set   <- sort(unique(Y))
Y.index <- cbind(1:N, match(Y, Y.set))

# number of input features
D <- ncol(X)
# number of categories for classification
K <- length(unique(Y))
H <-  hidden

# create and init weights and bias 
W1 <- 0.01*matrix(rnorm(D*H), nrow=D, ncol=H)
b1 <- matrix(0, nrow=1, ncol=H)

W2 <- 0.01*matrix(rnorm(H*K), nrow=H, ncol=K)
b2 <- matrix(0, nrow=1, ncol=K)

# use all train data to update weights since it's a small dataset
batchsize <- N
# updated: March 17. 2016
# init loss to a very big value
loss <- 100000

# Training the network
i <- 1

# forward ....
# 1 indicate row, 2 indicate col
hidden.layer <- sweep(X %*% W1 ,2, b1, '+')

# neurons : ReLU
hidden.layer <- pmax(hidden.layer, 0)
score <- sweep(hidden.layer %*% W2, 2, b2, '+')

# softmax
score.exp <- exp(score)
probs <-sweep(score.exp, 1, rowSums(score.exp), '/') 

# compute the loss
# corect.logprobs <- -log(probs[Y.index])
# data.loss  <- sum(corect.logprobs)/batchsize
# reg.loss   <- 0.5*reg* (sum(W1*W1) + sum(W2*W2))
# loss <- data.loss + reg.loss

# backward ....
dscores <- probs
dscores[Y.index] <- dscores[Y.index] -1

dscores <- dscores / batchsize


dW2 <- t(hidden.layer) %*% dscores 
db2 <- colSums(dscores)

dhidden <- dscores %*% t(W2)
dhidden[hidden.layer <= 0] <- 0

dW1 <- t(X) %*% dhidden
db1 <- colSums(dhidden) 

# update ....
dW2 <- dW2 + reg*W2
dW1 <- dW1  + reg*W1

W1 <- W1 - lr * dW1
b1 <- b1 - lr * db1

W2 <- W2 - lr * dW2
b2 <- b2 - lr * db2

# final results
# creat list to store learned parameters
# you can add more parameters for debug and visualization
# such as residuals, fitted.values ...
model <- list( D = D,
               H = H,
               K = K,
               # weights and bias
               W1= W1, 
               b1= b1, 
               W2= W2, 
               b2= b2)
