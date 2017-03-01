# Load data
data(iris)

# Input & output
irs_ipt <- iris[, 1:4] #input
irs_opt <- iris[, 5] #output

# Step 2: standardize data
irs_ipt <- scale(irs_ipt,
                 center = TRUE, #subtract mean
                 scale = FALSE #divide by sd
                 )

apply(irs_ipt, 2, mean) #check if mean = 0
apply(irs_ipt, 2, sd) #check if sd = 1

# Step 3: find cov matrix
irs_cov <- cov(irs_ipt, irs_ipt)

# Step 4: eigenvectors, eigenvalues
irs_eigen <- eigen(irs_cov, symmetric = TRUE)
irs_eigen_vec <- irs_eigen$vectors #eigen vectors
irs_eigen_val <- irs_eigen$values #eigen values

apply(irs_eigen_vec^2, 1, sum) #check if eigen vecs are unit

# Step 5: forming feature vectors
pca1 <- irs_eigen_vec[1, ]

final_data <- pca1 %*% irs_ipt
final_data

