# the PCA for excel

database <- read.csv("/Users/lin/Desktop/the analysis of blood sugar/rawdata3.csv",
                       header=T,
                       sep=",")
head(database)
#database <- as.data.frame(database)
#View(database)
pca <- prcomp(formula =~f+h+i+j+k+l+n+p,
              data= database,
              scale =TRUE
              )
pca

# Scree Plot
plot(pca,
     type="line",
     main="scree Plot for rawdata")
abline(h =1,col="blue")

# Pareto Plot
vars <- (pca$sdev)^2
vars

props <- vars/sum(vars)
props

cumulative.props <- cumsum(props)
cumulative.props
plot(cumulative.props) # It can explation the 90.26% variation

top3_pca.data <- pca$x[,1:3]
top3_pca.data

# eigenVector diff linear combination
pca$rotation

# Extract first 3 PC
top3.pca.eigenvector <- pca$rotation[, 1:3]
top3.pca.eigenvector

# Plot the main loading figure
first.pca <- top3.pca.eigenvector[, 1]   #  first component
second.pca <- top3.pca.eigenvector[, 2]  #  second component
third.pca <- top3.pca.eigenvector[, 3]   #  third component

#the first component
first.pca[order(first.pca,decreasing = FALSE)]

dotchart(first.pca[order(first.pca, decreasing=FALSE)] ,   # ??????????????????
         main="Loading Plot for PC1",                      # ?????????
         xlab="Variable Loadings",                         # x????????????
         col="red") 
# the PC1 related I H P
# the pc1 related I K F
# the pc1 related I P N

#the second component
second.pca[order(second.pca,decreasing = FALSE)]

dotchart(second.pca[order(second.pca,decreasing = FALSE)],
         main="Loading Plot for PC2",
         xlab="Variable Loadibgs",
         col="blue")
# the PC2 related K H F
# the PC2 related N J F (k)
# the PC2 related N J F (K)

# the third component
dotchart(third.pca[order(third.pca,decreasing = FALSE)],
         main="Loading Plot for PC3",
         xlab="Variable Loadings",
         col="blue")
# the PC3 related I N (K)
# the PC3 related P K N (I F)
# the PC3 related P F J (I L N)

# choosing PC1 and PC2 plot the main loading
biplot(pca,choices=1:2)
biplot(pca,choices=2:3)
