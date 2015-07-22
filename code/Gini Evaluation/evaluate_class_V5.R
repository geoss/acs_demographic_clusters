
evaluate_class<-function (data, cluster_lookup, groups_or_types, variables,denominator_name) 
{
  options(warn = -1)
  df <- data.frame(data)
  b <- c()
  clusters <- c()
  if (!(groups_or_types == "types") & !(groups_or_types == 
                                          "groups")) {
    cat("Groups or types not specified!. This is required. Please try again.", 
        "\n")
    stop
  }
  if (groups_or_types == "types") {
    clusters <- as.character(cluster_lookup[, 3])
    for (i in 1:length(clusters)) {
      clusters[i] <- gsub("[A-Z]", "", clusters[i])
    }
    b <- clusters
  }
  if (groups_or_types == "groups") {
    clusters <- as.character(cluster_lookup[, 2])
    for (i in 1:length(clusters)) {
      temp <- clusters[i]
      if (nchar(temp) == 1) {
        b[i] <- match(temp, LETTERS)
      }
      if (nchar(temp) > 1) {
        temp <- substr(temp, 1, 1)
        b[i] <- match(temp, LETTERS)
      }
    }
  }
  b <- as.numeric(b)
  df <- cbind(df, b)
  mat <- matrix(nrow = max(b), ncol = 0)
  vec <- colnames(df)
  for (k in 1:(length(vec) - 1)) {
    d2 <- as.vector(by(df[, k], df$b, sum))
    mat <- cbind(mat, d2)
  }
  l <- length(vec)
  df[, l] <- NULL
  num_clust <- max(b)
  colnames(mat) <- colnames(df)
  mat <- data.frame(mat)
  Cluster_Number <- c()
  for (i in 1:num_clust) {
    Cluster_Number[i] <- i
  }
  master_list <- list()
  mat <- cbind(Cluster_Number, mat)
#   View(mat)
  for (k in 1:length(variables)) {
    variable_name <- variables[k]
    names <- colnames(data)
    num1 <- match(variable_name, names)
    denominator<-denominator_name
    num2 <- match(denominator, names)
    denominator <- mat[, num2 + 1]
    numerator <- mat[, num1 + 1]
    cluster_number <- mat$Cluster_Number
    nmat <- matrix(nrow = num_clust, ncol = 0)
    nmat <- cbind(cluster_number, denominator)
    nmat <- cbind(nmat, numerator)
    denom <- c()
    numer <- c()
    for (i in 1:num_clust) {
      denom[i] <- (nmat[i, 2]/sum(nmat[, 2]))
      numer[i] <- (nmat[i, 3]/sum(nmat[, 3]))
    }
    nmat <- cbind(nmat, denom)
    nmat <- cbind(nmat, numer)
    Index <- c()
    for (i in 1:num_clust) {
      Index[i] <- (numer[i]/denom[i]) * 100
    }
    nmat <- cbind(nmat, Index)
    nmat <- data.frame(nmat)
    nmat <- nmat[order(nmat$Index, decreasing = TRUE), ]
    den <- c()
    num <- c()
    for (i in 1:num_clust) {
      den[i] <- nmat[i, 4]
      num[i] <- nmat[i, 5]
    }
    XK <- c()
    YK <- c()
    XK[1] <- den[1]
    YK[1] <- num[1]
    for (i in 2:num_clust) {
      XK[i] <- (XK[i - 1] + den[i])
      YK[i] <- (YK[i - 1] + num[i])
    }
    absXminusY <- c()
    for (i in 1:num_clust) {
      absXminusY[i] <- abs(XK[i] - YK[i])
    }
    nmat <- cbind(nmat, XK)
    nmat <- cbind(nmat, YK)
    nmat <- cbind(nmat, absXminusY)
    A <- c()
    B <- c()
    AxB <- c()
    for (i in 1:num_clust) {
      if (i == 1) {
        A[i] <- YK[i]
        B[i] <- XK[i]
      }
      if (i > 1) {
        A[i] <- YK[i] + YK[i - 1]
        B[i] <- XK[i] - XK[i - 1]
      }
    }
    for (i in 1:num_clust) {
      AxB[i] <- A[i] * B[i]
    }
    nmat <- cbind(nmat, A)
    nmat <- cbind(nmat, B)
    nmat <- cbind(nmat, AxB)
    Gini <- abs(1 - (sum(nmat[, 12])))
    GiniCo <- c()
    for (i in 1:num_clust) {
      GiniCo[i] <- abs(1-sum(nmat[,12]))
    }
    nmat <- cbind(nmat, GiniCo)
    master_list[[k]] <- nmat

  }

  cheese <- data.frame()
  for (i in 1:length(master_list)) {
    a <- master_list[[i]]
    x <- c(0, a[, 7])
    y <- c(0, a[, 8])
    naa <- variables[i]
    nap <- nrow(a) + 1
    names <- rep(naa, nap)
    new <- cbind(x, y)
    new <- cbind(new, names)
    cheese <- rbind(cheese, new)
  }
  half <- floor(num_clust/2)
  remainder <- num_clust - half
  dummy1 <- as.numeric(c(rep(0, half), rep(1, remainder + 1)))
  dummy2 <- as.numeric(c(rep(0, half), rep(1, remainder + 1)))
  brie <- cbind(dummy1, dummy2)
  dummy3 <- rep("x=y", nrow(a) + 1)
  brie <- cbind(brie, dummy3)
  colnames(brie) <- c("x", "y", "names")
  cheese <- rbind(cheese, brie)
  cheese$y <- as.numeric(as.character(cheese$y))
  cheese$x <- as.numeric(as.character(cheese$x))
  colours <- c(rep("steelblue", length(master_list)), "black")
  library(ggplot2)
  g <- ggplot(data = cheese, aes(x = y, y = x, group = names, 
                                 colour = names)) + ggtitle("Lorenz curves for specified variables") + 
    geom_line()
  print(g)
  gini_vec <- c()
  dev.new()
  for (i in 1:length(master_list)) {
    a <- master_list[[i]]
    b <- a[i, 13]
    gini_vec[i] <- b
  }
  layout(1)
  par(las = 2)
  par(mar = c(5, 8, 4, 2))
  gini_vec <- sort(gini_vec, decreasing = TRUE)
#   for (i in 1:length(variables)) {
#     cat("Variable: ", variables[i], "   ", "Gini Coefficient: ", 
#         gini_vec[i], "\n")
#   }
#   news <- data.frame(cbind(gini_vec, variables))
#   news$gini_vec <- as.numeric(as.character(news$gini_vec))
#   gg <- ggplot(data = news, aes(x = variables, y = gini_vec, 
#                                 group = variables)) + geom_bar() + ggtitle("Gini Coefficients for variables specified")
#   print(gg)
# 
#   aa <- summary(gini_vec)
#   sd <- sd(gini_vec)
#   cat("Summary statistics for Gini Coefficients: ", "\n")
#   print(aa)
#   cat("Standard deviation: ", "\n")
#   print(sd)
#   cat("Returning List of Calculation Information","\n")
  gini_coefficients<-c()
  for(i in 1:length(variables)){
    cat("Variable:   ",variables[i],"    Gini Co.:    ",master_list[[i]][1,13],"\n")
    gini_coefficients[i]<-master_list[[i]][1,13]
  }
cat("Mean Gini Co.:   ",mean(gini_coefficients),"\n")
  #master_list
  gini_coefficients
}