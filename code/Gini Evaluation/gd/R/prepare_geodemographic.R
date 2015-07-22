prepare_geodemographic<-function (data, compare = NULL, remove = NULL) 
{
  options(warn=-1)
  suppressWarnings(library(car))
  n <- ncol(data)
  r <- nrow(data)
  p <- matrix(nrow = r, ncol = 0)
  count <- 0
  for (i in 1:n) {
    
    
    if (attr(data[, i], "non_count?")) {
      count <- count + 1
      Names <- colnames(data)
      nc <- data[, i]
      non <- Names[i]
      
      
      if(attr(data[,i],"transform")=="NONE,BC"){
        
        
        bc <- boxcox(nc ~ 1, plotit = FALSE)
        bestlambda <- with(bc, x[which.max(y)])
        NCBC <- bcPower(nc, bestlambda)
        #         NCBC <- scale(NCBC)
        p <- cbind(p, NCBC)
        colnames(p)[count] <- non
        
      }
      if(attr(data[,i],"transform")=="NONE,LOG"){
        NCLOG<-log(nc)
        #         NCLOG<-scale(NCLOG)
        p <- cbind(p, NCLOG)
        colnames(p)[count] <- non
        
      }
      
    }
    if ((!attr(data[, i], "is_denom?")) & (attr(data[, i], 
                                                "denominator") != "default")) {
      denominator <- attr(data[, i], "denominator")
      count <- count + 1
      Names <- (colnames(data))
      numerator <- Names[i]
      j <- match(denominator, Names)
      divisor <- data[, j]
      dividend <- data[, i]
      dividend <- dividend + 1
      divisor <- divisor + 1
      r <- sum(data[, j])
      s <- sum(data[, i])
      nat_avg <- (s/r)
      
      if (attr(data[, i], "transform") == "PCT,BC") {
        q <- (dividend/divisor) * 100
        bc <- boxcox(q ~ 1, plotit = FALSE)
        bestlambda <- with(bc, x[which.max(y)])
        PCTBC <- bcPower(q, bestlambda)
        #         PCTBC <- scale(PCTBC)
        p <- cbind(p, PCTBC)
        colnames(p)[count] <- numerator
        if(ncol(p)>48)
          cat("pctbc")
      }
      if (attr(data[, i], "transform") == "PCT,LOG") {
        q <- (dividend/divisor) * 100
        PCTLOG <- log(q)
        #         PCTLOG <- scale(PCTLOG)
        p <- cbind(p, PCTLOG)
        colnames(p)[count] <- numerator
        if(ncol(p)>48)
          cat("pctlog")
      }
      if (attr(data[, i], "transform") == "IND,BC") {
        q <- (dividend/divisor)
        q <- (q/nat_avg) * 100
        bc <- boxcox(q ~ 1, plotit = FALSE)
        bestlambda <- with(bc, x[which.max(y)])
        INDBC <- bcPower(q, bestlambda)
        #         INDBC <- scale(INDBC)
        p <- cbind(p, INDBC)
        colnames(p)[count] <- numerator
        if(ncol(p)>48)
          cat("indbc")
      }
      if (attr(data[, i], "transform") == "IND,LOG") {
        q <- (dividend/divisor)
        w <- (q/nat_avg) * 100
        INDLOG <- log(w)
        #         INDLOG <- scale(INDLOG)
        p <- cbind(p, INDLOG)
        colnames(p)[count] <- numerator
        if(ncol(p)>48)
          cat("indlog")
      }
      if (attr(data[, i], "transform") == "IND,NONE") {
        
        q <- (dividend/divisor)
        w <- (q/nat_avg) * 100
        #         w <- scale(w)
        
        p <- cbind(p, w)
        colnames(p)[count] <- numerator
        if(ncol(p)>48)
          cat("ind")
      }
      if (attr(data[, i], "transform") == "PCT,NONE") {
        q <- (dividend/divisor) * 100
        #         q <- scale(q)
        p <- cbind(p, q)
        colnames(p)[count] <- numerator
        if(ncol(p)>48)
          cat("pct")
      }
    }
  }
  if (!is.null(remove)) {
    for (i in 1:length(remove)) {
      p.1 <- data.frame(p)
      var_name <- remove[i]
      
      names <- colnames(p.1)
      temp <- match(var_name, names)
      
      p.1 <- p.1[, -temp]
      p <- p.1
    }
  }
  
  
  #RESULTS

  if (!is.null(compare)) {
    if (compare == "correlation_matrix") {
      
      print("Covariance Matrix:")
      print(cor(p))

    }
    if (compare == "heatmap") {
      suppressWarnings(library(car))
      suppressWarnings(library(gplots))
      
      w <- c(4, 4, 4, 4)
      x <- c(4, 4, 4, 4)
      y <- c(4, 4, 4, 4)
      z <- c(5, 1, 2, 3)
      w1 <- rbind(w, x)
      y1 <- rbind(y, z)
      ma <- rbind(w1, y1)
      h <- heatmap.2(cor(p), Rowv = TRUE, Colv = TRUE, 
                     dendrogram = "none", key = TRUE,symkey=TRUE,cexRow=0.6,cexCol=0.6,margins = c(8, 8),
                     breaks = c(-1, -0.6, -0.2, 0.2, 0.6, 1), col = c("#A6611A", 
                                                                      "#DFC27D", "#F5F5F5", "#80CDC1", "#018571"), 
                     density.info = "none", trace = "none", colsep = c(1:ncol(cor(p))), 
                     rowsep = c(1:nrow(cor(p))), sepwidth = c(0.001, 
                                                              0.15), sepcolor = "white",lhei = c(0.13,0.65),lwid = c(0.2,0.65))

      #       legend(x = 0.06363513, y = 1.9732542, c("Cor: -1 < -0.6", 
      #                                               "Cor: -0.6 < -0.2", "Cor: -0.2 > 0.2", "Cor: 0.2 > 0.6", 
      #                                               "Cor: 0.6 > 1"), fill = c("#A6611A", "#DFC27D", 
      #                                                                         "#F5F5F5", "#80CDC1", "#018571"), xpd = NA)
      
    }
  }
  
  rownames(p) <- rownames(data)

p
}
