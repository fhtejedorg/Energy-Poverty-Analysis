gwr.basic2 <- function (formula, data, regression.points, bw, kernel = "bisquare", 
          adaptive = FALSE, p = 2, theta = 0, longlat = F, dMat, F123.test = F, 
          cv = F, W.vect = NULL, parallel.method = FALSE, parallel.arg = NULL) 
{
  timings <- list()
  timings[["start"]] <- Sys.time()
  this.call <- match.call()
  p4s <- as.character(NA)
  spdf <- FALSE
  sf.poly <- FALSE
  if (inherits(data, "Spatial")) 
    spdf <- TRUE
  else if (any((st_geometry_type(data) == "POLYGON")) | any(st_geometry_type(data) == 
                                                            "MULTIPOLYGON")) 
    sf.poly <- TRUE
  if (missing(regression.points)) {
    rp.given <- FALSE
    regression.points <- data
    if (spdf) 
      rp.locat <- coordinates(data)
    else if (sf.poly) 
      rp.locat <- st_coordinates(st_centroid(st_geometry(data)))
    else rp.locat <- st_coordinates(st_geometry(data))
    hatmatrix <- T
  }
  else {
    rp.given <- TRUE
    hatmatrix <- F
    if (inherits(regression.points, "Spatial")) {
      rp.locat <- coordinates(regression.points)
    }
    else if (inherits(regression.points, "sf")) {
      if (any((st_geometry_type(regression.points) == 
               "POLYGON")) | any(st_geometry_type(regression.points) == 
                                 "MULTIPOLYGON")) 
        rp.locat <- st_coordinates(st_centroid(st_geometry(regression.points)))
      else rp.locat <- st_coordinates(st_centroid(st_geometry(regression.points)))
    }
    else if (is.numeric(regression.points) && dim(regression.points)[2] == 
             2) 
      rp.locat <- regression.points
    else {
      warning("Output loactions are not packed in a Spatial object,and it has to be a two-column numeric vector")
      rp.locat <- dp.locat
    }
  }
  griddedObj <- F
  if (inherits(regression.points, "Spatial")) {
    if (inherits(regression.points, "SpatialPolygonsDataFrame")) 
      polygons <- polygons(regression.points)
    else griddedObj <- gridded(regression.points)
  }
  if (spdf) {
    p4s <- proj4string(data)
    dp.locat <- coordinates(data)
    data <- as(data, "data.frame")
  }
  else if (inherits(data, "sf")) {
    p4s <- st_crs(data)$proj4string
    if (sf.poly) 
      dp.locat <- st_coordinates(st_centroid(st_geometry(data)))
    else dp.locat <- st_coordinates(st_geometry(data))
  }
  else {
    stop("Given regression data must be a Spatial*DataFrame or sf object")
  }
  mf <- match.call(expand.dots = FALSE)
  m <- match(c("formula", "data"), names(mf), 0L)
  mf <- mf[c(1L, m)]
  mf$drop.unused.levels <- TRUE
  mf[[1L]] <- as.name("model.frame")
  mf <- eval(mf, parent.frame())
  mt <- attr(mf, "terms")
  y <- model.extract(mf, "response")
  x <- model.matrix(mt, mf)
  var.n <- ncol(x)
  rp.n <- nrow(rp.locat)
  dp.n <- nrow(data)
  betas <- matrix(0, nrow = rp.n, ncol = var.n)
  if (hatmatrix) {
    betas.SE <- matrix(0, nrow = rp.n, ncol = var.n)
    betas.TV <- matrix(0, nrow = rp.n, ncol = var.n)
  }
  idx1 <- match("(Intercept)", colnames(x))
  if (!is.na(idx1)) 
    colnames(x)[idx1] <- "Intercept"
  colnames(betas) <- colnames(x)
  lms <- lm(formula, data = data)
  lms$x <- x
  lms$y <- y
  gTSS <- c(cov.wt(matrix(y, ncol = 1), wt = rep(as.numeric(1), 
                                                 dp.n), method = "ML")$cov * dp.n)
  DM.given <- F
  if (missing(dMat)) {
    DM.given <- F
    DM1.given <- F
    if (dp.n + rp.n <= 5000) {
      dMat <- gw.dist(dp.locat = dp.locat, rp.locat = rp.locat, 
                      p = p, theta = theta, longlat = longlat)
      DM.given <- T
    }
    else {
      dMat <- matrix(0, 1, 1)
    }
  }
  else {
    DM.given <- T
    DM1.given <- T
    dim.dMat <- dim(dMat)
    if (dim.dMat[1] != dp.n || dim.dMat[2] != rp.n) 
      stop("Dimensions of dMat are not correct")
  }
  timings[["calibration"]] <- Sys.time()
  s_hat <- c(0, 0)
  q.diag <- matrix(0, 1, dp.n)
  if (parallel.method == F) {
    reg.result <- gw_reg_all(x, y, dp.locat, rp.given, rp.locat, 
                             DM.given, dMat, hatmatrix, p, theta, longlat, bw, 
                             kernel, adaptive)
    betas = betas + reg.result$betas
    if (hatmatrix) {
      betas.SE = reg.result$betas.SE
      s_hat = reg.result$s_hat
      q.diag = reg.result$q.diag
    }
  }
  else if (parallel.method == "cuda") {
    if (missing(parallel.arg)) {
      groupl <- 16
    }
    else {
      groupl <- ifelse(is(parallel.arg, "numeric"), parallel.arg, 
                       16)
    }
    reg.result <- gw_reg_all_cuda(x, y, dp.locat, rp.given, 
                                  rp.locat, DM.given, dMat, hatmatrix, p, theta, longlat, 
                                  bw, kernel, adaptive, groupl)
    if (is(reg.result, "logical") && reg.result == FALSE) {
      stop("Some CUDA errors occured.")
    }
    else {
      betas = betas + reg.result$betas
      if (hatmatrix) {
        betas.SE = reg.result$betas.SE
        s_hat = reg.result$s_hat
        q.diag = reg.result$q.diag
      }
    }
  }
  else if (parallel.method == "omp") {
    if (missing(parallel.arg)) {
      threads <- 0
    }
    else {
      threads <- ifelse(is(parallel.arg, "numeric"), parallel.arg, 
                        0)
    }
    reg.result <- gw_reg_all_omp(x, y, dp.locat, rp.given, 
                                 rp.locat, DM.given, dMat, hatmatrix, p, theta, longlat, 
                                 bw, kernel, adaptive, threads)
    betas = betas + reg.result$betas
    if (hatmatrix) {
      betas.SE = reg.result$betas.SE
      s_hat = reg.result$s_hat
      q.diag = reg.result$q.diag
    }
  }
  else if (parallel.method == "cluster") {
    if (missing(parallel.arg)) {
      parallel.arg.n <- max(detectCores() - 4, 2)
      parallel.arg <- makeCluster(parallel.arg.n)
    }
    else parallel.arg.n <- length(parallel.arg)
    clusterCall(parallel.arg, function() {
      library(GWmodel)
    })
    parallel.arg.results <- clusterApplyLB(parallel.arg, 
                                           1:parallel.arg.n, function(group.i, parallel.arg.n, 
                                                                      x, y, dp.locat, rp.given, rp.locat, DM.given, 
                                                                      dMat, hatmatrix, p, theta, longlat, bw, kernel, 
                                                                      adaptive) {
                                             reg.result <- gw_reg_all(x, y, dp.locat, rp.given, 
                                                                      rp.locat, DM.given, dMat, hatmatrix, p, theta, 
                                                                      longlat, bw, kernel, adaptive, parallel.arg.n, 
                                                                      group.i)
                                             return(reg.result)
                                           }, parallel.arg.n, x, y, dp.locat, rp.given, rp.locat, 
                                           DM.given, dMat, hatmatrix, p, theta, longlat, bw, 
                                           kernel, adaptive)
    for (i in 1:parallel.arg.n) {
      reg.result <- parallel.arg.results[[i]]
      betas = betas + reg.result$betas
      if (hatmatrix) {
        betas.SE = betas.SE + reg.result$betas.SE
        s_hat = s_hat + reg.result$s_hat
        q.diag = q.diag + reg.result$q.diag
      }
    }
    if (missing(parallel.arg)) {
      stopCluster(parallel.arg)
    }
  }
  else {
    for (i in 1:rp.n) {
      if (DM.given) 
        dist.vi <- dMat[, i]
      else {
        if (rp.given) 
          dist.vi <- gw.dist(dp.locat, rp.locat, focus = i, 
                             p, theta, longlat)
        else dist.vi <- gw.dist(dp.locat = dp.locat, 
                                focus = i, p = p, theta = theta, longlat = longlat)
      }
      W.i <- gw.weight(dist.vi, bw, kernel, adaptive)
      if (!is.null(W.vect)) 
        W.i <- W.i * W.vect
      gwsi <- gw_reg(x, y, W.i, hatmatrix, i)
      betas[i, ] <- gwsi[[1]]
      if (hatmatrix) {
        si <- gwsi[[2]]
        Ci <- gwsi[[3]]
        betas.SE[i, ] <- rowSums(Ci * Ci)
        s_hat[1] = s_hat[1] + si[i]
        s_hat[2] = s_hat[2] + sum(si %*% t(si))
        onei <- numeric(rp.n)
        onei[i] = 1
        p_i = onei - si
        q.diag = q.diag + p_i * p_i
      }
    }
  }
  timings[["diagnostic"]] <- Sys.time()
  GW.diagnostic <- NA
  Ftests <- list()
  if (hatmatrix) {
    diags <- gwr_diag1(y, x, betas, as.vector(s_hat))
    tr.S <- s_hat[1]
    tr.StS <- s_hat[2]
    RSS.gw <- diags[5]
    yhat <- gw_fitted(x, betas)
    residual <- y - yhat
    CV <- numeric(dp.n)
    local.R2 <- numeric(dp.n)
    if (cv) 
      CV <- gwr.cv.contrib(bw, x, y, kernel, adaptive, 
                           dp.locat, p, theta, longlat, dMat)
    sigma.hat1 <- RSS.gw/(dp.n - 2 * tr.S + tr.StS)
    Stud_residual <- residual/sqrt(sigma.hat1 * as.vector(q.diag))
    betas.SE <- sqrt(sigma.hat1 * betas.SE)
    betas.TV <- betas/betas.SE
    dybar2 <- (y - mean(y))^2
    dyhat2 <- (y - yhat)^2
    if (DM.given) {
      W <- gw.weight(dMat, bw, kernel, adaptive)
      TSSw <- W %*% dybar2
      RSSw <- W %*% dyhat2
      local.R2 <- (TSSw - RSSw)/TSSw
    }
    else {
      dybar2 <- t(dybar2)
      dyhat2 <- t(dyhat2)
      local.R2 <- gw_local_r2(dp.locat, dybar2, dyhat2, 
                              DM.given, dMat, p, theta, longlat, bw, kernel, 
                              adaptive)
    }
    AIC <- diags[1]
    AICc <- diags[2]
    edf <- diags[3]
    enp <- diags[4]
    gw.R2 <- diags[6]
    gwR2.adj <- diags[7]
    BIC <- diags[8]
    GW.diagnostic <- list(RSS.gw = RSS.gw, AIC = AIC, AICc = AICc, 
                          enp = enp, edf = edf, gw.R2 = gw.R2, gwR2.adj = gwR2.adj, 
                          BIC = BIC)
    Ftests <- list()
    if (F123.test) {
      F.test.parameters <- list(dp.n = dp.n, var.n = var.n, 
                                dMat = dMat, dp.locat = dp.locat, x = x, bw = bw, 
                                adaptive = adaptive, kernel = kernel, betas = betas, 
                                RSS.lm = sum(lms$residuals^2), DF.lm = lms$df.residual, 
                                RSS.gw = RSS.gw, tr.S = tr.S, tr.StS = tr.StS, 
                                q.diag = q.diag, W.vect = W.vect, p = p, theta = theta, 
                                longlat = longlat)
      Ftests <- F1234.test2(F.test.parameters)
    }
  }
  timings[["encapsulate"]] <- Sys.time()
  GW.arguments <- list(formula = formula, rp.given = rp.given, 
                       hatmatrix = hatmatrix, bw = bw, kernel = kernel, adaptive = adaptive, 
                       p = p, theta = theta, longlat = longlat, DM.given = DM1.given, 
                       F123.test = F123.test)
  if (hatmatrix) {
    if (is.null(W.vect)) {
      gwres.df <- data.frame(betas, y, yhat, residual, 
                             CV, Stud_residual, betas.SE, betas.TV, local.R2)
      colnames(gwres.df) <- c(c(c(colnames(betas), c("y", 
                                                     "yhat", "residual", "CV_Score", "Stud_residual")), 
                                paste(colnames(betas), "SE", sep = "_")), paste(colnames(betas), 
                                                                                "TV", sep = "_"), "Local_R2")
    }
    else {
      gwres.df <- data.frame(betas, y, yhat, residual, 
                             CV, Stud_residual, betas.SE, betas.TV, W.vect, 
                             local.R2)
      colnames(gwres.df) <- c(c(c(colnames(betas), c("y", 
                                                     "yhat", "residual", "CV_Score", "Stud_residual")), 
                                paste(colnames(betas), "SE", sep = "_")), paste(colnames(betas), 
                                                                                "TV", sep = "_"), "E_weigts", "Local_R2")
    }
  }
  else {
    if (is.null(W.vect)) 
      gwres.df <- data.frame(betas)
    else {
      gwres.df <- data.frame(betas, W.vect)
      colnames(gwres.df) <- c(colnames(betas), "E_weigts")
    }
  }
  rownames(rp.locat) <- rownames(gwres.df)
  if (inherits(regression.points, "Spatial")) {
    if (inherits(regression.points, "SpatialPolygonsDataFrame")) {
      polygons <- polygons(regression.points)
      rownames(gwres.df) <- sapply(slot(polygons, "polygons"), 
                                   function(i) slot(i, "ID"))
      SDF <- SpatialPolygonsDataFrame(Sr = polygons, data = gwres.df, 
                                      match.ID = F)
    }
    else {
      SDF <- SpatialPointsDataFrame(coords = rp.locat, 
                                    data = gwres.df, proj4string = CRS(p4s), match.ID = F)
      if (griddedObj) 
        gridded(SDF) <- T
    }
  }
  else if (inherits(regression.points, "sf")) {
    SDF <- st_sf(gwres.df, geometry = st_geometry(regression.points))
  }
  else SDF <- SpatialPointsDataFrame(coords = rp.locat, data = gwres.df, 
                                     proj4string = CRS(p4s), match.ID = F)
  timings[["stop"]] <- Sys.time()
  res <- list(GW.arguments = GW.arguments, GW.diagnostic = GW.diagnostic, 
              lm = lms, SDF = SDF, timings = timings, this.call = this.call, 
              Ftests = Ftests)
  class(res) <- "gwrm"
  invisible(res)
}



F1234.test2 <- function (F.test.parameters = list()) 
{
  print("F1234.test")
  F1.test <- F2.test <- F3.test <- F4.test <- NULL
  v1 <- F.test.parameters$tr.S
  v2 <- F.test.parameters$tr.StS
  dp.n <- F.test.parameters$dp.n
  edf <- dp.n - 2 * v1 + v2
  RSSg <- F.test.parameters$RSS.gw
  RSSo <- F.test.parameters$RSS.lm
  DFo <- F.test.parameters$DF.lm
  dMat <- F.test.parameters$dMat
  if (dim(dMat)[1] == 1) {
    DM.given <- F
    dp.locat <- F.test.parameters$dp.locat
    p <- F.test.parameters$p
    theta <- F.test.parameters$theta
    longlat <- F.test.parameters$longlat
  }
  else DM.given <- T
  delta1 <- dp.n - 2 * v1 + v2
  q.diag <- F.test.parameters$q.diag
  sigma2.delta1 <- RSSg/delta1
  sigma2 <- RSSg/dp.n
  odelta2 <- sum(q.diag^2)
  var.n <- F.test.parameters$var.n
  x <- F.test.parameters$x
  transpose_x <- t(x)
  betas <- F.test.parameters$betas
  bw <- F.test.parameters$bw
  adaptive <- F.test.parameters$adaptive
  kernel <- F.test.parameters$kernel
  delta2 <- 0
  L.delta1 <- sum(q.diag)
  L.delta2 <- delta2
  F1 <- (RSSg/L.delta1)/(RSSo/DFo)
  F1.DF <- c(L.delta1^2/L.delta2, DFo)
  F1.pv <- pf(F1, F1.DF[1], F1.DF[2], lower.tail = TRUE)
  F1.test <- matrix(nrow = 1, ncol = 4)
  F1.test[1, 1] <- F1
  F1.test[1, c(2, 3)] <- F1.DF
  F1.test[1, 4] <- F1.pv
  colnames(F1.test) <- c("F1 statistic", "Numerator DF", "Denominator DF", 
                         "Pr(>)")
  rownames(F1.test) <- c(" ")
  F2 <- ((RSSo - RSSg)/(DFo - L.delta1))/(RSSo/DFo)
  F2.DF <- c((DFo - L.delta1)^2/(DFo - 2 * L.delta1 + L.delta2), 
             DFo)
  F2.pv <- pf(F2, F2.DF[1], F2.DF[2], lower.tail = FALSE)
  F2.test <- matrix(nrow = 1, ncol = 4)
  F2.test[1, 1] <- F2
  F2.test[1, c(2, 3)] <- F2.DF
  F2.test[1, 4] <- F2.pv
  colnames(F2.test) <- c("F2 statistic", "Numerator DF", "Denominator DF", 
                         "Pr(>)")
  rownames(F2.test) <- NULL
  ek <- diag(var.n)
  iden <- diag(dp.n)
  J <- matrix(1, nrow = dp.n, ncol = dp.n)
  Vk2 <- numeric(var.n)
  for (i in 1:var.n) {
    Vk2[i] <- (1/dp.n) * (t(betas[, i]) %*% (iden - (1/dp.n) * 
                                               J) %*% betas[, i])
  }
  gamma1 <- numeric(var.n)
  gamma2 <- numeric(var.n)
  numdf <- numeric(var.n)
  F3 <- numeric(var.n)
  F3.pv <- numeric(var.n)
  F3.DF <- matrix(numeric(var.n * 2), ncol = 2)
  cat("Total:", var.n*dp.n)
  t1 <- Sys.time()
  for (i in 1:var.n) {
    B <- matrix(nrow = dp.n, ncol = dp.n)
    for (j in 1:dp.n) {
      if (DM.given) {
        dist.vj <- dMat[, j]
      }
      else {
        dist.vj <- gw.dist(dp.locat, dp.locat, focus = j, 
                           p, theta, longlat)
      }
      wj <- as.numeric(gw.weight(dist.vj, bw, kernel, 
                                 adaptive))
      diag_wj <- diag(wj)
      B[j, ] <- ek[i, ] %*% Matrix:::solve(transpose_x %*% diag_wj %*%x) %*% transpose_x %*% diag_wj
    
      #B[j, ] <- ek[i, ] %*% Matrix:::solve(crossprod(x, diag_wj %*% x)) %*% transpose_x %*% diag_wj
      
      # XtWX <- crossprod(x, diag(wj) %*% x)
      # chol_decomp <- chol(XtWX)
      # inv_XtWX <- chol2inv(chol_decomp)
      # B[j, ] <- ek[i, ] %*% inv_XtWX %*% transpose_x %*% diag_wj
      # A <- crossprod(x, diag_wj %*% x)
      # ev <- eigen(A)
      # print(all(ev$values>0))
    }
    BJ <- (1/dp.n) * (t(B) %*% (iden - (1/dp.n) * J) %*% 
                        B)
    gamma1[i] <- sum(diag(BJ))
    gamma2[i] <- sum(diag(BJ)^2)
    numdf[i] <- gamma1[i]^2/gamma2[i]
    F3[i] <- (Vk2[i]/gamma1[i])/sigma2.delta1
    F3.pv[i] <- pf(F3[i], numdf[i], F1.DF[1], lower.tail = FALSE)
    F3.DF[i, ] <- c(numdf[i], F1.DF[1])
    print(difftime(Sys.time(), t1, units = "mins"))
  }
  
  F3.test <- matrix(nrow = var.n, ncol = 4)
  F3.test[, 1] <- F3
  F3.test[, c(2, 3)] <- F3.DF
  F3.test[, 4] <- F3.pv
  colnames(F3.test) <- c("F3 statistic", "Numerator DF", "Denominator DF", 
                         "Pr(>)")
  rownames(F3.test) <- colnames(x)
  F4 <- RSSg/RSSo
  F4.DF <- c(delta1, DFo)
  F4.pv <- pf(F4, F4.DF[1], F4.DF[2], lower.tail = TRUE)
  F4.test <- matrix(nrow = 1, ncol = 4)
  F4.test[1, 1] <- F4
  F4.test[1, c(2, 3)] <- F4.DF
  F4.test[1, 4] <- F4.pv
  colnames(F4.test) <- c("F4 statistic", "Numerator DF", "Denominator DF", 
                         "Pr(>)")
  rownames(F4.test) <- NULL
  res <- list(F1.test = F1.test, F2.test = F2.test, F3.test = F3.test, 
              F4.test = F4.test)
  res
}

gwr.t.adjust2 <- function(gwm.Obj)
{
  hatmatrix <-gwm.Obj$GW.arguments$hatmatrix
  if(!hatmatrix)
    stop("No p-values to be adjusted")
  
  if(inherits(gwm.Obj$SDF, "Spatial"))
    gwmx <- as.data.frame(gwm.Obj$SDF)
  else
    gwmx <- st_drop_geometry(gwmx <- as.data.frame(gwm.Obj$SDF))
  
  #colnames(gmx)
  n <- dim(gwmx)[1]
  m <- dim(gwmx)[2]
  vnames<-names(gwm.Obj$lm$coefficients)#vnames<-all.vars(gwm.Obj$GW.arguments$formula)
  if(length(vnames)==length(names(gwm.Obj$lm$coefficients))){
    vnames[1]<-"Intercept"
  }
  nv <- length(names(gwm.Obj$lm$coefficients))#length(vnames)
  np <- length(names(gwm.Obj$lm$coefficients))
  ntests <- n * np
  enp <- gwm.Obj$GW.diagnostic$enp
  SDFnms <- names(gwmx)
  idx <- c()
  for(i in 1:m)
  {
    if(grepl("_TV", SDFnms[i]))
      idx <- c(idx, i)
  }
  tvals <- as.matrix(gwmx[, idx])
  pvals <- round(2 * (1 - pt(abs(tvals), enp)), 3)
  pvals

  bey_pvals <- round(p.adjust(pvals, "BY", n = ntests))
  beh_pvals <- round(p.adjust(pvals, "BH", n = ntests))
  bon_pvals <- round(p.adjust(pvals, "bonferroni", n = ntests))
  dim(bey_pvals) <- c(n,nv)
  dim(beh_pvals) <- c(n,nv)
  dim(bon_pvals) <- c(n,nv)
  #print(bey_pvals)
  colnames(tvals) <- paste(vnames, "_t", sep = "")
  colnames(pvals) <- paste(vnames, "_p", sep = "")
  colnames(bey_pvals) <- paste(colnames(pvals), "_by", sep = "")
  colnames(beh_pvals) <- paste(colnames(pvals), "_bh", sep = "")
  colnames(bon_pvals) <- paste(colnames(pvals), "_bo", sep = "")
  asf_pvals <- round(pvals * (1 + enp - (enp/ntests)), 3)
  asf_pvals[asf_pvals > 1] <- 1
  colnames(asf_pvals) <- paste(colnames(pvals), "_fb", sep = "")
  results <- list(t = tvals, p = pvals, by = bey_pvals, fb = asf_pvals,
                  bo = bon_pvals, bh = beh_pvals)
  df.res<-data.frame(tvals, pvals, bey_pvals, asf_pvals,bon_pvals, beh_pvals)
  p4s <- proj4string(gwm.Obj$SDF)
  if(is(gwm.Obj$SDF, "SpatialPolygonsDataFrame"))
    polygons<-polygons(gwm.Obj$SDF)
  else
  {
    locat <- coordinates(gwm.Obj$SDF)
    rownames(locat)<-rownames(df.res)
  }
  griddedObj <- F
  if(inherits(gwm.Obj$SDF, "Spatial"))
  { 
    if (is(gwm.Obj$SDF, "SpatialPolygonsDataFrame"))
    {
      polygons<-polygons(gwm.Obj$SDF)
      #SpatialPolygons(regression.points)
      #rownames(gwres.df) <- sapply(slot(polygons, "polygons"),
      #  function(i) slot(i, "ID"))
      SDF <-SpatialPolygonsDataFrame(Sr=polygons, data=df.res, match.ID=F)
    }
    else
    {
      griddedObj <- gridded(gwm.Obj$SDF)
      SDF <- SpatialPointsDataFrame(coords=locat, data=df.res, proj4string=CRS(p4s), match.ID=F)
      gridded(SDF) <- griddedObj 
    }
  }  else if (inherits(gwm.Obj$SDF, "sf"))
  {
    SDF <- st_sf(df.res, geometry = st_geometry(gwm.Obj$SDF))
  }else{
    SDF <- SpatialPointsDataFrame(coords=locat, data=df.res, proj4string=CRS(p4s), match.ID=F)
  }
  
  #  if (is(gwm.Obj$SDF, "SpatialPolygonsDataFrame"))
  #  {
  #     SDF <-SpatialPolygonsDataFrame(Sr=polygons, data=df.res)
  #  }
  #  else
  #     SDF <- SpatialPointsDataFrame(coords=locat, data=df.res, proj4string=CRS(p4s))
  
  res<-list(results=results, SDF=SDF)
  res
}
