rNumbCopula <- function(){

pdf(file="R-Project1, Exercise2,3")

#1. Start with normal copula

myCop.norm <- ellipCopula(family="normal", dim=3, dispstr="ex", param=0.4)
u.n <- rcopula(myCop.norm, 500)
scatterplot3d(u.n, main="500 obs. generated for 3-D normal copula. Var = 1 & corr = 0.4")
pearson.norm <- cor(u.n)[1, 2]
kendall.norm <- cor(u.n, method="kendall")[1, 2]
spearman.norm <- cor(u.n, method="spearman")[1, 2]
th.p.norm <- 0.4
th.k.norm <- (2/pi)*asin(0.4)
th.s.norm <- (6/pi)*asin(0.4/2)

#Print the results to main window (WILL NOT SAVE IN THE PDF FILE)
cat("NORMAL COPULA \n	Pearson's rho: \n	Empirical:", pearson.norm, "\n	Theoretical:", th.p.norm, "\n\n	Kendall's tau: \n	Empirical:", kendall.norm, "\n	Theoretical:", th.k.norm, "\n\n	Spearman's rho: \n	Empirical:", spearman.norm, "\n	Theoretical:", th.s.norm, "\n\n\n" )


#2. Bivariate t copula

myCop.t1 <- ellipCopula(family="t", dim=3, dispstr="ex", param=-0.5, df=8)
u.t1 <- rcopula(myCop.t1, 500)
myCop.t2 <- ellipCopula(family="t", dim=3, dispstr="ex", param=-0.2, df=8)
u.t2 <- rcopula(myCop.t2, 500)
myCop.t3 <- ellipCopula(family="t", dim=3, dispstr="ex", param=0, df=8)
u.t3 <- rcopula(myCop.t3, 500)
myCop.t4 <- ellipCopula(family="t", dim=3, dispstr="ex", param=0.2, df=8)
u.t4 <- rcopula(myCop.t4, 500)
myCop.t5 <- ellipCopula(family="t", dim=3, dispstr="ex", param=0.5, df=8)
u.t5 <- rcopula(myCop.t5, 500)

par(mfrow=c(3, 2),mar=c(2, 2, 1, 1), oma=c(1, 1, 0, 0), mgp=c(2, 1, 0))
scatterplot3d(u.t1, main="3D scatterplot for t-distr. copula with param -0.5")
scatterplot3d(u.t2, main="3D scatterplot for t-distr. copula with param -0.2")
scatterplot3d(u.t3, main="3D scatterplot for t-distr. copula with param 0")
scatterplot3d(u.t4, main="3D scatterplot for t-distr. copula with param 0.2")
scatterplot3d(u.t5, main="3D scatterplot for t-distr. copula with param 0.5")
#compare 2D-plot
plot(rcopula(myCop.t1, 500), main="2D plot for t-distr. copula with param -0.5")

#COMPUTE DEPENDENCY FOR t4, PARAM = 0.2
pearson.t <- cor(u.t4)[1, 2]
kendall.t <- cor(u.t4, method="kendall")[1, 2]
spearman.t <- cor(u.t4, method="spearman")[1, 2]
th.p.t <- 0.2
th.k.t <- (2/pi)*asin(0.2)
th.s.t <- (6/pi)*asin(0.2/2)

#Print the results to main window (WILL NOT SAVE IN THE PDF FILE)
cat("t4 COPULA, PARAM = 0.2 \n	Pearson's rho: \n	Empirical:", pearson.t, "\n	Theoretical:", th.p.t, "\n\n	Kendall's tau: \n	Empirical:", kendall.t, "\n	Theoretical:", th.k.t, "\n\n	Spearman's rho: \n	Empirical:", spearman.t, "\n	Theoretical:", th.s.t, "\n\n\n" )


#3. Frank copula
myCop.frank <- archmCopula(family="frank", dim=3, param=3)
u.f <- rcopula(myCop.frank, 500)
param.f <- 3
pearson.f <- cor(u.f)[1, 2]
kendall.f <- cor(u.f, method="kendall")[1, 2]
spearman.f <- cor(u.f, method="spearman")[1, 2]
th.k.f <- 0.3

cat("FRANK COPULA \n	Pearson's rho: \n	Empirical:", pearson.f, "\n\n	Kendall's tau: \n	Empirical:", kendall.f, "\n	Theoretical:", th.k.f, "\n\n	Spearman's rho: \n	Empirical:", spearman.f, "\n\n\n" )


#4. Clayton copula
myCop.clayton <- archmCopula(family="clayton", dim=2, param=2)
u.c <- rcopula(myCop.clayton, 500)
param.c <- 2
pearson.c <- cor(u.c)[1, 2]
kendall.c <- cor(u.c, method="kendall")[1, 2]
spearman.c <- cor(u.c, method="spearman")[1, 2]
th.k.c <- param.c/(param.c+2)

cat("CLAYTON COPULA \n	Pearson's rho: \n	Empirical:", pearson.c, "\n\n	Kendall's tau: \n	Empirical:", kendall.c, "\n	Theoretical:", th.k.c, "\n\n	Spearman's rho: \n	Empirical:", spearman.c, "\n\n\n" )


#5. Gumbel copula
myCop.gumbel <- archmCopula(family="gumbel", dim=2, param=5)
u.g <- rcopula(myCop.gumbel, 500)
param.g <- 5
pearson.g <- cor(u.g)[1, 2]
kendall.g <- cor(u.g, method="kendall")[1, 2]
spearman.g <- cor(u.g, method="spearman")[1, 2]
th.k.g <- 1-(1/param.g)

cat("GUMBEL COPULA \n	Pearson's rho: \n	Empirical:", pearson.g, "\n\n	Kendall's tau: \n	Empirical:", kendall.g, "\n	Theoretical:", th.k.g, "\n\n	Spearman's rho: \n	Empirical:", spearman.g, "\n\n\n" )



#EXERCISE 3: GRAPHICS FOR COPULAS
#For each data set plot the pdf and the cdf of each bivariate copulas.
par(mfrow=c(5, 2),mar=c(2, 2, 1, 1), oma=c(1, 1, 0, 0), mgp=c(2, 1, 0))
plot(ecdf(u.t1), main="cdf for t-distr. copula with param -0.5")
plot(density(u.t1), main="pdf for t-distr. copula with param -0.5")
plot(ecdf(u.t2), main="cdf for t-distr. copula with param -0.2")
plot(density(u.t2), main="pdf for t-distr. copula with param -0.2")
plot(ecdf(u.t3), main="cdf for t-distr. copula with param 0")
plot(density(u.t3), main="pdf for t-distr. copula with param 0")
plot(ecdf(u.t4), main="cdf for t-distr. copula with param 0.2")
plot(density(u.t4), main="pdf for t-distr. copula with param 0.2")
plot(ecdf(u.t5), main="cdf for t-distr. copula with param 0.5")
plot(density(u.t5), main="pdf for t-distr. copula with param 0.5")


par(mfrow=c(4, 2),mar=c(2, 2, 1, 1), oma=c(1, 1, 0, 0), mgp=c(2, 1, 0))
plot(ecdf(u.n), main="cdf for normal copula with parameter 0.4")
plot(density(u.n), main="pdf for normal copula with parameter 0.4")
plot(ecdf(u.f), main="cdf for Frank copula with parameter 3")
plot(density(u.f), main="pdf for Frank copula with parameter 3")
plot(ecdf(u.c), main="cdf for Clayton copula with parameter 2")
plot(density(u.c), main="pdf for Clayton copula with parameter 2")
plot(ecdf(u.g), main="cdf for Gumbel copula with parameter 5")
plot(density(u.g), main="pdf for normal copula with parameter 5")


dev.off()
}