# we use a meta-analytical based method combining within and between-
# participants variability (see Valentine & Pigott, 2010).
# 
# Valentine, J. C., Pigott, T. D., & Rothstein, H. R. (2010). How Many Studies Do You Need?: A Primer on Statistical Power for Meta-Analysis. Journal of Educational and Behavioral Statistics: A Quarterly Publication Sponsored by the American Educational Research Association and the American Statistical Association, 35(2), 215–247. https://doi.org/10.3102/1076998609346961

nt <- 75  # number of trials
p <- 0.58 # accuracy (effect size)
s <- 0.5  # by-participant standard deviation (logit scale)
tpower <- 0.8 # desired power level

# the s value has been chosen for being a reasonable value considering
# the subliminal task and the expected true accuracy being equal or 
# greater than 0.5 (chance level)

# expected range of true proportions with s = 0.5
# higher s values would assume unrealist true accuracies

hist(plogis(qlogis(p) + rnorm(1e4, 0, s)))

power_p <- function(p, nt, ns, s, alpha = 0.05){
    # standard error of a proportion in logit scale
    # see Agresti(2012, pp. 73)
    se_p <- sqrt((1 / (nt * p * (1 - p)) + s^2) / ns)
    
    # critical z, one-tail
    zc <- abs(qnorm(alpha))
    
    # observed z
    z <- qlogis(p) / se_p
    
    # power
    1 - pnorm(zc - z) + pnorm(-zc - z)
}

# vector of sample sizes
ns <- seq(1, 100, 1)

# calculate the power for each sample size 
power <- sapply(ns, function(x) power_p(p = p, nt = nt, x, s = s))

# find the closest sample size to 80% power
target_n <- ns[which.min(abs(power - 0.8))]

# target_n

# we use a meta-analytical based method combining within and between-
# participants variability (see Valentine & Pigott, 2010).
# 
# Valentine, J. C., Pigott, T. D., & Rothstein, H. R. (2010). How Many Studies Do You Need?: A Primer on Statistical Power for Meta-Analysis. Journal of Educational and Behavioral Statistics: A Quarterly Publication Sponsored by the American Educational Research Association and the American Statistical Association, 35(2), 215–247. https://doi.org/10.3102/1076998609346961

nt <- 75  # number of trials
p <- 0.58 # accuracy (effect size)
s <- 0.5  # by-participant standard deviation (logit scale)

# the s value has been chosen for being a reasonable value considering
# the subliminal task and the expected true accuracy being equal or 
# greater than 0.5 (chance level)

# expected range of true proportions with s = 0.5
# higher s values would assume unrealist true accuracies

hist(plogis(qlogis(p) + rnorm(1e4, 0, s)))

power_p <- function(p, nt, ns, s, alpha = 0.05){
    # standard error of a proportion in logit scale
    # see Agresti(2012, pp. 73)
    se_p <- sqrt((1 / (nt * p * (1 - p)) + s^2) / ns)
    
    # critical z, one-tail
    zc <- abs(qnorm(alpha))
    
    # observed z
    z <- qlogis(p) / se_p
    
    # power
    1 - pnorm(zc - z) + pnorm(-zc - z)
}

# vector of sample sizes
ns <- seq(1, 100, 1)

# calculate the power for each sample size 
power <- sapply(ns, function(x) power_p(p = p, nt = nt, x, s = s))

# find the closest sample size to 80% power

idx <- which.min(abs(power - tpower))

res_n <- ns[idx]
res_power <- power[idx]

# res_n
# res_power

title <- latex2exp::TeX("Trials = 75, $\\sigma_{\\;id} = 0.5$, $p = 0.58$")

plot(ns, power, type = "l", ylim = c(0, 1), xlab = "Sample Size", ylab = "Power",
     main = title)
segments(res_n, 0, res_n, res_power, lty = "dashed")
segments(0, res_power, res_n, res_power, lty = "dashed")
points(res_n, res_power, pch = 19, cex = 1.5, col = "firebrick")
