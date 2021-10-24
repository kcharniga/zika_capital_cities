# Getting priors distributions of ZIKV infection attack rate for cities with 
# seroprevalence data using method of moments

# Sincelejo
mu <- 0.659
alpha <- 0.62
gamma <- 0.696
ci <- c(alpha, gamma)
f <- function(par) {
  a <- exp(par['a'])
  sum((pbeta(ci, a, (1-mu)/mu * a) - c(0.025, 0.975))^2)
}
opt <- optim(par = list(a = 1), fn = f)
x <- seq(0,1,0.001)
a <- exp(opt$par['a'])
b <- (1-mu)/mu * a
print(cbind(data=c(mu, alpha, gamma),
            pred = c(a/(a+b),
                     qbeta(c(0.025, 0.975), a, b))))


# Neiva
mu <- 0.578
alpha <- 0.538
gamma <- 0.618
ci <- c(alpha, gamma)
f <- function(par) {
  a <- exp(par['a'])
  sum((pbeta(ci, a, (1-mu)/mu * a) - c(0.025, 0.975))^2)
}
opt <- optim(par = list(a = 1), fn = f)
x <- seq(0,1,0.001)
a <- exp(opt$par['a'])
b <- (1-mu)/mu * a
print(cbind(data=c(mu, alpha, gamma),
            pred = c(a/(a+b),
                     qbeta(c(0.025, 0.975), a, b))))

# Cucuta
mu <- 0.479
alpha <- 0.44
gamma <- 0.519
ci <- c(alpha, gamma)
f <- function(par) {
  a <- exp(par['a'])
  sum((pbeta(ci, a, (1-mu)/mu * a) - c(0.025, 0.975))^2)
}
opt <- optim(par = list(a = 1), fn = f)
x <- seq(0,1,0.001)
a <- exp(opt$par['a'])
b <- (1-mu)/mu * a
print(cbind(data=c(mu, alpha, gamma),
            pred = c(a/(a+b),
                     qbeta(c(0.025, 0.975), a, b))))

# Medellin
mu <- 0.067
alpha <- 0.048
gamma <- 0.09
ci <- c(alpha, gamma)
f <- function(par) {
  a <- exp(par['a'])
  sum((pbeta(ci, a, (1-mu)/mu * a) - c(0.025, 0.975))^2)
}
opt <- optim(par = list(a = 1), fn = f)
x <- seq(0,1,0.001)
a <- exp(opt$par['a'])
b <- (1-mu)/mu * a
print(cbind(data=c(mu, alpha, gamma),
            pred = c(a/(a+b),
                     qbeta(c(0.025, 0.975), a, b))))

