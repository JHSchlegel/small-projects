library(ggplot2)
library(svglite)
library(rpapers) # my own package for theme
set_paper_plot_specs()

# install.packages("TMB")
library(TMB) # use TMB to massively speed up MLE 
data(UKDriverDeaths)
drivers.deaths <- as.numeric(UKDriverDeaths)
head(drivers.deaths)


# R Version of the MLE ---------------------------------------------
# Define the log-likelihood function
neglogL <- function(params, data) {
  mu <- params[1]
  sigma <- params[2]
  n <- length(data)
  logL <- -n*log(sigma) - sum((data - mu)^2)/(2*sigma^2)
  return(-logL)
}

# optimize the log-likelihood function
res <- optim(
    par=c(100, 10), 
    fn=neglogL, 
    data=drivers.deaths, 
    method="L-BFGS-B",
    lower=c(0, 0.01)
)

# save the optimized parameters
opt_pars <- res$par
names(opt_pars) <- c("mu", "sigma")



# TMB Version of the MLE ---------------------------------------------
## compare cpp to the MLE from the built-in R function
# compile the cpp code
compile("drivers_mle.cpp")
dyn.load(dynlib("drivers_mle"))

# instantiate the neglogL function
neglogL_cpp <- MakeADFun(
    data=list(x=drivers.deaths),
    parameters=list(mu=100, sigma=10),
    DLL="drivers_mle"
)


# optimize the log-likelihood function
res_cpp <- optim(
    par=c(100, 10), 
    fn=neglogL_cpp$fn, 
    gr=neglogL_cpp$gr, 
    method="L-BFGS-B",
    lower=c(0, 0.01)
)

# save the optimized parameters
opt_pars_cpp <- res_cpp$par
names(opt_pars_cpp) <- c("mu", "sigma")

# Plot the results ---------------------------------------------------
## Histogram of the data
hist(
    drivers.deaths, breaks=20, freq=FALSE, main="UK Driver Deaths", 
    prob = TRUE, col = "skyblue"
)
# Add the fitted normal distribution
curve(dnorm(x, mean=opt_pars["mu"], sd=opt_pars["sigma"]), 
      col="red", lwd=2, add=TRUE)



# create a sequence of values for the x-axis
mu <- seq(1400, 2000, length.out=1000)
sigma <- seq(200, 400, length.out=1000)


# Benchmarking -------------------------------------------------------
library(microbenchmark)
# benchmark R and TMB versions of the MLE
# bench <- microbenchmark(
#     R_MLE = optim(
#         par=c(100, 10), 
#         fn=neglogL, 
#         data=drivers.deaths, 
#         method="L-BFGS-B",
#         lower=c(0, 0.01)
#     ),
#     TMB_MLE = optim(
#         par=c(100, 10), 
#         fn=neglogL_cpp$fn, 
#         gr=neglogL_cpp$gr, 
#         method="L-BFGS-B",
#         lower=c(0, 0.01)
#     ),
#     times = 1000L
# )

bench <- microbenchmark(
    R_MLE = nlminb(
        start=c(100, 10), 
        objective=neglogL, 
        data=drivers.deaths, 
        lower=c(0, 0.01)
    ),
    TMB_MLE = nlminb(
        start = neglogL_cpp$par, 
        objective=neglogL_cpp$fn, 
        gradient=neglogL_cpp$gr, 
        lower=c(0, 0.01)
    ),
)

# get summary
summary(bench)

# create data frame of benchmark results
bench_df <- data.frame(
    expr = bench$expr,
    time = bench$time
)

# plot the results
p_bench <- ggplot(bench_df, aes(x=expr, y=time, fill = expr, color = expr)) + 
    geom_violin() + 
    geom_boxplot(width=0.1, color="black", alpha=0.2) +
    scale_fill_paper() +
    scale_color_paper() +
    labs(title="Benchmarking MLE", y="Time (ms)", x="Method")

ggsave("benchmark_MLE.pdf", plot = p_bench, width=6, height=4, dpi=300)

# we see that TMB was slower; most likely due to the low number of samples