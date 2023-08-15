library(ggplot2)
library(microbenchmark)
library(tidyverse)
library(rpapers) # my own package for theme
library(plotly)


# install.packages("TMB")
library(TMB) # use TMB to massively speed up MLE 
data(UKDriverDeaths)
set.seed(123)
# idx <- sample(1:length(UKDriverDeaths), 1E6, replace = TRUE)
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
print(opt_pars)


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
print(opt_pars_cpp)


# Plot the results ---------------------------------------------------
## Histogram of the data
hist(
    drivers.deaths, breaks=20, freq=FALSE, main="UK Driver Deaths", 
    prob = TRUE, col = "skyblue"
)
# Add the fitted normal distribution
curve(dnorm(x, mean=opt_pars["mu"], sd=opt_pars["sigma"]), 
      col="red", lwd=2, add=TRUE)

## Plotting the log-likelihood surface
# create a sequence of values for the x-axis
mu <- seq(1400, 2000, length.out=500)
sigma <- seq(200, 400, length.out=500)
grid <- expand_grid(mu=mu, sigma=sigma)

z <- apply(grid, 1, neglogL, data=drivers.deaths)
grid$z <- z

# manually find the optimal parameters
idx <- which.min(z)
grid[idx, ]

# plot the log-likelihood surface
p <- ggplot(grid, aes(x=mu, y=sigma, z=z)) + 
    geom_contour_filled(bins = 20) + 
    geom_point(aes(x=opt_pars["mu"], y=opt_pars["sigma"]), color="red", size=3) +
    labs(title="Log-Likelihood Surface", x="mu", y="sigma")

# save the plot
ggsave("log_likelihood_surface.pdf", plot = p, width=6, height=4, dpi=300)


## Interactive plotly plots
# Contour plot
plot_ly(grid, x = ~mu, y = ~sigma, z = ~z, type = "contour", 
        contours = list(
            coloring = "heatmap",
            showlabels = TRUE,
            labelfont = list(
                family = "Raleway",
                size = 12,
                color = "white"
            )
        )
    )

# plot the log-likelihood surface in 3D
plot_ly(z = ~xtabs(z ~ mu + sigma, data = grid)) %>% add_surface(
        colors = c("#132B43", "#56B1F7", "#FFFFFF", "#E6842A", "#132B43"),
        colorbar = list(
            title = "Log-Likelihood",
            titleside = "top",
            tickmode = "array",
            tickvals = c(-1000, -500, 0, 500, 1000),
            ticktext = c("-1000", "-500", "0", "500", "1000"),
            ticks = "outside"
        ),
        lighting = list(
            ambient = 0.95,
            diffuse = 0.99,
            specular = 0.99,
            roughness = 0.99
        )
        # hoverinfo = "text",
        # hovertext = ~paste0("mu: ", mu, "<br>sigma: ", sigma, "<br>z: ", z)
) %>% 
    layout(
        scene = list(
            xaxis = list(title = "mu"),
            yaxis = list(title = "sigma"),
            zaxis = list(title = "Log-Likelihood")
        )
    )

# Benchmarking -------------------------------------------------------
set_paper_plot_specs()
# benchmark R and TMB versions of the MLE
bench <- microbenchmark(
    R_MLE = optim(
        par=c(1000, 100), 
        fn=neglogL, 
        data=drivers.deaths, 
        method="L-BFGS-B",
        lower=c(0, 0.01)
    ),
    TMB_MLE = optim(
        par=c(1000, 100), 
        fn=neglogL_cpp$fn, 
        gr=neglogL_cpp$gr, 
        method="L-BFGS-B",
        lower=c(0, 0.01)
    ),
    times = 100L
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

# for low sample sizes,  TMB version performs worse than
# the base R version; when using very large sample sizes, it performs considerably
# better than the base R version
