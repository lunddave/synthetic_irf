library(quantmod)
library(garchx)
library(lmtest)
library(extraDistr)
library(gnorm)
library(tsDyn)
library(Rsolnp)
library(RColorBrewer)
library(DescTools)
library(forecast)
library(LaplacesDemon)

options(scipen = 7)

    
n <- 100
p <- 2
sigma_x <- 1

# The vector innovations for the VAR
innovations_matrix_entries <- rnorm(n * p, sd = sigma_x)

# The VAR mapping matrix
param_matrix_entries <- runif(p**2, min = -1/p, max = 1/p)
simVAR_params <- matrix(param_matrix_entries, nrow = p, byrow = T)

# Now create SIGMA for VAR
SIGMA_mat <- matrix(c(.5,.2,.4,.7) , nrow = 2)

VAR_process <- VAR.sim(B = simVAR_params, 
                       lag = 1, 
                       include = "none", 
                       n = n, # we do not need more data points than this
                       #innov = sim_VAR_innovations,
                       varcov = SIGMA_mat)

#Let's give this simulated data column names
colnames(VAR_process) <- c('delta_oil', 'delta_GDP')

plot.ts(VAR_process)

# Can we view cholesky decomposition of SIGMA of VAR ?
t(chol(SIGMA_mat))

# First we try IRFs using...

# Load package
library(vars)

# Estimate model
model <- VAR(VAR_process, p = 1, type = "const")

# Look at summary statistics
summary(model)

feir <- irf(model, impulse = "delta_oil", response = "delta_GDP",
            n.ahead = 8, ortho = FALSE, runs = 1000)

plot(feir)

# NOW we do IRFs using local projection
####### Local projections
# https://cran.r-project.org/web/packages/lpirfs/lpirfs.pdf

# Load package
library(lpirfs)

# Estimate linear model
results_lin <- lp_lin(VAR_process_df,
                      lags_endog_lin = 1,
                      trend = 0,
                      shock_type = 1,
                      confint = 1.96,
                      hor = 8)
# Show all impulse responses
# Compare with Figure 5 in Jordà (2005)
plot(results_lin)
# Make individual plots
linear_plots <- plot_lin(results_lin)
# Show single plots
# * The first element of 'linear_plots' shows the response of the first
# variable (GDP_gap) to a shock in the first variable (GDP_gap).
# * The second element of 'linear_plots' shows the response of the first
# variable (GDP_gap) to a shock in the second variable (inflation).
# * ...

png(file="localprojections.png", width=800, height=800)
plot(results_lin)
dev.off()

linear_plots[[1]]
linear_plots[[2]]
linear_plots[[3]]
linear_plots[[4]]
linear_plots[[5]]
linear_plots[[6]]
linear_plots[[7]]
linear_plots[[8]]
linear_plots[[9]]
linear_plots[[10]]
linear_plots[[11]]
linear_plots[[12]]
linear_plots[[13]]
linear_plots[[14]]
linear_plots[[15]]
linear_plots[[16]]
linear_plots[[17]]
linear_plots[[18]]

for (k in 1:ncol(difflog_df)){
  linear_plots[[k]]
}

# Show diagnostics. The first element corresponds to the first shock variable.
summary(results_lin)
## E
