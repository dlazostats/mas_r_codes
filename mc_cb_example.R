############################################################
# MONTE CARLO SIMULATION (Crystal Ball equivalent)
############################################################

# Install package if needed
# install.packages("triangle")

library(triangle)
library(ggplot2)

set.seed(123)

############################################################
# SIMULATION SETTINGS
############################################################

n_sim <- 10000

############################################################
# ASSUMPTION CELLS
############################################################

# Demand ~ Normal(10000, 1500)
demand <- rnorm(
  n = n_sim,
  mean = 10000,
  sd = 1500
)

# Selling Price ~ Triangular(18, 20, 25)
price <- rtriangle(
  n = n_sim,
  a = 18,
  b = 25,
  c = 20
)

# Unit Cost ~ Normal(12, 1)
cost <- rnorm(
  n = n_sim,
  mean = 12,
  sd = 1
)

# Fixed Cost
fixed_cost <- 50000

############################################################
# FORECAST CELL
############################################################

profit <- demand * (price - cost) - fixed_cost

############################################################
# SUMMARY STATISTICS
############################################################
{
  cat("=================================\n")
  cat(" MONTE CARLO PROFIT SIMULATION\n")
  cat("=================================\n\n")
  
  cat("Mean Profit:\n")
  print(mean(profit))
  
  cat("\nStandard Deviation:\n")
  print(sd(profit))
  
  cat("\nMinimum:\n")
  print(min(profit))
  
  cat("\nMaximum:\n")
  print(max(profit))
  
  cat("\nPercentiles:\n")
  print(
    quantile(
      profit,
      probs = c(0.01, 0.05, 0.25, 0.50, 0.75, 0.95, 0.99)
    )
  )
}

############################################################
# PROBABILITY OF LOSS
############################################################

prob_loss <- mean(profit < 0)
{
  cat("\nProbability of Loss:\n")
  print(prob_loss)  
}


############################################################
# CERTAINTY RANGES
############################################################

{
  cat("\nProbability Profit > 25000:\n")
  print(mean(profit > 25000))
}
{
  cat("\nProbability Profit > 50000:\n")
  print(mean(profit > 50000))
}

############################################################
# HISTOGRAM (Crystal Ball Forecast Chart)
############################################################

hist(
  profit,
  breaks = 50,
  col = "lightblue",
  border = "white",
  main = "Monte Carlo Simulation of Profit",
  xlab = "Profit"
)

abline(
  v = mean(profit),
  col = "red",
  lwd = 3
)

############################################################
# DENSITY PLOT
############################################################

ggplot(
  data.frame(profit),
  aes(profit)
) +
  geom_density(fill = "skyblue", alpha = .5) +
  geom_vline(
    xintercept = mean(profit),
    color = "red",
    linewidth = 1
  ) +
  labs(
    title = "Profit Distribution",
    x = "Profit",
    y = "Density"
  ) +
  theme_minimal()

############################################################
# INPUTS + OUTPUT DATASET
############################################################

sim_results <- data.frame(
  Demand = demand,
  Price = price,
  Cost = cost,
  Profit = profit
)

head(sim_results)

############################################################
# SENSITIVITY ANALYSIS
# (Crystal Ball Tornado approximation)
############################################################

sensitivity <- data.frame(
  Variable = c("Demand", "Price", "Cost"),
  Correlation = c(
    cor(demand, profit),
    cor(price, profit),
    cor(cost, profit)
  )
)

print(sensitivity)

############################################################
# TORNADO CHART
############################################################

ggplot(
  sensitivity,
  aes(
    x = reorder(
      Variable,
      abs(Correlation)
    ),
    y = Correlation,
    fill = Correlation > 0
  )
) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Sensitivity Analysis",
    x = "",
    y = "Correlation with Profit"
  ) +
  theme_minimal() +
  guides(fill = "none")

############################################################
# SAVE RESULTS
############################################################

write.csv(
  sim_results,
  "simulation_results.csv",
  row.names = FALSE
)

cat("\nResults saved to simulation_results.csv\n")