# --- sample 1 ---
# Define the objective function (e.g., Rosenbrock function)
objective_function <- function(x) {
  (1 - x[1])^2 + 100 * (x[2] - x[1]^2)^2
}

# Initial guess for the parameters
initial_parameters <- c(-1.2, 1)

# Perform optimization
result <- optim(par = initial_parameters, fn = objective_function, method = "BFGS")
# Perform optimization
result_2 <- optim(par = initial_parameters, fn = objective_function)

# Print the results
print(result)
print(result_2)
