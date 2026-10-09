
# utils_scoring.R
# Calculates the matching score for AWS services.

match_score <- function(r, s, noise_sd = 1.0) {
  
  # Check required columns
  required_r <- c(
    "project_scale", "scalability_need", "low_ops_pref",
    "budget_sensitivity", "latency_need", "availability_need",
    "security_need", "serverless_pref", "realtime_need"
  )
  
  
  # Load the function
  source("R/utils_scoring.R")
  
  # Load your AWS service dataset
  services <- read.csv("data/aws_services.csv")
  
  # Create a sample workload request
  request <- data.frame(
    project_scale = "Small",
    scalability_need = 3,
    low_ops_pref = 4,
    budget_sensitivity = 4,
    latency_need = 3,
    availability_need = 4,
    security_need = 5,
    serverless_pref = 1,
    realtime_need = 0
  )
  
  # Calculate scores
  scores <- match_score(request, services, noise_sd = 0)
  
  # Display results
  print(scores)
  
  # Show services ranked by score
  result <- data.frame(
    service = services$service,
    score = scores
  )
  
  result <- result[order(-result$score), ]
  print(result)
  
  
  required_s <- c(
    "scalability", "ops_burden", "cost_small", "cost_large",
    "latency", "availability", "security", "serverless", "realtime"
  )
  
  if (!all(required_r %in% names(r))) {
    stop("Request data is missing required columns.")
  }
  
  if (!all(required_s %in% names(s))) {
    stop("Service data is missing required columns.")
  }
  
  if (nrow(r) != 1) {
    stop("Request must contain exactly one row.")
  }
  
  if (nrow(s) == 0) {
    stop("Service dataset is empty.")
  }
  
  if (length(noise_sd) != 1 ||
      !is.finite(noise_sd) || noise_sd < 0) {
    stop("noise_sd must be a non-negative number.")
  }
  
  # Select project cost
  if (as.character(r$project_scale[1]) == "Small") {
    cost <- s$cost_small
  } else {
    cost <- s$cost_large
  }
  
  # Calculate matching scores
  score <-
    -0.8 * abs(r$scalability_need[1] - s$scalability) -
    0.9 * abs(r$low_ops_pref[1] - (6 - s$ops_burden)) -
    0.9 * abs(r$budget_sensitivity[1] - cost) -
    0.7 * abs(r$latency_need[1] - s$latency) -
    0.7 * abs(r$availability_need[1] - s$availability) -
    0.6 * abs(r$security_need[1] - s$security) +
    1.5 * (r$serverless_pref[1] == s$serverless) +
    1.5 * (r$realtime_need[1] == 1 & s$realtime == 1)
  
  # Add random noise if required
  if (noise_sd > 0) {
    score <- score + rnorm(nrow(s), 0, noise_sd)
  }
  
  return(as.numeric(score))
}

