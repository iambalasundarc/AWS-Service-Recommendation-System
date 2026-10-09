# 01_generate_data.R -------------------------------------------------------
# Builds the workload-request dataset used to train/evaluate the recommender.
# The service catalogue (data/aws_services.csv) is hand-curated from AWS docs.
# Requests are SIMULATED: each request has requirement levels (1-5); the
# "chosen" service is the best match in that workload type plus random noise
# (to mimic real architects not always agreeing on a single best answer).
set.seed(2026)
services <- read.csv("data/aws_services.csv", stringsAsFactors = FALSE)
types <- unique(services$workload_type)
n <- 3000
clip <- function(x) pmin(5, pmax(1, round(x)))

workload_type <- sample(types, n, TRUE,
  prob = c(.14,.10,.08,.12,.10,.08,.09,.06,.07,.06,.05,.05))
project_scale   <- sample(c("Small","Medium","Large"), n, TRUE, c(.45,.35,.20))
team_experience <- sample(c("Beginner","Intermediate","Expert"), n, TRUE, c(.35,.40,.25))

scalability_need   <- clip(rnorm(n, c(Small=2.3, Medium=3.3, Large=4.3)[project_scale], 1))
low_ops_pref       <- clip(rnorm(n, c(Beginner=4.2, Intermediate=3.1, Expert=2.2)[team_experience], 1))
budget_sensitivity <- clip(rnorm(n, c(Small=4.0, Medium=3.0, Large=2.3)[project_scale], 1))
latency_need       <- clip(rnorm(n, 3.3, 1.2))
availability_need  <- clip(rnorm(n, c(Small=3.0, Medium=3.6, Large=4.3)[project_scale], 1))
security_need      <- clip(rnorm(n, 3.6, 1.1))
serverless_pref    <- rbinom(n, 1, c(Beginner=.55, Intermediate=.40, Expert=.30)[team_experience])
realtime_need      <- rbinom(n, 1, .30)

req <- data.frame(request_id = seq_len(n), workload_type, project_scale, team_experience,
                  scalability_need, low_ops_pref, budget_sensitivity, latency_need,
                  availability_need, security_need, serverless_pref, realtime_need,
                  stringsAsFactors = FALSE)

source("R/utils_scoring.R")   # match_score(): requirement-vs-service fit (+ noise)
req$recommended_service <- vapply(seq_len(n), function(i) {
  s <- services[services$workload_type == req$workload_type[i], ]
  s$service[which.max(match_score(req[i, ], s))]
}, character(1))

write.csv(req, "data/workload_requests.csv", row.names = FALSE)
cat("Generated", nrow(req), "requests ->", "data/workload_requests.csv\n")
