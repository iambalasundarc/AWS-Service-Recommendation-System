# 04_recommend_demo.R ------------------------------------------------------
suppressPackageStartupMessages(library(ranger))
obj <- readRDS("outputs/rf_model.rds")
recommend_aws <- function(workload_type, project_scale, team_experience,
                          scalability_need = 3, low_ops_pref = 3, budget_sensitivity = 3,
                          latency_need = 3, availability_need = 3, security_need = 3,
                          serverless_pref = 0, realtime_need = 0, top_n = 3) {
  nd <- data.frame(
    workload_type   = factor(workload_type,   levels = obj$lv$workload_type),
    project_scale   = factor(project_scale,   levels = obj$lv$project_scale),
    team_experience = factor(team_experience, levels = obj$lv$team_experience),
    scalability_need, low_ops_pref, budget_sensitivity, latency_need,
    availability_need, security_need, serverless_pref, realtime_need)
  p <- predict(obj$rf, nd)$predictions[1, ]
  p <- p[names(p)[obj$svc_type[match(names(p), obj$cls)] == workload_type]]   # only services of this workload type
  p <- p / sum(p)
  out <- head(sort(p, decreasing = TRUE), top_n)
  data.frame(rank = seq_along(out), service = names(out), confidence = round(as.numeric(out), 3))
}
cat("\n=== Demo 1: Small startup, beginner team, hosting a website on a tight budget ===\n")
print(recommend_aws("Web & App Hosting", "Small", "Beginner", scalability_need = 2,
                    low_ops_pref = 5, budget_sensitivity = 5, availability_need = 3))
cat("\n=== Demo 2: Large company, expert team, real-time event processing ===\n")
print(recommend_aws("Analytics", "Large", "Expert", scalability_need = 5, low_ops_pref = 2,
                    budget_sensitivity = 2, latency_need = 5, availability_need = 5, realtime_need = 1))
cat("\n=== Demo 3: Medium team, intermediate, relational database with high security ===\n")
print(recommend_aws("Relational Database", "Medium", "Intermediate", scalability_need = 4,
                    security_need = 5, availability_need = 5, budget_sensitivity = 3))
