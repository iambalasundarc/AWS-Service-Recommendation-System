# 03_models.R --------------------------------------------------------------
suppressPackageStartupMessages({library(caret); library(ranger); library(rpart); library(ggplot2)})
source("R/utils_scoring.R")
set.seed(42)
aws <- read.csv("data/workload_requests_clean.csv", stringsAsFactors = FALSE)
services <- read.csv("data/aws_services.csv", stringsAsFactors = FALSE)

feat <- c("workload_type","project_scale","team_experience","scalability_need","low_ops_pref",
          "budget_sensitivity","latency_need","availability_need","security_need",
          "serverless_pref","realtime_need")
aws$request_id <- NULL
for (v in c("workload_type","project_scale","team_experience")) aws[[v]] <- factor(aws[[v]])
cls <- sort(unique(services$service))
aws$recommended_service <- factor(aws$recommended_service, levels = cls)
svc_type <- services$workload_type[match(cls, services$service)]

cat("\n=== Stage 4: Data splitting (80/20, stratified) ===\n")
idx   <- createDataPartition(aws$recommended_service, p = 0.8, list = FALSE)
train <- aws[idx, ]; test <- aws[-idx, ]
cat("Train rows:", nrow(train), "| Test rows:", nrow(test), "\n")

# ---- helpers ---------------------------------------------------------------
full <- function(p) { m <- matrix(0, nrow(p), length(cls), dimnames = list(NULL, cls)); m[, colnames(p)] <- p; m }
# a recommender may only suggest services of the requested workload type
restrict <- function(m, wt) { a <- outer(as.character(wt), svc_type, "=="); m <- m * a + 1e-9 * a; m / rowSums(m) }
hit_at <- function(m, truth, k) mean(sapply(seq_along(truth), function(i)
  truth[i] %in% names(sort(m[i, ], decreasing = TRUE))[1:k]))
res <- data.frame()
add <- function(name, m) res <<- rbind(res, data.frame(Model = name,
  Hit_at_1 = round(hit_at(m, as.character(test$recommended_service), 1), 4),
  Hit_at_2 = round(hit_at(m, as.character(test$recommended_service), 2), 4)))

cat("\n=== Stage 5: Model training ===\n")
# Reference A: popularity within workload type
pop <- table(train$workload_type, train$recommended_service)
add("Popularity baseline", restrict(t(sapply(as.character(test$workload_type), function(w) pop[w, ])) + 0, test$workload_type))
# Reference B: random choice inside workload type
add("Random in workload type", restrict(matrix(runif(nrow(test) * length(cls)), nrow(test), dimnames = list(NULL, cls)), test$workload_type))

# Model 1: content-based cosine similarity (no training needed)
svc_vec <- with(services, cbind(scalability, 6 - ops_burden, cost_small, latency, availability, security, serverless * 5, realtime * 5))
cosine <- function(a, b) sum(a * b) / (sqrt(sum(a^2)) * sqrt(sum(b^2)))
m_cos <- t(sapply(seq_len(nrow(test)), function(i) {
  r <- test[i, ]
  q <- c(r$scalability_need, r$low_ops_pref, r$budget_sensitivity, r$latency_need,
         r$availability_need, r$security_need, r$serverless_pref * 5, r$realtime_need * 5)
  v <- setNames(rep(0, length(cls)), cls); rows <- which(services$workload_type == r$workload_type)
  v[services$service[rows]] <- sapply(rows, function(j) cosine(q, svc_vec[j, ])); v }))
add("Content-based (cosine)", restrict(m_cos, test$workload_type))

# Model 2: decision tree
tree <- rpart(recommended_service ~ ., data = train[, c(feat, "recommended_service")],
              method = "class", control = rpart.control(cp = 0.002))
add("Decision Tree (rpart)", restrict(full(predict(tree, test, type = "prob")), test$workload_type))

# Model 3: random forest, tuned with 5-fold CV on the training set
grid <- expand.grid(mtry = c(3, 5, 8, 11), min.node.size = c(1, 5, 10))
folds <- createFolds(train$recommended_service, k = 5)
grid$cv_hit1 <- sapply(seq_len(nrow(grid)), function(g) mean(sapply(folds, function(f) {
  tr <- train[-f, ]; va <- train[f, ]
  m <- ranger(recommended_service ~ ., data = tr[, c(feat, "recommended_service")], num.trees = 200,
              mtry = grid$mtry[g], min.node.size = grid$min.node.size[g], probability = TRUE, seed = 1)
  hit_at(restrict(full(predict(m, va)$predictions), va$workload_type), as.character(va$recommended_service), 1) })))
cat("\nRandom Forest tuning (5-fold CV Hit@1):\n"); print(grid[order(-grid$cv_hit1), ], row.names = FALSE)
best <- grid[which.max(grid$cv_hit1), ]
rf <- ranger(recommended_service ~ ., data = train[, c(feat, "recommended_service")], num.trees = 500,
             mtry = best$mtry, min.node.size = best$min.node.size, probability = TRUE,
             importance = "impurity", seed = 42)
m_rf <- restrict(full(predict(rf, test)$predictions), test$workload_type)
add("Random Forest (ranger, tuned)", m_rf)

# Ceiling: noise-free scoring rule (the best any model could do if architects never disagreed)
m_or <- t(sapply(seq_len(nrow(test)), function(i) { r <- test[i, ]; v <- setNames(rep(-1e9, length(cls)), cls)
  s <- services[services$workload_type == r$workload_type, ]; v[s$service] <- match_score(r, s, 0); v }))
add("Noise-free rule (ceiling)", m_or)

cat("\n=== Stage 6: Evaluation on test set ===\n"); print(res, row.names = FALSE)
write.csv(res, "outputs/model_comparison.csv", row.names = FALSE)

top1 <- cls[max.col(m_rf, ties.method = "first")]
acc_by <- aggregate(correct ~ workload_type, data = data.frame(workload_type = test$workload_type,
                    correct = top1 == as.character(test$recommended_service)), FUN = mean)
acc_by <- acc_by[order(-acc_by$correct), ]
cat("\nRandom Forest Hit@1 by workload type:\n"); print(acc_by, row.names = FALSE)
write.csv(acc_by, "outputs/rf_accuracy_by_workload.csv", row.names = FALSE)

# ---- figures ---------------------------------------------------------------
theme_set(theme_minimal(base_size = 12))
long <- reshape(res, direction = "long", varying = c("Hit_at_1","Hit_at_2"), v.names = "score",
                timevar = "metric", times = c("Hit@1","Hit@2"), idvar = "Model")
p6 <- ggplot(long, aes(x = reorder(Model, score), y = score, fill = metric)) +
  geom_col(position = "dodge") + coord_flip() + scale_fill_manual(values = c("#FF9900","#232F3E")) +
  geom_text(aes(label = sprintf("%.2f", score)), position = position_dodge(.9), hjust = -.1, size = 3) +
  ylim(0, 1.1) + labs(title = "Model comparison (test set)", x = NULL, y = "score", fill = NULL)
ggsave("outputs/figures/06_model_comparison.png", p6, width = 7.5, height = 4.5, dpi = 150)
imp <- sort(rf$variable.importance, decreasing = TRUE)
p7 <- ggplot(data.frame(f = names(imp), v = imp), aes(x = reorder(f, v), y = v)) +
  geom_col(fill = "#FF9900") + coord_flip() + labs(title = "Random Forest: feature importance", x = NULL, y = "impurity importance")
ggsave("outputs/figures/07_rf_importance.png", p7, width = 7, height = 4.5, dpi = 150)
p8 <- ggplot(acc_by, aes(x = reorder(workload_type, correct), y = correct)) +
  geom_col(fill = "#232F3E") + coord_flip() + ylim(0, 1) +
  labs(title = "Random Forest Hit@1 by workload type", x = NULL, y = "accuracy")
ggsave("outputs/figures/08_rf_accuracy_by_workload.png", p8, width = 7.5, height = 4.5, dpi = 150)

saveRDS(list(rf = rf, lv = lapply(aws[c("workload_type","project_scale","team_experience")], levels),
             cls = cls, svc_type = svc_type), "outputs/rf_model.rds")
