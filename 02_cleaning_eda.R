# 02_cleaning_eda.R --------------------------------------------------------
suppressPackageStartupMessages({library(ggplot2); library(corrplot)})
aws <- read.csv("data/workload_requests.csv", stringsAsFactors = FALSE)
cat("\n=== Stage 0: Loading dataset ===\n")
print(head(aws))
cat("\nDimensions:\n"); print(dim(aws))
cat("\nColumn names:\n"); print(names(aws))
cat("\nStructure:\n"); str(aws)
cat("\nSummary:\n"); print(summary(aws))

cat("\n=== Stage 1: Data cleaning ===\n")
na_summary <- colSums(is.na(aws)); print(na_summary)
dup_count <- sum(duplicated(aws[, -1]))      # ignore request_id
cat("Duplicate requests (ignoring request_id):", dup_count, "\n")
aws <- aws[!duplicated(aws[, -1]), ]
cat("Rows after removing duplicates:", nrow(aws), "\n")
cat("Range check (needs must be 1-5): ",
    all(sapply(aws[, 5:10], function(x) all(x >= 1 & x <= 5))), "\n")
write.csv(aws, "data/workload_requests_clean.csv", row.names = FALSE)

cat("\n=== Stage 2: Data visualization ===\n")
theme_set(theme_minimal(base_size = 12))
save <- function(p, f, w = 7, h = 4.5) ggsave(file.path("outputs/figures", f), p, width = w, height = h, dpi = 150)

# 1. requests per workload type
p1 <- ggplot(aws, aes(x = reorder(workload_type, workload_type, function(x) length(x)))) +
  geom_bar(fill = "#FF9900") + coord_flip() +
  labs(title = "Requests per workload type", x = NULL, y = "count")
save(p1, "01_workload_type_counts.png")

# 2. top recommended services
top <- head(sort(table(aws$recommended_service), decreasing = TRUE), 15)
p2 <- ggplot(data.frame(service = names(top), n = as.integer(top)),
             aes(x = reorder(service, n), y = n)) +
  geom_col(fill = "#232F3E") + coord_flip() +
  labs(title = "Top 15 most recommended AWS services", x = NULL, y = "count")
save(p2, "02_top_services.png")

# 3. correlation matrix
num_vars <- aws[, sapply(aws, is.numeric)]
num_vars$request_id <- NULL
cor_mat <- cor(num_vars, use = "pairwise.complete.obs")
png("outputs/figures/03_correlation.png", width = 900, height = 800, res = 130)
corrplot(cor_mat, method = "color", addCoef.col = "black", number.cex = 0.6, tl.cex = 0.7, tl.col = "black")
dev.off()
cat("\nCorrelation matrix:\n"); print(round(cor_mat, 2))

# 4. boxplots
long <- stack(num_vars[, 1:6])
p4 <- ggplot(long, aes(x = ind, y = values)) + geom_boxplot(fill = "#FF9900", alpha = .7) +
  labs(title = "Boxplots of requirement scores", x = NULL, y = "score (1-5)") +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))
save(p4, "04_boxplots.png")

# 5. needs by project scale
p5 <- ggplot(aws, aes(x = project_scale, y = budget_sensitivity, fill = project_scale)) +
  geom_boxplot(show.legend = FALSE) + scale_x_discrete(limits = c("Small","Medium","Large")) +
  labs(title = "Budget sensitivity by project scale", x = NULL)
save(p5, "05_budget_by_scale.png")
cat("Figures saved to outputs/figures\n")
