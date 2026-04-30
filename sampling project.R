
library(dplyr)
library(ggplot2)

# ---------------------------
# PARAMETER
# ---------------------------
target_r <- 20

# ---------------------------
# FUNCTION: INVERSE SAMPLING
# ---------------------------
inverse_sample <- function(data, error_col, target_r) {
  
  temp <- data %>%
    mutate(cum_errors = cumsum(.data[[error_col]]))
  
  # Find stopping index (first time cum_errors == r)
  stop_index <- which(temp$cum_errors == target_r)[1]
  
  # Subset exactly up to stopping point
  inv_data <- temp[1:stop_index, ]
  
  N <- nrow(inv_data)
  r <- sum(inv_data[[error_col]])
  p_hat <- r / N
  
  var <- (p_hat^2 * (1 - p_hat)) / r
  se <- sqrt(var)
  
  ci_low <- max(0, p_hat - 1.96 * se)
  ci_high <- min(1, p_hat + 1.96 * se)
  
  return(list(
    data = inv_data,
    N = N,
    r = r,
    p_hat = p_hat,
    var = var,
    se = se,
    ci_low = ci_low,
    ci_high = ci_high
  ))
}

# ---------------------------
# APPLY TO BOTH CHATBOTS
# ---------------------------
gemini_res <- inverse_sample(df, "gemini_error", target_r)
claude_res <- inverse_sample(df, "claude_error", target_r)

# ---------------------------
# SUMMARY TABLE
# ---------------------------
summary_inv <- data.frame(
  chatbot = c("Gemini", "Claude"),
  r = c(gemini_res$r, claude_res$r),
  N = c(gemini_res$N, claude_res$N),
  p_hat = c(gemini_res$p_hat, claude_res$p_hat),
  se = c(gemini_res$se, claude_res$se),
  ci_low = c(gemini_res$ci_low, claude_res$ci_low),
  ci_high = c(gemini_res$ci_high, claude_res$ci_high)
)

print(summary_inv)

# ---------------------------
# HYPOTHESIS TEST
# ---------------------------
z <- (gemini_res$p_hat - claude_res$p_hat) /
  sqrt(gemini_res$var + claude_res$var)

p_value <- 2 * (1 - pnorm(abs(z)))

cat("Z value:", z, "\n")
cat("P value:", p_value, "\n")


plot_df <- summary_inv

ggplot(plot_df, aes(x = chatbot, y = p_hat)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.15) +
  geom_text(aes(label = round(p_hat, 3)), vjust = -1) +
  labs(
    title = "Estimated Error Probability (Inverse Sampling)",
    x = "Chatbot",
    y = "Estimated Error Probability"
  ) +
  theme_minimal()


# Gemini cumulative plot
ggplot(gemini_res$data, aes(x = 1:nrow(gemini_res$data), y = cum_errors)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = target_r, linetype = "dashed") +
  labs(
    title = "Inverse Sampling Process (Gemini)",
    x = "Number of Questions (N)",
    y = "Cumulative Errors"
  ) +
  theme_minimal()

ggplot(claude_res$data, aes(x = 1:nrow(claude_res$data), y = cum_errors)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = target_r, linetype = "dashed") +
  labs(
    title = "Inverse Sampling Process (Claude)",
    x = "Number of Questions (N)",
    y = "Cumulative Errors"
  ) +
  theme_minimal()

eff_df <- data.frame(
  chatbot = c("Gemini", "Claude"),
  N_required = c(gemini_res$N, claude_res$N)
)

ggplot(eff_df, aes(x = chatbot, y = N_required)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = N_required), vjust = -0.5) +
  labs(
    title = "Number of Questions Needed to Reach 20 Errors",
    x = "Chatbot",
    y = "Total Questions (N)"
  ) +
  theme_minimal()
