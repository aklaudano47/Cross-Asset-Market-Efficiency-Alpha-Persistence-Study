### FINAL THESIS MOMENTUM: MASTER SCRIPT (ALL ASSETS)
# --------------------------------------------------
rm(list = ls())
library(readr); library(dplyr); library(lubridate); library(stringr)

assets <- c("CORN","WHEAT","SOYBEANS","COTTON","LUMBER","COFFEE","DOWJONES","BITCOIN","SOYOIL","OATS","GOLD","SUGAR")
base_path <- "Data/"
momentum_results <- list()

# Helper: detect which of two columns is the date col (robust over multiple rows)
detect_date_col_2col <- function(df, n = 25) {
  n <- min(n, nrow(df))
  if (n == 0) return(TRUE)
  c1 <- as.character(df[[1]][1:n])
  c2 <- as.character(df[[2]][1:n])
  s1 <- mean(!is.na(parse_date_time(c1, orders=c("mdy","ymd","dmy","mdy HMS","ymd HMS"), quiet=TRUE)))
  s2 <- mean(!is.na(parse_date_time(c2, orders=c("mdy","ymd","dmy","mdy HMS","ymd HMS"), quiet=TRUE)))
  s1 >= s2
}

for (a in assets) {
  file_path <- paste0(base_path, a, "_Thesis_Data.csv")
  if (!file.exists(file_path)) {
    cat(paste0("Missing file: ", a, "\n"))
    next
  }
  
  # 1) READ RAW
  df <- read_csv(file_path, show_col_types = FALSE)
  
  # 2) STANDARDIZE TO TWO COLUMNS: D_COL (date) and V_COL (value)
  if (a == "BITCOIN") {
    # BTC format: Start, End, Open, High, Low, Close, Volume, Market Cap
    # Use End date + Close price
    if (!all(c("End","Close") %in% names(df))) {
      cat(paste0("BTC missing End/Close columns -> skipped: ", a, "\n"))
      next
    }
    df_clean <- df %>% transmute(D_COL = End, V_COL = Close)
  } else {
    # Macrotrends-like: 2 columns (Date, Value) or swapped
    if (ncol(df) < 2) {
      cat(paste0("Not enough columns -> skipped: ", a, "\n"))
      next
    }
    
    col1_date <- detect_date_col_2col(df, n = 25)
    if (col1_date) {
      df_clean <- df %>% rename(D_COL = 1, V_COL = 2)
    } else {
      df_clean <- df %>% rename(V_COL = 1, D_COL = 2)
    }
  }
  
  # 3) ANALYSIS PIPELINE
  m_data <- df_clean %>%
    mutate(
      D = parse_date_time(as.character(D_COL),
                          orders = c("mdy","ymd","dmy","mdy HMS","ymd HMS"),
                          quiet = TRUE),
      V = suppressWarnings(as.numeric(gsub("[$,]", "", as.character(V_COL))))
    ) %>%
    filter(!is.na(D), !is.na(V)) %>%
    filter(year(D) < 2024) %>%             # keep your thesis cutoff
    arrange(D) %>%
    group_by(yr = year(D), mn = month(D)) %>%
    summarise(V = last(V), .groups="drop") %>%
    arrange(yr, mn) %>%
    mutate(Ret = (V / lag(V) - 1) * 100) %>%
    mutate(Sig = ifelse(lag(Ret) > 0, "Up", "Down")) %>%
    filter(!is.na(Ret), !is.na(Sig))
  
  # 4) STATS (Spread + permutation p-value)
  stats <- m_data %>% group_by(Sig) %>% summarise(M = mean(Ret), .groups = "drop")
  
  if (nrow(stats) < 2 ||
      !all(c("Up","Down") %in% stats$Sig)) {
    cat(paste0("Insufficient Up/Down months -> skipped: ", a, "\n"))
    next
  }
  
  obs_diff <- stats$M[stats$Sig == "Up"] - stats$M[stats$Sig == "Down"]
  
  set.seed(123)
  null_dist <- replicate(20000, {
    shuffled <- sample(m_data$Sig)
    mean(m_data$Ret[shuffled == "Up"]) - mean(m_data$Ret[shuffled == "Down"])
  })
  p_val <- mean(abs(null_dist) >= abs(obs_diff))
  
  momentum_results[[a]] <- data.frame(
    Asset = a,
    Spread = round(obs_diff, 4),
    P_Value = round(p_val, 5)
  )
  
  cat(paste0("Success: ", a, "\n"))
}

final_monthly_table <- bind_rows(momentum_results)

cat("\n--- MASTER THESIS RESULTS ---\n")
print(final_monthly_table)
