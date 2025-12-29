### THESIS EXPERIMENT 1: QUARTERLY SEASONALITY (UNIVERSAL FIX)
# ------------------------------------------------------------
rm(list = ls())
library(readr); library(dplyr); library(lubridate); library(stringr)

assets <- c("CORN","WHEAT","SOYBEANS","COTTON","LUMBER","COFFEE","DOWJONES","BITCOIN","SOYOIL","OATS","GOLD","SUGAR")
base_path <- "Data/"
seasonal_results <- list()

# robust date-col detector for 2-col files
detect_date_col_2col <- function(df, n = 25) {
  n <- min(n, nrow(df))
  if (n == 0) return(TRUE)
  c1 <- as.character(df[[1]][1:n])
  c2 <- as.character(df[[2]][1:n])
  s1 <- mean(!is.na(parse_date_time(c1, orders=c("mdy","ymd","dmy","mdy HMS","ymd HMS"), quiet=TRUE)))
  s2 <- mean(!is.na(parse_date_time(c2, orders=c("mdy","ymd","dmy","mdy HMS","ymd HMS"), quiet=TRUE)))
  s1 >= s2
}

# universal 2-column extractor for multi-col files (fallback)
pick_date_value_cols <- function(df) {
  nms <- names(df)
  
  # choose date column by name preference first
  date_pref <- intersect(nms, c("Date","date","End","Start","Timestamp","timestamp","time","datetime"))
  date_col <- if (length(date_pref) > 0) date_pref[1] else nms[1]
  
  # choose value column by preference
  val_pref <- intersect(nms, c("Close","close","Adj Close","AdjClose","adj_close","Price","price","Value","value","Last","last"))
  value_col <- if (length(val_pref) > 0) val_pref[1] else nms[2]
  
  list(date_col = date_col, value_col = value_col)
}

for (a in assets) {
  file_path <- paste0(base_path, a, "_Thesis_Data.csv")
  if (!file.exists(file_path)) {
    cat(paste0("Missing file: ", a, "\n"))
    next
  }
  
  df <- read_csv(file_path, show_col_types = FALSE)
  
  # 1) Standardize to D_COL / V_COL
  if (a == "BITCOIN") {
    if (!all(c("End","Close") %in% names(df))) {
      cat("BTC missing End/Close columns -> skipped\n")
      next
    }
    df_std <- df %>% transmute(D_COL = End, V_COL = Close)
    
  } else if (ncol(df) >= 2) {
    
    # If it's 2-col, use your robust swap logic
    if (ncol(df) == 2) {
      col1_date <- detect_date_col_2col(df, n = 25)
      df_std <- if (col1_date) df %>% rename(D_COL = 1, V_COL = 2)
      else          df %>% rename(V_COL = 1, D_COL = 2)
    } else {
      # multi-col fallback (e.g., GOLD from some source)
      cols <- pick_date_value_cols(df)
      df_std <- df %>% transmute(D_COL = .data[[cols$date_col]],
                                 V_COL = .data[[cols$value_col]])
    }
    
  } else {
    cat(paste0("Not enough columns -> skipped: ", a, "\n"))
    next
  }
  
  # 2) Clean + parse
  df_clean <- df_std %>%
    mutate(
      D = parse_date_time(as.character(D_COL),
                          orders = c("mdy","ymd","dmy","mdy HMS","ymd HMS"),
                          quiet = TRUE),
      V = suppressWarnings(as.numeric(gsub("[$,]", "", as.character(V_COL))))
    ) %>%
    filter(!is.na(D), !is.na(V), year(D) < 2024) %>%
    arrange(D)
  
  if (nrow(df_clean) < 50) {  # sanity guard
    cat(paste0("Too few rows after cleaning -> skipped: ", a, "\n"))
    next
  }
  
  # 3) Monthly close series -> returns -> quarter label (but quarters tested via months)
  q_data <- df_clean %>%
    group_by(yr = year(D), mn = month(D)) %>%
    summarise(Price = last(V), .groups = "drop") %>%
    arrange(yr, mn) %>%
    mutate(
      Ret = (Price / lag(Price) - 1) * 100,
      Qtr = case_when(
        mn %in% 1:3   ~ "Q1",
        mn %in% 4:6   ~ "Q2",
        mn %in% 7:9   ~ "Q3",
        mn %in% 10:12 ~ "Q4"
      )
    ) %>%
    filter(!is.na(Ret))
  
  # need all 4 quarters represented
  if (n_distinct(q_data$Qtr) < 4) {
    cat(paste0("Missing quarters -> skipped: ", a, "\n"))
    next
  }
  
  # 4) Observed spread
  q_stats <- q_data %>% group_by(Qtr) %>% summarise(M = mean(Ret), .groups = "drop")
  obs_spread <- max(q_stats$M) - min(q_stats$M)
  
  # 5) Permutation test
  set.seed(123)
  perm_dist <- replicate(5000, {
    shuffled_q <- sample(q_data$Qtr)
    null_stats <- q_data %>%
      mutate(sq = shuffled_q) %>%
      group_by(sq) %>%
      summarise(M = mean(Ret), .groups = "drop")
    max(null_stats$M) - min(null_stats$M)
  })
  p_val <- mean(perm_dist >= obs_spread)
  
  # 6) Store result (ensure means exist)
  get_mean <- function(q) {
    m <- q_stats$M[q_stats$Qtr == q]
    if (length(m) == 0) NA else round(m, 3)
  }
  
  seasonal_results[[a]] <- data.frame(
    Asset   = a,
    Q1_Mean = get_mean("Q1"),
    Q2_Mean = get_mean("Q2"),
    Q3_Mean = get_mean("Q3"),
    Q4_Mean = get_mean("Q4"),
    Spread  = round(obs_spread, 3),
    P_Value = round(p_val, 5),
    Sig     = ifelse(p_val < 0.05, "YES***", "NO")
  )
  
  cat(paste0("Success: ", a, "\n"))
}

final_seasonal_table <- bind_rows(seasonal_results)
cat("\n--- MASTER SEASONALITY RESULTS ---\n")
print(final_seasonal_table)
