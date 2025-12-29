rm(list = ls())
library(readr); library(dplyr); library(lubridate)

assets <- c("CORN","WHEAT","SOYBEANS","COTTON","LUMBER","COFFEE",
            "DOWJONES","BITCOIN","SOYOIL","OATS","GOLD","SUGAR","SOYOIL")
base_path <- "Data/"
wf_results <- list()

detect_date_col_2col <- function(df, n = 25) {
  n <- min(n, nrow(df))
  if (n == 0) return(TRUE)
  c1 <- as.character(df[[1]][1:n])
  c2 <- as.character(df[[2]][1:n])
  s1 <- mean(!is.na(parse_date_time(c1, orders=c("mdy","ymd","dmy"), quiet=TRUE)))
  s2 <- mean(!is.na(parse_date_time(c2, orders=c("mdy","ymd","dmy"), quiet=TRUE)))
  s1 >= s2
}

PCTL <- 0.75
N_PERM <- 20000
CUTOFF_YEAR <- 2024
MIN_HISTORY_WEEKS <- 52

for (a in assets) {
  file_path <- paste0(base_path, a, "_Thesis_Data.csv")
  if (!file.exists(file_path)) next
  
  df <- read_csv(file_path, show_col_types = FALSE)
  
  # STANDARD OVERRIDES for known Macrotrends assets
  if (a == "COFFEE") {
    df_std <- df %>% transmute(D_COL = Date, V_COL = Value)
  } else if (a == "GOLD") {
    df_std <- df %>% transmute(D_COL = Date, V_COL = Value)
  } else if (a == "BITCOIN") {
    if (!all(c("End","Close") %in% names(df))) next
    df_std <- df %>% transmute(D_COL = End, V_COL = Close)
  } else {
    if (ncol(df) < 2) next
    col1_date <- detect_date_col_2col(df, n = 25)
    df_std <- if (col1_date) df %>% rename(D_COL = 1, V_COL = 2)
    else          df %>% rename(V_COL = 1, D_COL = 2)
  }
  
  df_clean <- df_std %>%
    mutate(
      D = parse_date_time(as.character(D_COL), orders=c("mdy","ymd","dmy"), quiet=TRUE),
      V = suppressWarnings(as.numeric(gsub("[$,]", "", as.character(V_COL))))
    ) %>%
    filter(!is.na(D), !is.na(V), year(D) < CUTOFF_YEAR) %>%
    arrange(D)
  
  w <- df_clean %>%
    mutate(wk = floor_date(D, unit="week", week_start=1)) %>%
    group_by(wk) %>%
    summarise(Price = last(V), .groups="drop") %>%
    arrange(wk) %>%
    mutate(Ret = (Price / lag(Price) - 1) * 100) %>%
    filter(!is.na(Ret))
  
  if (nrow(w) < (MIN_HISTORY_WEEKS + 20)) next
  
  n <- nrow(w)
  Sig <- rep(NA_character_, n)
  Thr <- rep(NA_real_, n)
  
  for (t in (MIN_HISTORY_WEEKS+2):n) {
    past <- w$Ret[1:(t-2)]
    thr_t <- quantile(past, probs=PCTL, na.rm=TRUE, type=7)
    Thr[t] <- thr_t
    Sig[t] <- ifelse(w$Ret[t-1] > thr_t, "Extreme", "Normal")
  }
  
  wf_data <- w %>% mutate(Thr=Thr, Sig=Sig) %>% filter(!is.na(Sig))
  
  m_ext  <- mean(wf_data$Ret[wf_data$Sig=="Extreme"])
  m_norm <- mean(wf_data$Ret[wf_data$Sig=="Normal"])
  obs_diff <- m_ext - m_norm
  
  null <- replicate(N_PERM, {
    shuf <- sample(wf_data$Sig)
    mean(wf_data$Ret[shuf=="Extreme"]) - mean(wf_data$Ret[shuf=="Normal"])
  })
  
  p_val <- mean(abs(null) >= abs(obs_diff))
  
  wf_results[[a]] <- data.frame(
    Asset = a,
    Spread = round(obs_diff,4),
    P_Value = round(p_val,5),
    Avg_Thr_75 = round(mean(Thr,na.rm=TRUE),4),
    Extreme_Share = round(mean(wf_data$Sig=="Extreme"),4),
    N_Extreme = sum(wf_data$Sig=="Extreme"),
    N_Normal  = sum(wf_data$Sig=="Normal")
  )
}

final_weekly_wf_table <- bind_rows(wf_results)
print(final_weekly_wf_table)
