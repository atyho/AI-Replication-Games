# ---- 1. Load data ----
main <- readRDS("data/AI games.rds")

# ---- 2. Create branch_team_n ----
main <- main %>%
  group_by(branch) %>%
  mutate(branch_team_n = row_number()) %>%
  ungroup()

# ---- 3. Variable list ----
varlist <- c("time2_reproduction", "time2_first_minor", "time2_first_major")
xtitles <- c(
  time2_reproduction = "Minutes to reproduction",
  time2_first_minor  = "Minutes to first minor error",
  time2_first_major  = "Minutes to first major error"
)

# ---- 4. Loop over variables ----
for (var in varlist) {
  
  # a. Set missing to 999
  df <- main %>%
    mutate(!!sym(var) := ifelse(is.na(.data[[var]]), 999, .data[[var]])) %>%
    dplyr::select(branch_team_n, branch, all_of(var))
  
  # Map branch to number for wide reshape
  df$branch_num <- case_when(
    df$branch == "Human-Only"    ~ 1,
    df$branch == "AI-Assisted"   ~ 2,
    df$branch == "AI-Led"        ~ 3,
    TRUE                        ~ NA_real_
  )
  colnames(df)[3] <- paste0(var, "_br")
  
  # b. Reshape wide
  df_wide <- df %>%
    pivot_wider(names_from = branch_num, values_from = paste0(var, "_br"), names_prefix = paste0(var, "_br"))
  
  
  # Ensure branches 1-3 exist
  for (b in 1:3) {
    nm <- paste0(var, "_br", b)
    if (!(nm %in% names(df_wide))) {
      df_wide[[nm]] <- NA_real_
    }
  }
  
  # c. Cumulative for each branch
  for (b in 1:3) {
    df_wide <- df_wide %>%
      arrange(.data[[paste0(var, "_br", b)]]) %>%
      mutate(
        !!paste0(var, "_cum_br", b) :=
          cumsum(!is.na(.data[[paste0(var, "_br", b)]]) & .data[[paste0(var, "_br", b)]] != 999) /
          sum(!is.na(.data[[paste0(var, "_br", b)]]) & .data[[paste0(var, "_br", b)]] != 999)
      )
  }
  
  # d. Stack back into long format
  dfs <- lapply(1:3, function(b) {
    tibble(
      branch = b,
      var_br = df_wide[[paste0(var, "_br", b)]],
      var_cum_br = df_wide[[paste0(var, "_cum_br", b)]]
    )
  })
  df_long <- bind_rows(dfs) %>% mutate(branch = as.factor(branch))

  
  # e. Extend CDF to 420 if needed
  df_ext <- df_long %>%
    group_split(branch) %>%
    lapply(function(.x) {
      last_row <- filter(.x, var_br != 999) %>%
        slice_max(order_by = var_br, n = 1, with_ties = FALSE)
      # Si está vacío, skip (caso extremo)
      if (nrow(last_row) == 0) return(.x)
      last_row_var_br <- last_row$var_br[1]
      last_row_var_cum_br <- last_row$var_cum_br[1]
      # ¿El último valor es menor a 420?
      if (!is.na(last_row_var_br) && last_row_var_br < 420) {
        add_row <- tibble(
          branch = unique(.x$branch),
          var_br = 420,
          var_cum_br = last_row_var_cum_br
        )
        bind_rows(.x, add_row)
      } else {
        .x
      }
    }) %>%
    bind_rows() %>%
    arrange(branch, var_br)
  
  # Remove 999 from plot
  df_ext <- df_ext %>% filter(var_br != 999)
  
  # f. Set labels and color-blind-friendly colors (Okabe-Ito palette)
  branch_labels <- c("1" = "Human-Only", "2" = "AI-Assisted", "3" = "AI-Led")
  fill_colors <- c("1" = "#0072B2", "2" = "#D55E00", "3" = "#009E73")
  line_colors <- fill_colors
  
  # g. Plot
  p <- ggplot(df_ext, aes(x = var_br, y = var_cum_br, fill = branch, color = branch)) +
    geom_area(alpha = 0.5, position = "identity") +
    scale_fill_manual(values = fill_colors, labels = branch_labels) +
    scale_color_manual(values = line_colors, labels = branch_labels) +
    labs(
      y = "Cumulative density",
      x = xtitles[[var]],
      fill = "",
      color = "",
      title = NULL,
      subtitle = NULL
    ) +
    theme_minimal(base_size = 13) +
    theme(
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "white", color = NA),
      legend.position = "top",
      legend.title = element_blank()
    )
  
  # h. Export
  ggsave(sprintf("output/figures/%s.pdf", var), p, width = 8, height = 4)
  
  
  
  p_s1 <- ggplot(
    df_ext %>% filter(branch != "AI-Led"),   # Change to your branch to exclude
    aes(x = var_br, y = var_cum_br, fill = branch, color = branch)
  ) +
    geom_area(alpha = 0.5, position = "identity") +
    scale_fill_manual(values = fill_colors, labels = branch_labels) +
    scale_color_manual(values = line_colors, labels = branch_labels) +
    labs(
      y = "Cumulative density",
      x = xtitles[[var]],
      fill = "",
      color = "",
      title = NULL,
      subtitle = NULL
    ) +
    theme_minimal(base_size = 13) +
    theme(
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "white", color = NA),
      legend.position = "top",
      legend.title = element_blank()
    )
  
  ggsave(sprintf("output/figures/%s (s1).pdf", var), p_s1, width = 8, height = 4)
}