# Read the clean dataset
main <- readRDS("data/AI games.rds")

# ADD THIS: create unique rownames BEFORE filtering/subsetting
rownames(main) <- 1:nrow(main)
panelA_data <- main %>% filter(game != "Virtual 2025")
rownames(panelA_data) <- 1:nrow(panelA_data)

# Create the combined FE variable and make sure all factors are factors
main <- main %>%
  mutate(
    game_software = interaction(game, software, drop = TRUE),
    max_skill = as.factor(max_skill),
    min_skill = as.factor(min_skill),
    max_gpt   = as.factor(max_gpt),
    min_gpt   = as.factor(min_gpt),
    attendance = as.factor(attendance),
    branch = as.factor(branch)
  )
panelA_data <- panelA_data %>%
  mutate(
    game_software = interaction(game, software, drop = TRUE),
    max_skill = as.factor(max_skill),
    min_skill = as.factor(min_skill),
    max_gpt   = as.factor(max_gpt),
    min_gpt   = as.factor(min_gpt),
    attendance = as.factor(attendance),
    branch = as.factor(branch)
  )

# Dependent variables (make sure they are numeric)
dep_vars <- c(
  "reproduction", "minor_errors", "major_errors",
  "one_good_robustness", "two_good_robustness",
  "ran_one_robustness", "ran_two_robustness"
)
main <- main %>% mutate(across(all_of(dep_vars), as.numeric))
panelA_data <- panelA_data %>% mutate(across(all_of(dep_vars), as.numeric))

# Run models (use the combined FE)
panelA_models <- map(dep_vars, ~
                       feols(
                         as.formula(paste(.x, "~ branch + number_teammates | game_software + max_skill + min_skill + attendance")),
                         data = panelA_data,
                         vcov = "hetero"
                       )
)

panelB_models <- map(dep_vars, ~
                       feols(
                         as.formula(paste(.x, "~ branch + number_teammates | game_software + max_skill + min_skill + attendance")),
                         data = main,
                         vcov = "hetero"
                       )
)

# Function to get p-value (AI-Assisted vs AI-Led) with HC1 robust using car
get_pval <- function(model) {
  lh <- tryCatch(
    linearHypothesis(model, "branchAI-Assisted = branchAI-Led",
                     vcov. = vcov(model, type = "HC1")),
    error = function(e) return(NA)
  )
  if (is.null(lh) || !is.data.frame(lh)) return(NA)
  if ("Pr(>Chisq)" %in% colnames(lh) && nrow(lh) >= 2) {
    return(as.numeric(lh[2, "Pr(>Chisq)"]))
  }
  NA
}

# Robust function to extract coefficients, errors, CI, means, and N
extract_summary <- function(model, dv, df) {
  coefs <- broom::tidy(model, conf.int = TRUE)
  ai_assisted <- coefs[coefs$term == "branchAI-Assisted", ]
  ai_led <- coefs[coefs$term == "branchAI-Led", ]
  used_rows <- which(!is.na(model$fitted.values))
  ymean <- mean(as.numeric(df[[dv]][used_rows]), na.rm = TRUE)
  nobs <- nobs(model)
  pval <- get_pval(model)
  tibble(
    coef_ai_assisted = if (nrow(ai_assisted)) ai_assisted$estimate else NA,
    se_ai_assisted   = if (nrow(ai_assisted)) ai_assisted$std.error else NA,
    ci_low_ai_assisted = if (nrow(ai_assisted)) ai_assisted$conf.low else NA,
    ci_high_ai_assisted = if (nrow(ai_assisted)) ai_assisted$conf.high else NA,
    coef_ai_led      = if (nrow(ai_led)) ai_led$estimate else NA,
    se_ai_led        = if (nrow(ai_led)) ai_led$std.error else NA,
    ci_low_ai_led    = if (nrow(ai_led)) ai_led$conf.low else NA,
    ci_high_ai_led   = if (nrow(ai_led)) ai_led$conf.high else NA,
    ymean            = ymean,
    N                = nobs,
    pval             = pval
  )
}

# Run the functions and pack results in tibbles for panels
panelA_res <- map2(panelA_models, dep_vars, ~extract_summary(.x, .y, panelA_data))
panelB_res <- map2(panelB_models, dep_vars, ~extract_summary(.x, .y, main))

panel_to_tbl <- function(res, panel_name) {
  tibble(
    Variable = c("Reproduction", "Minor errors", "Major errors", "One good robustness",
                 "Two good robustness", "Ran one robustness", "Ran two robustness"),
    "AI-Assisted" = map_chr(res, ~sprintf("%.3f", .x$coef_ai_assisted)),
    "(SE) AA"     = map_chr(res, ~sprintf("(%.3f)", .x$se_ai_assisted)),
    "AI-Led"      = map_chr(res, ~sprintf("%.3f", .x$coef_ai_led)),
    "(SE) AL"     = map_chr(res, ~sprintf("(%.3f)", .x$se_ai_led)),
    "Mean DV"     = map_chr(res, ~ifelse(is.na(.x$ymean), "", sprintf("%.3f", .x$ymean))),
    "p-value"     = map_chr(res, ~ifelse(is.na(.x$pval), "", sprintf("%.3f", .x$pval))),
    "Obs."        = map_chr(res, ~as.character(.x$N)),
    Panel = panel_name
  )
}

panelA_tbl <- panel_to_tbl(panelA_res, "Panel A: Study 1")
panelB_tbl <- panel_to_tbl(panelB_res, "Panel B: Study 2 (Combined)")

full_tbl <- bind_rows(panelA_tbl, panelB_tbl)

print(full_tbl)

get_stars <- function(pval) {
  if (is.na(pval)) return("")
  if (pval < 0.01) return("***")
  if (pval < 0.05) return("**")
  if (pval < 0.1)  return("*")
  return("")
}

make_panel_latex <- function(panel_res, panel_label) {
  coefs_aa <- sapply(panel_res, function(x) sprintf("%8.3f%s", x$coef_ai_assisted, get_stars(x$pval)))
  ses_aa   <- sapply(panel_res, function(x) sprintf("%8.3f", x$se_ai_assisted))
  ci_aa    <- sapply(panel_res, function(x) sprintf("[%8.3f; %8.3f]", x$ci_low_ai_assisted, x$ci_high_ai_assisted))
  coefs_al <- sapply(panel_res, function(x) sprintf("%8.3f%s", x$coef_ai_led, get_stars(x$pval)))
  ses_al   <- sapply(panel_res, function(x) sprintf("%8.3f", x$se_ai_led))
  ci_al    <- sapply(panel_res, function(x) sprintf("[%8.3f; %8.3f]", x$ci_low_ai_led, x$ci_high_ai_led))
  ymean    <- sapply(panel_res, function(x) sprintf("%8.3f", x$ymean))
  pvals    <- sapply(panel_res, function(x) ifelse(is.na(x$pval), "", sprintf("%8.3f", x$pval)))
  nobs     <- sapply(panel_res, function(x) as.character(x$N))
  
  varnames <- c(
    "Reproduction", 
    "\\shortstack[c]{Minor\\\\errors}", 
    "\\shortstack[c]{Major\\\\errors}", 
    "\\shortstack[c]{One good\\\\robustness}", 
    "\\shortstack[c]{Two good\\\\robustness}", 
    "\\shortstack[c]{Ran one\\\\robustness}", 
    "\\shortstack[c]{Ran two\\\\robustness}"
  )
  
  # Start panel
  out <- ""
  out <- paste0(out, "\\multicolumn{8}{l}{\\textbf{", panel_label, "}}\\\\\n")
  out <- paste0(out, "& (1) & (2) & (3) & (4) & (5) & (6) & (7)\\\\\n")
  out <- paste0(out, "                    &", paste(varnames, collapse = "   &"), "   \\\\\n\\hline\n")
  
  # AI-Assisted
  out <- paste0(out, "AI-Assisted         &", paste(coefs_aa, collapse="   &"), "   \\\\\n")
  out <- paste0(out, "                    &", paste(sprintf("(%s)", ses_aa), collapse="   &"), "   \\\\\n")
  out <- paste0(out, "                    &", paste(ci_aa, collapse="   &"), "   \\\\\n")
  # AI-Led
  out <- paste0(out, "AI-Led              &", paste(coefs_al, collapse="   &"), "   \\\\\n")
  out <- paste0(out, "                    &", paste(sprintf("(%s)", ses_al), collapse="   &"), "   \\\\\n")
  out <- paste0(out, "                    &", paste(ci_al, collapse="   &"), "   \\\\\n")
  # Controls
  out <- paste0(out, "\\hline\nControls            &", paste(rep("  \\checkmark  ", 7), collapse="&"), "   \\\\\n")
  out <- paste0(out, "Mean of dep. var    &", paste(ymean, collapse="   &"), "   \\\\\n")
  out <- paste0(out, "p-val (AI-Assisted vs. AI-Led)&", paste(pvals, collapse="   &"), "   \\\\\n")
  out <- paste0(out, "Obs.                &", paste(nobs, collapse="   &"), "   \\\\\n\\hline\n")
  return(out)
}

print_full_latex_table <- function(panelA_res, panelB_res) {
  cat("\\def\\sym#1{\\ifmmode^{#1}\\else\\(^{#1}\\)\\fi}\n")
  cat("\\begin{tabular}{l*{7}{c}}\n")
  cat("\\hline\\hline\n")
  cat(make_panel_latex(panelA_res, "Panel A: Study 1"))
  cat("\\\\\n")
  cat(make_panel_latex(panelB_res, "Panel B: Study 2 combined"))
  cat("\\hline\\hline\n")
  cat("\\multicolumn{8}{l}{\\it{Note:} Standard errors in ")
  cat("parentheses, confidence intervals in brackets; ")
  cat("human-only branch omitted.}\\\\\n")
  cat("\\multicolumn{8}{l}{Controls include number of ")
  cat("teammates; game--software, skill, and attendance ")
  cat("fixed effects.}\\\\\n")
  cat("\\multicolumn{8}{l}{\\sym{*} \\(p<0.1\\), \\sym{**} \\(p<0.05\\), \\sym{***} \\(p<0.01\\)}\\\\\n")
  cat("\\end{tabular}\n")
}

sink("output/tables/main.tex")
print_full_latex_table(panelA_res, panelB_res)
sink()