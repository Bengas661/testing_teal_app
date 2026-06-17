library(teal.modules.general)
library(teal.modules.clinical)
library(dplyr)
library(forcats)

# Load data ----
# Ensure character variables are converted to factors and 
# empty strings and NAs are explicit missing levels.
ADSL <- df_explicit_na(pharmaverseadam::adsl)
ADAE <- df_explicit_na(pharmaverseadam::adae)
ADTTE <- df_explicit_na(pharmaverseadam::adtte_onco)

# Data processing steps ----
arm_levels <- c("Screen Failure", "Placebo", "Xanomeline Low Dose", "Xanomeline High Dose")

# Factor levels control the order of display in tables and graphs.
ADSL <- ADSL %>%
  mutate(
    TRT01P = fct_relevel(TRT01P, arm_levels),
    TRT01A = fct_relevel(TRT01A, arm_levels)
  )



ADSL <- ADSL %>%
  mutate(
    EOTSTT = sample(
      c("ONGOING", "COMPLETED", "DISCONTINUED"),
      size = nrow(ADSL),
      replace = TRUE
    ) %>% as.factor()
  ) %>%
  col_relabel(
    EOTSTT = "End Of Treatment Status"
  )

date_vars_asl <- names(ADSL)[vapply(ADSL, function(x) inherits(x, c("Date", "POSIXct", "POSIXlt")), logical(1))]
demog_vars_asl <- names(ADSL)[!(names(ADSL) %in% c("USUBJID", "STUDYID", date_vars_asl))]




# Variable labels can be included in the app UI and are helpful for users
ADAE <- ADAE %>%
  mutate(
    AEDECOD = with_label(AEDECOD, "Dictionary-Derived Term"),
    AEBODSYS = with_label(AEBODSYS, "Body System or Organ Class")
  )

# Add variable for time unit as it is required for module
ADTTE$AVALU <- "DAYS"

# Reusable configuration for modules ----
cs_arm_var <- choices_selected(
  choices = variable_choices(ADSL, subset = c("TRT01A", "TRT01P")),
  selected = "TRT01P")

arm_ref_comp <- list(
  TRT01P = list(
    ref = "Placebo",
    comp = c("Xanomeline Low Dose", "Xanomeline High Dose")
  )
)

# Main app ----
app <- init(
  data = cdisc_data(
    ADSL=ADSL, 
    ADAE=ADAE,
    ADTTE=ADTTE
  ),
  modules = modules(
    tm_data_table(),
    tm_variable_browser(),
    tm_t_summary(
      label = "Demographic Table",
      dataname = "ADSL",
      arm_var = cs_arm_var,
      summarize_vars = choices_selected(
        choices = variable_choices(ADSL),
        selected = c("SEX", "AGE")
      ),
      numeric_stats = c("n", "mean_sd", "median", "range")
    ),
    
    
    tm_t_summary(
      label = "Disposition Table",
      dataname = "ADSL",
      arm_var = choices_selected(c("ARM", "ARMCD"), "ARM"),
      summarize_vars = choices_selected(
        variable_choices(ADSL, demog_vars_asl),
        c("EOSSTT",  "EOTSTT")
      ),
      useNA = "ifany"
    ),
    
    
    tm_t_events(
      label = "Adverse Event Table ",
      dataname = "ADAE",
      arm_var = cs_arm_var,
      llt = choices_selected(
        choices = variable_choices(ADAE, c("AETERM", "AEDECOD")),
        selected = c("AEDECOD")
      ),
      hlt = choices_selected(
        choices = variable_choices(ADAE, c("AEBODSYS", "AESOC")),
        selected = NULL
      )
    ),
    tm_g_km(
      label = "KM Plot",
      plot_height = c(600, 100, 2000),
      dataname = "ADTTE",
      arm_var = cs_arm_var,
      paramcd = choices_selected(
        value_choices(ADTTE, "PARAMCD", "PARAM"),
        "OS"
      ),
      arm_ref_comp = arm_ref_comp,
      strata_var = choices_selected(
        variable_choices(ADSL, c("SEX", "AGEGR1")),
        NULL
      ),
      facet_var = choices_selected(
        variable_choices(ADSL, c("SEX", "AGEGR1")),
        NULL
      )
    )
  )
)
shinyApp(app$ui, app$server)