# ==============================================================================
# AFRHiCARE Uganda -- Round 2 Fieldwork Dashboard: REDCap connection + cleaning
# ==============================================================================
# Pulls three REDCap projects through their own API tokens:
#   HSR  -- Health System Responsiveness survey (patients / caregivers)
#   HWF  -- Health Workforce survey (health workers)
#   HFM  -- Health Facility Mapping (one record per hospital, 5 expected)
#
# Everything you are likely to edit is in the SETTINGS block below.
# If a token is missing, that tool runs on DEMO data so you can see the
# layout before the tokens are in place.
# ==============================================================================

library(curl)
library(jsonlite)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)

`%||%` <- function(a, b) if (is.null(a)) b else a

# ------------------------------------------------------------------------------
# SETTINGS -- edit these
# ------------------------------------------------------------------------------
REDCAP_URL <- Sys.getenv("REDCAP_URL", unset = "https://redcap.musph.ac.ug/api/")

TOKENS <- list(
  hsr = Sys.getenv("REDCAP_TOKEN_HSR", unset = "542E11901816B3CF26824A644F2D706E"),
  hwf = Sys.getenv("REDCAP_TOKEN_HWF", unset = "537C17A96FC737FD362B1EF2746ACF5E"),
  hfm = Sys.getenv("REDCAP_TOKEN_HFM", unset = "D0249255541069E9169501FBB6030820")
)

REFRESH_SECONDS <- 900          # re-pull from REDCap every 15 minutes

# 12-day fieldwork window (used as the default date filter and "Day x of 12")
FIELD_START <- as.Date("2026-11-16")
FIELD_DAYS  <- 12
FIELD_END   <- FIELD_START + FIELD_DAYS - 1

# The 5 Round 2 hospitals (Luweero is excluded). Names must match the REDCap
# choice labels of q3_hosp_name_ug exactly; the value is the short display name.
HOSPITALS <- c(
  "Adjumani General Hospital"      = "Adjumani",
  "Kawolo General Hospital Buikwe" = "Kawolo (Buikwe)",
  "St Francis Hospital Naggalama"  = "Naggalama",
  "Masindi General Hospital"       = "Masindi",
  "Kiryandongo General Hospital"   = "Kiryandongo"
)

# Optional targets per hospital (completed interviews). Leave NA if none.
TARGETS <- data.frame(
  hospital = names(HOSPITALS),
  hsr      = c(NA, NA, NA, NA, NA),
  hwf      = c(NA, NA, NA, NA, NA),
  stringsAsFactors = FALSE
)

# Which REDCap form statuses count as "Completed"
COMPLETE_STATUSES <- c("Complete")

# HF Mapping: Uganda forms. Core forms are expected at every hospital;
# department forms are expected only if that department was ticked in
# ug_depts (Departments available form).
HFM_CORE_FORMS <- c(
  facility_details           = "Facility details",
  bed_capacity               = "Bed capacity",
  service_delivery_outputs   = "Service delivery outputs",
  service_quality            = "Service quality",
  hr_section_info            = "HR section info",
  hr_staffing                = "HR staffing",
  departments_available      = "Departments available",
  tb_diagnosis_and_treatment = "TB diagnosis & treatment",
  clinical_support_services  = "Clinical support services"
)

HFM_DEPT_FORMS <- data.frame(
  form = c("outpatient_department", "emergency_department", "mental_health_clinic",
           "eye_clinic", "pharmacy", "dental_unit", "radiology_and_imaging",
           "physiotherapy_and_rehabilitation", "chronic_care_unit",
           "maternity_and_obgyn_ward", "paediatrics_ward_and_nicu", "surgical_ward",
           "operating_theatre", "icu_and_hdu", "medical_ward"),
  label = c("OPD", "Emergency", "Mental health", "Eye clinic", "Pharmacy", "Dental",
            "Radiology & imaging", "Physio & rehab", "Chronic care", "Maternity & OBGYN",
            "Paediatrics & NICU", "Surgical ward", "Operating theatre", "ICU & HDU",
            "Medical ward"),
  # ug_depts choice codes that make the form applicable ("3|4" = either)
  dept_codes = c("20", "19", "25", "22", "15", "23", "17", "18", "24", "5", "3|4",
                 "27", "12", "26", "28"),
  stringsAsFactors = FALSE
)

# ------------------------------------------------------------------------------
# REDCap API export (flat, labels, all records)
# ------------------------------------------------------------------------------
# Returns list(data = data.frame or NULL, error = NULL or message)
redcap_export <- function(token) {
  if (REDCAP_URL == "" || token == "") return(list(data = NULL, error = NULL))

  params <- c(
    token = token, content = "record", format = "json", type = "flat",
    rawOrLabel = "label", rawOrLabelHeaders = "raw",
    exportCheckboxLabel = "true", exportSurveyFields = "true",
    exportDataAccessGroups = "true", returnFormat = "json"
  )
  body <- paste(paste0(names(params), "=", curl_escape(params)), collapse = "&")

  h <- new_handle()
  handle_setheaders(h, "Content-Type" = "application/x-www-form-urlencoded")
  handle_setopt(h, postfields = body)

  resp <- tryCatch(curl_fetch_memory(REDCAP_URL, handle = h),
                   error = function(e) e)
  if (inherits(resp, "error")) {
    return(list(data = NULL, error = paste("Connection failed:", conditionMessage(resp))))
  }
  txt <- rawToChar(resp$content)
  Encoding(txt) <- "UTF-8"
  if (resp$status_code >= 400) {
    return(list(data = NULL, error = paste0("HTTP ", resp$status_code, ": ", substr(txt, 1, 200))))
  }
  if (str_trim(txt) %in% c("", "[]")) return(list(data = data.frame(), error = NULL))

  df <- tryCatch(fromJSON(txt, simplifyVector = TRUE), error = function(e) e)
  if (inherits(df, "error")) return(list(data = NULL, error = "Could not read REDCap response"))
  list(data = as.data.frame(df, stringsAsFactors = FALSE), error = NULL)
}

# ------------------------------------------------------------------------------
# Helpers
# ------------------------------------------------------------------------------
blank_to_na <- function(x) {
  x <- as.character(x)
  x[!is.na(x) & str_trim(x) == ""] <- NA
  x
}

# a column, or all-NA if REDCap didn't export it
col <- function(df, name) {
  if (name %in% names(df)) blank_to_na(df[[name]]) else rep(NA_character_, nrow(df))
}

to_date <- function(x) {
  x <- blank_to_na(x)
  out <- suppressWarnings(parse_date_time(x, orders = c("Ymd HMS", "Ymd HM", "Ymd", "dmY HMS", "dmY"),
                                          quiet = TRUE))
  as_date(out)
}

form_status <- function(x) {
  x <- blank_to_na(x)
  case_when(
    is.na(x)          ~ "Not started",
    x == "Complete"   ~ "Complete",
    x == "Unverified" ~ "Unverified",
    TRUE              ~ "Incomplete"
  )
}

# RA name, using the "Other, specify" text when "Other" was picked
ra_name <- function(main, other) {
  out <- ifelse(!is.na(main) & str_detect(main, regex("^other", ignore_case = TRUE)),
                ifelse(is.na(other), "Other (not named)", str_to_title(str_trim(other))),
                main)
  ifelse(is.na(out), "Not recorded", out)
}

# Shared finishing steps for HSR / HWF
finish_survey <- function(df) {
  if (nrow(df) == 0) return(df)
  df %>%
    # Uganda only (refusals have no country, so keep blanks), drop Luweero
    filter(is.na(country) | country == "Uganda") %>%
    filter(is.na(hospital) | hospital %in% names(HOSPITALS)) %>%
    mutate(
      hosp_short = ifelse(is.na(hospital), "Not recorded", unname(HOSPITALS[hospital])),
      consented  = !is.na(consent) & consent == "Yes",
      refused    = !is.na(consent) & consent == "No",
      completed  = consented & status %in% COMPLETE_STATUSES
    )
}

empty_hsr <- function() {
  tibble(record_id = character(0), ra = character(0), consent = character(0),
         country = character(0), hospital = character(0), date = as.Date(character(0)),
         status = character(0), respondent_group = character(0), ward = character(0),
         patient_sex = character(0), age = numeric(0), satisfaction = numeric(0),
         hosp_short = character(0), consented = logical(0), refused = logical(0),
         completed = logical(0))
}

empty_hwf <- function() {
  tibble(record_id = character(0), ra = character(0), consent = character(0),
         country = character(0), hospital = character(0), date = as.Date(character(0)),
         status = character(0), department = character(0), profession = character(0),
         sex = character(0), age = numeric(0),
         hosp_short = character(0), consented = logical(0), refused = logical(0),
         completed = logical(0))
}

# ------------------------------------------------------------------------------
# HSR -- patients / caregivers
# ------------------------------------------------------------------------------
clean_hsr <- function(raw) {
  if (is.null(raw) || nrow(raw) == 0) return(empty_hsr())
  rt <- col(raw, "q10a_respondent_type")
  tibble(
    record_id = col(raw, "record_id"),
    ra        = ra_name(col(raw, "ra_hrs"), col(raw, "ra_hrs_2")),
    consent   = col(raw, "consent"),
    country   = col(raw, "q2_country"),
    hospital  = col(raw, "q3_hosp_name_ug"),
    date      = coalesce(to_date(col(raw, "q1_date_survey")),
                         to_date(col(raw, "date_today")),
                         to_date(col(raw, "survey_timestamp")),
                         to_date(col(raw, "health_system_responsiveness_survey_timestamp"))),
    status    = form_status(col(raw, "health_system_responsiveness_survey_complete")),
    respondent_group = case_when(
      is.na(rt)                                          ~ NA_character_,
      str_detect(rt, regex("child", ignore_case = TRUE)) ~ "Caregiver of child (<18)",
      str_detect(rt, regex("caregiver", ignore_case = TRUE)) ~ "Caregiver of adult",
      TRUE                                               ~ "Adult patient"
    ),
    ward        = coalesce(col(raw, "clinical_ward"), col(raw, "clinical_ward_u18")),
    patient_sex = col(raw, "q11_gender"),
    age         = suppressWarnings(as.numeric(col(raw, "q10_age"))),
    satisfaction = suppressWarnings(as.numeric(str_extract(
      coalesce(col(raw, "q53_csat_h"), col(raw, "q53_csat_h_u18")), "^\\d+")))
  ) %>%
    finish_survey()
}

# ------------------------------------------------------------------------------
# HWF -- health workers
# ------------------------------------------------------------------------------
clean_hwf <- function(raw) {
  if (is.null(raw) || nrow(raw) == 0) return(empty_hwf())
  tibble(
    record_id  = col(raw, "record_id"),
    ra         = ra_name(col(raw, "ra_hwf"), col(raw, "ra_hwf_2")),
    consent    = col(raw, "consent"),
    country    = col(raw, "q2_country"),
    hospital   = col(raw, "q3_hosp_name_ug"),
    date       = coalesce(to_date(col(raw, "q1_date_survey")),
                          to_date(col(raw, "date_today")),
                          to_date(col(raw, "survey_timestamp")),
                          to_date(col(raw, "afrhicare_health_workforce_baseline_survey_timestamp"))),
    status     = form_status(col(raw, "afrhicare_health_workforce_baseline_survey_complete")),
    department = col(raw, "q8_department"),
    profession = col(raw, "q22_health_profession"),
    sex        = col(raw, "q10_gender"),
    age        = suppressWarnings(as.numeric(col(raw, "q9_age")))
  ) %>%
    mutate(
      department = ifelse(!is.na(department) & str_detect(department, regex("^other", ignore_case = TRUE)),
                          "Other", department),
      profession = ifelse(!is.na(profession) & str_detect(profession, regex("^other", ignore_case = TRUE)),
                          "Other", profession)
    ) %>%
    finish_survey()
}

# ------------------------------------------------------------------------------
# HF Mapping -- one record per hospital; status of every expected form
# ------------------------------------------------------------------------------
dept_ticked <- function(row, codes) {
  any(vapply(str_split(codes, "\\|")[[1]], function(cd) {
    v <- col(row, paste0("ug_depts___", cd))
    !is.na(v) && !(v %in% c("Unchecked", "0"))
  }, logical(1)))
}

clean_hfm <- function(raw) {
  hosp <- names(HOSPITALS)
  all_forms <- c(names(HFM_CORE_FORMS), HFM_DEPT_FORMS$form)
  all_labels <- c(unname(HFM_CORE_FORMS), HFM_DEPT_FORMS$label)

  if (is.null(raw) || nrow(raw) == 0) {
    raw <- data.frame(record_id = character(0), q3_hosp_name_ug = character(0),
                      stringsAsFactors = FALSE)
  }
  raw$hospital <- col(raw, "q3_hosp_name_ug")
  raw$n_complete <- if (nrow(raw) == 0) numeric(0) else
    Reduce(`+`, lapply(paste0(all_forms, "_complete"), function(f)
      as.integer(col(raw, f) %in% COMPLETE_STATUSES)))

  long <- list(); summ <- list()
  for (h in hosp) {
    recs <- raw[!is.na(raw$hospital) & raw$hospital == h, , drop = FALSE]
    n_recs <- nrow(recs)
    if (n_recs > 1) recs <- recs[order(-recs$n_complete), , drop = FALSE]
    row <- if (n_recs > 0) recs[1, , drop = FALSE] else NULL
    depts_known <- !is.null(row) &&
      any(grepl("^ug_depts___", names(row)) & !is.na(unlist(lapply(row, blank_to_na))))

    st <- vapply(all_forms, function(f) {
      if (is.null(row)) "Not started" else form_status(col(row, paste0(f, "_complete")))
    }, character(1))
    applicable <- c(rep(TRUE, length(HFM_CORE_FORMS)),
                    vapply(HFM_DEPT_FORMS$dept_codes, function(cd) {
                      if (!depts_known) TRUE else dept_ticked(row, cd)
                    }, logical(1)))
    st[!applicable & st == "Not started"] <- "N/A"

    long[[h]] <- tibble(hospital = h, hosp_short = unname(HOSPITALS[h]),
                        form = all_forms, form_label = all_labels,
                        type = c(rep("Core", length(HFM_CORE_FORMS)), rep("Department", nrow(HFM_DEPT_FORMS))),
                        applicable = applicable | st != "N/A", status = st)
    expected <- sum(long[[h]]$applicable)
    done <- sum(long[[h]]$applicable & long[[h]]$status %in% COMPLETE_STATUSES)
    summ[[h]] <- tibble(
      hospital = h, hosp_short = unname(HOSPITALS[h]),
      record_id = if (is.null(row)) NA_character_ else col(row, "record_id"),
      records = n_recs,
      visit_date = if (is.null(row)) as.Date(NA) else to_date(col(row, "q0_date_interview")),
      forms_expected = expected, forms_complete = done,
      pct_complete = if (expected > 0) round(100 * done / expected) else 0,
      depts_recorded = depts_known
    )
  }
  list(long = bind_rows(long), summary = bind_rows(summ))
}

# ------------------------------------------------------------------------------
# Data checks -- records supervisors should follow up
# ------------------------------------------------------------------------------
build_checks <- function(hsr, hwf, hfm_summary) {
  chk <- function(df, tool, cond, issue) {
    if (nrow(df) == 0) return(NULL)
    d <- df[cond, , drop = FALSE]
    if (nrow(d) == 0) return(NULL)
    tibble(Tool = tool, `Record ID` = d$record_id, Hospital = d$hosp_short,
           RA = d$ra, Date = d$date, Issue = issue)
  }
  out <- list(
    chk(hsr, "HSR", hsr$consented & !(hsr$status %in% COMPLETE_STATUSES), "Consented but form not marked Complete"),
    chk(hsr, "HSR", hsr$consented & is.na(hsr$hospital), "Hospital not recorded"),
    chk(hsr, "HSR", hsr$ra == "Not recorded", "RA not recorded"),
    chk(hsr, "HSR", is.na(hsr$date), "No interview date"),
    chk(hsr, "HSR", !is.na(hsr$age) & hsr$respondent_group %in% c("Adult patient", "Caregiver of adult") & hsr$age < 18,
        "Patient under 18 but recorded as adult"),
    chk(hsr, "HSR", !is.na(hsr$age) & hsr$respondent_group %in% "Caregiver of child (<18)" & hsr$age >= 18,
        "Patient 18+ but recorded as child"),
    chk(hwf, "HWF", hwf$consented & !(hwf$status %in% COMPLETE_STATUSES), "Consented but form not marked Complete"),
    chk(hwf, "HWF", hwf$consented & is.na(hwf$hospital), "Hospital not recorded"),
    chk(hwf, "HWF", hwf$ra == "Not recorded", "RA not recorded"),
    chk(hwf, "HWF", is.na(hwf$date), "No interview date")
  )
  dup <- hfm_summary %>% filter(records > 1)
  if (nrow(dup) > 0) {
    out[[length(out) + 1]] <- tibble(Tool = "HFM", `Record ID` = dup$record_id, Hospital = dup$hosp_short,
                                     RA = NA_character_, Date = dup$visit_date,
                                     Issue = paste(dup$records, "records for this hospital (duplicate?)"))
  }
  res <- bind_rows(out)
  if (nrow(res) == 0) {
    return(tibble(Tool = character(0), `Record ID` = character(0), Hospital = character(0),
                  RA = character(0), Date = as.Date(character(0)), Issue = character(0)))
  }
  res %>% arrange(Tool, Issue, Hospital)
}

# ------------------------------------------------------------------------------
# DEMO data (used for any tool whose token is not set)
# ------------------------------------------------------------------------------
demo_dates <- function(n) FIELD_START + sample(0:5, n, replace = TRUE)

make_demo_hsr <- function(n = 180) {
  set.seed(11)
  ras <- c("Arinda Agatha", "Aroko Suzan", "Watum Boniface", "Mubangizi Julius",
           "Mpanja Rhita", "Baluku Yonasan", "Angella Nabwire", "Mubiru Ronald")
  consent <- sample(c("Yes", "No"), n, TRUE, c(0.9, 0.1))
  grp <- sample(c("Adult patient", "Caregiver of child (<18)", "Caregiver of adult"), n, TRUE, c(.6, .3, .1))
  tibble(
    record_id = as.character(seq_len(n)), ra = sample(ras, n, TRUE), consent = consent,
    country = ifelse(consent == "Yes", "Uganda", NA),
    hospital = ifelse(consent == "Yes", sample(names(HOSPITALS), n, TRUE), NA),
    date = demo_dates(n),
    status = ifelse(consent == "Yes", sample(c("Complete", "Incomplete"), n, TRUE, c(.88, .12)), "Incomplete"),
    respondent_group = ifelse(consent == "Yes", grp, NA),
    ward = ifelse(consent == "Yes", sample(c("Medical Ward", "Surgical Ward", "Paediatric Ward",
                                             "Maternity Ward", "Post-Natal Ward (Normal Delivery or C/S)"), n, TRUE), NA),
    patient_sex = ifelse(consent == "Yes", sample(c("Male", "Female"), n, TRUE), NA),
    age = ifelse(grp == "Caregiver of child (<18)", sample(0:17, n, TRUE), sample(18:80, n, TRUE)),
    satisfaction = sample(4:10, n, TRUE)
  ) %>% finish_survey()
}

make_demo_hwf <- function(n = 120) {
  set.seed(12)
  consent <- sample(c("Yes", "No"), n, TRUE, c(0.93, 0.07))
  tibble(
    record_id = as.character(seq_len(n)), ra = sample(c("RA1", "RA2", "RA3"), n, TRUE), consent = consent,
    country = ifelse(consent == "Yes", "Uganda", NA),
    hospital = ifelse(consent == "Yes", sample(names(HOSPITALS), n, TRUE), NA),
    date = demo_dates(n),
    status = ifelse(consent == "Yes", sample(c("Complete", "Incomplete"), n, TRUE, c(.85, .15)), "Incomplete"),
    department = ifelse(consent == "Yes", sample(c("Internal medicine", "Surgery", "Obstetrics and gynaecology",
                                                   "Paediatrics", "Outpatient", "Casualty/emergency medicine"), n, TRUE), NA),
    profession = ifelse(consent == "Yes", sample(c("Nurse/Midwife", "Clinical officer or associate",
                                                   "Medical Doctor /surgeon", "Lab technician /technologist",
                                                   "Pharmacist / Pharmaceutical technologist"), n, TRUE, c(.5, .2, .15, .1, .05)), NA),
    sex = ifelse(consent == "Yes", sample(c("Male", "Female"), n, TRUE), NA),
    age = sample(22:60, n, TRUE)
  ) %>% finish_survey()
}

make_demo_hfm_raw <- function() {
  set.seed(13)
  hosp <- names(HOSPITALS)[1:4]                       # one hospital not yet visited
  forms <- c(names(HFM_CORE_FORMS), HFM_DEPT_FORMS$form)
  raw <- data.frame(record_id = as.character(seq_along(hosp)), q3_hosp_name_ug = hosp,
                    q0_date_interview = as.character(FIELD_START + 0:3), stringsAsFactors = FALSE)
  for (f in forms) {
    raw[[paste0(f, "_complete")]] <- sample(c("Complete", "Complete", "Incomplete", ""), length(hosp), TRUE)
  }
  for (cd in c("20", "19", "25", "22", "15", "23", "17", "18", "24", "5", "3", "4", "27", "12", "26", "28")) {
    raw[[paste0("ug_depts___", cd)]] <- sample(c("Ticked", ""), length(hosp), TRUE, c(.8, .2))
  }
  raw
}

# ------------------------------------------------------------------------------
# Single entry point app.R calls
# ------------------------------------------------------------------------------
load_all_data <- function() {
  r_hsr <- redcap_export(TOKENS$hsr)
  r_hwf <- redcap_export(TOKENS$hwf)
  r_hfm <- redcap_export(TOKENS$hfm)

  demo <- c(hsr = TOKENS$hsr == "" || REDCAP_URL == "",
            hwf = TOKENS$hwf == "" || REDCAP_URL == "",
            hfm = TOKENS$hfm == "" || REDCAP_URL == "")

  hsr <- if (demo[["hsr"]]) make_demo_hsr() else if (is.null(r_hsr$data)) empty_hsr() else clean_hsr(r_hsr$data)
  hwf <- if (demo[["hwf"]]) make_demo_hwf() else if (is.null(r_hwf$data)) empty_hwf() else clean_hwf(r_hwf$data)
  hfm <- clean_hfm(if (demo[["hfm"]]) make_demo_hfm_raw() else r_hfm$data)

  errors <- c(HSR = r_hsr$error %||% NA, HWF = r_hwf$error %||% NA, HFM = r_hfm$error %||% NA)
  errors <- errors[!is.na(errors)]

  list(
    hsr = hsr, hwf = hwf,
    hfm_long = hfm$long, hfm_summary = hfm$summary,
    checks = build_checks(hsr, hwf, hfm$summary),
    demo = demo, errors = errors, synced = Sys.time()
  )
}

