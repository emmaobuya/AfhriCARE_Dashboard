# ==============================================================================
# AFRHiCARE Uganda -- Round 2 supervision data checks (HSR and HWF)
# ==============================================================================
# Sourced by app.R. Pulls the RAW records (codes, not labels) and the live data
# dictionary for each survey from REDCap, then runs two kinds of checks:
#
#  1. SKIP-LOGIC CHECKS (automatic, from the data dictionary)
#     Every question's REDCap branching logic is translated into R and evaluated
#     for each record:
#       - question SHOULD have been shown, is required, but is blank
#           -> "Required question left blank"   (missing data)
#       - question should have been SKIPPED but has an answer
#           -> "Answered although it should be skipped" (stale/inconsistent data)
#     Because the logic is read from REDCap each time, the checks stay correct
#     if the forms are edited during fieldwork.
#
#  2. TOOL-SPECIFIC CHECKS (written for supervision)
#     Consent, routing variables, age/respondent-type consistency, dates,
#     payments, duplicates, straight-lining, forms left open, etc.
#
# Severity:
#   Critical    - affects eligibility, routing or identity; fix today
#   High        - missing or contradictory data that changes results
#   Review      - unusual values; confirm with the RA
#   In progress - blanks in forms not yet marked Complete
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(httr)
  library(jsonlite)
})

# ------------------------------------------------------------------------------
# SETTINGS -- check these against your .Renviron
# ------------------------------------------------------------------------------
CHECK_CFG <- list(
  hsr = list(
    label      = "HSR (patients)",
    form       = "health_system_responsiveness_survey",
    ra         = "ra_hrs", ra_other = "ra_hrs_2",
    key_field  = "q10_age",          # any answer here means the interview started
    # The first of these environment variables that is set is used as the token.
    token_vars = c("REDCAP_TOKEN_HSR", "HSR_TOKEN", "REDCAP_HSR_TOKEN", "TOKEN_HSR"),
    dict_csv   = "dictionaries/HSR_dictionary.csv",   # used only if the API metadata call fails
    # Optional (not required) questions that supervisors still want answered
    important_optional = c("q10a_respondent_relationship", "q33_is_sleep_disturbance",
                           "q33_is_sleep_disturbance_u18")
  ),
  hwf = list(
    label      = "HWF (health workers)",
    form       = "afrhicare_health_workforce_round_2_survey",
    ra         = "ra_hwf", ra_other = "ra_hwf_2",
    key_field  = "q9_age",
    token_vars = c("REDCAP_TOKEN_HWF", "HWF_TOKEN", "REDCAP_HWF_TOKEN", "TOKEN_HWF"),
    dict_csv   = "dictionaries/HWF_dictionary.csv",
    important_optional = c("q0_previous_round", "q8a_other_department", "q52_supervisor_sex",
                           "q53_supervisor_equality", "q53_supervisor_respect",
                           "q73a_plan_location_oth", "q74_stay_country")
  )
)

UGANDA_CODE  <- "3"       # q2_country code for Uganda
STALE_DAYS   <- 2         # consented forms still open after this many days are flagged
SEV_LEVELS   <- c("Critical", "High", "Review", "In progress")
SEV_BG       <- c(Critical = "#d93025", High = "#f6a04d", Review = "#f2d45c", `In progress` = "#e3e7ea")
SEV_FG       <- c(Critical = "white",   High = "black",   Review = "black",   `In progress` = "#5f6368")

# Plain-language list shown on each checks tab
CHECK_ABOUT <- list(
  hsr = c(
    "<b>Skip logic (all questions):</b> a required question that the branching logic says should have been asked is blank; or a question that should have been skipped has an answer.",
    "<b>Consent:</b> consent not recorded but interview data present; refusal recorded but answers entered.",
    "<b>Identification:</b> RA, hospital or country (must be Uganda) missing or wrong.",
    "<b>Respondent type (Q10a):</b> missing, or a retired code (1/2) used. All experience sections depend on this answer (4 = adult patient; 3/5 = caregiver).",
    "<b>Age:</b> adult patient / caregiver of an adult with patient age under 18; caregiver of a child with patient age 18+; caregiver under 18.",
    "<b>Admission date:</b> after the interview date, or more than 90 days before it.",
    "<b>Referral:</b> came from a lower-level facility but 'sought care before' = No.",
    "<b>Payments:</b> paid admission or hospital costs but no service amounts entered; 'not asked to pay' but paid; itemised amounts very different from the total; very large amounts, distance or transport cost.",
    "<b>Straight-lining:</b> 15 or more experience ratings all identical.",
    "<b>Possible duplicates:</b> same hospital, date, age, sex, respondent type and ward.",
    "<b>Dates and open forms:</b> interview date in the future, before fieldwork, or more than a day from the data-entry date; consented forms still not Complete after 2 days."
  ),
  hwf = c(
    "<b>Skip logic (all questions):</b> a required question that the branching logic says should have been asked is blank; or a question that should have been skipped has an answer.",
    "<b>Consent:</b> consent not recorded but interview data present; refusal recorded but answers entered.",
    "<b>Identification:</b> RA, hospital, department or country (must be Uganda) missing or wrong.",
    "<b>Career dates:</b> started at this hospital before qualifying; implied age at qualification under 17; last promotion before qualification or after the interview.",
    "<b>Children:</b> more children under 18 than children in total.",
    "<b>Profession:</b> a retired choice code used (2, 3, 6, 7, 9).",
    "<b>Phone digits (Q19):</b> not exactly 4 digits (needed to detect repeat interviews).",
    "<b>Absenteeism:</b> more than 20 days absent in the past 30 days.",
    "<b>Straight-lining:</b> all ProQOL items identical (the scale has reverse-worded items), or all supervision items identical.",
    "<b>Possible duplicates:</b> same hospital, age, sex, profession and last four phone digits.",
    "<b>Dates and open forms:</b> interview date in the future, before fieldwork, or more than a day from the data-entry date; consented forms still not Complete after 2 days."
  )
)

# ------------------------------------------------------------------------------
# Small helpers
# ------------------------------------------------------------------------------
`%||%` <- function(a, b) if (is.null(a)) b else a
.num <- function(x) suppressWarnings(as.numeric(as.character(x)))
squish <- function(x) gsub("\\s+", " ", trimws(x))

to_date <- function(x) {
  x <- as.character(x)
  out <- as.Date(rep(NA_character_, length(x)))
  ok <- !is.na(x) & x != ""
  if (!any(ok)) return(out)
  s <- substr(x[ok], 1, 10)
  d1 <- suppressWarnings(as.Date(s, format = "%Y-%m-%d"))
  bad <- is.na(d1)
  if (any(bad)) d1[bad] <- suppressWarnings(as.Date(s[bad], format = "%d-%m-%Y"))
  out[ok] <- d1
  out
}

parse_choices <- function(s) {
  if (length(s) == 0 || is.na(s) || s == "") return(setNames(character(0), character(0)))
  parts <- trimws(strsplit(s, "|", fixed = TRUE)[[1]])
  setNames(trimws(sub("^[^,]*,", "", parts)), trimws(sub(",.*$", "", parts)))
}

get_token <- function(vars) {
  for (v in vars) { t <- Sys.getenv(v); if (nzchar(t)) return(t) }
  ""
}

# ------------------------------------------------------------------------------
# REDCap API
# ------------------------------------------------------------------------------
redcap_post <- function(token, content, extra = list()) {
  url <- Sys.getenv("REDCAP_URL")
  if (!nzchar(url)) stop("REDCAP_URL is not set in .Renviron")
  body <- c(list(token = token, content = content, format = "json", returnFormat = "json"), extra)
  res <- httr::POST(url, body = body, encode = "form", httr::timeout(120))
  txt <- httr::content(res, as = "text", encoding = "UTF-8")
  if (httr::http_error(res)) stop("REDCap ", content, " export failed: ", substr(txt, 1, 200))
  out <- jsonlite::fromJSON(txt, simplifyVector = TRUE)
  if (length(out) == 0) return(data.frame())
  as.data.frame(out, stringsAsFactors = FALSE)
}

DICT_COLS <- c("field_name", "form_name", "section_header", "field_type", "field_label",
               "select_choices_or_calculations", "field_note",
               "text_validation_type_or_show_slider_number", "text_validation_min",
               "text_validation_max", "identifier", "branching_logic", "required_field",
               "custom_alignment", "question_number", "matrix_group_name", "matrix_ranking",
               "field_annotation")

load_dict <- function(cfg, token) {
  dd <- tryCatch(redcap_post(token, "metadata"), error = function(e) NULL)
  if (is.null(dd) || nrow(dd) == 0) {
    if (!file.exists(cfg$dict_csv)) stop("Could not read the data dictionary from REDCap or ", cfg$dict_csv)
    dd <- read.csv(cfg$dict_csv, check.names = FALSE, stringsAsFactors = FALSE, encoding = "UTF-8")
    names(dd)[seq_along(DICT_COLS)] <- DICT_COLS[seq_len(min(ncol(dd), length(DICT_COLS)))]
  }
  for (cc in DICT_COLS) if (!cc %in% names(dd)) dd[[cc]] <- ""
  dd[] <- lapply(dd, function(x) { x <- as.character(x); x[is.na(x)] <- ""; x })
  dd$label_short <- substr(squish(gsub("<[^>]+>", " ", dd$field_label)), 1, 70)
  dd$not_asked <- grepl("@HIDDEN|@CALCTEXT|@CALCDATE|@READONLY|@NOW|@TODAY|@DEFAULT",
                        dd$field_annotation) |
                  dd$field_type %in% c("descriptive", "calc", "file", "sql")
  dd
}

# ------------------------------------------------------------------------------
# REDCap branching logic -> R
# ------------------------------------------------------------------------------
# Comparisons follow REDCap rules: numeric if both sides are numbers, otherwise
# text; a blank never satisfies <, >, <=, >=.
cmp_eq <- function(a, b) {
  an <- .num(a); bn <- .num(b)
  ac <- as.character(a); bc <- as.character(b)
  ac[is.na(ac)] <- ""; bc[is.na(bc)] <- ""
  out <- ifelse(!is.na(an) & !is.na(bn), an == bn, ac == bc)
  out[is.na(out)] <- FALSE
  out
}
cmp_ne  <- function(a, b) !cmp_eq(a, b)
cmp_rel <- function(op) function(a, b) { out <- op(.num(a), .num(b)); out[is.na(out)] <- FALSE; out }

OPS <- c("==" = ".eq", "!=" = ".ne", "<" = ".lt", ">" = ".gt", "<=" = ".le", ">=" = ".ge")

swap_ops <- function(e) {
  if (is.call(e)) {
    fn <- e[[1]]
    if (is.name(fn) && as.character(fn) %in% names(OPS)) e[[1]] <- as.name(OPS[[as.character(fn)]])
    if (length(e) > 1) for (i in 2:length(e)) e[[i]] <- swap_ops(e[[i]])
  }
  e
}

logic_to_r <- function(x) {
  if (is.na(x) || trimws(x) == "") return(NULL)            # no logic: always shown
  s <- gsub("[\r\n\t]+", " ", x)
  s <- gsub("\\[([A-Za-z0-9_]+)\\(([^)]+)\\)\\]", '.v("\\1___\\2")', s)   # checkbox [f(code)]
  s <- gsub("\\[([A-Za-z0-9_]+)\\]", '.v("\\1")', s)
  s <- gsub("<>", "!=", s, fixed = TRUE)
  s <- gsub("(?<![<>!=])=(?!=)", "==", s, perl = TRUE)
  s <- gsub("\\band\\b", "&", s, ignore.case = TRUE)
  s <- gsub("\\bor\\b", "|", s, ignore.case = TRUE)
  e <- tryCatch(str2lang(s), error = function(err) NULL)
  if (is.null(e)) return(NA)                                 # could not translate
  swap_ops(e)
}

eval_logic <- function(expr, d) {
  n <- nrow(d)
  if (is.null(expr)) return(rep(TRUE, n))
  if (identical(expr, NA)) return(rep(NA, n))
  env <- new.env(parent = baseenv())
  env$.v <- function(f) {
    if (f %in% names(d)) { x <- as.character(d[[f]]); x[is.na(x)] <- ""; x } else rep("", n)
  }
  env$.eq <- cmp_eq; env$.ne <- cmp_ne
  env$.lt <- cmp_rel(`<`); env$.gt <- cmp_rel(`>`)
  env$.le <- cmp_rel(`<=`); env$.ge <- cmp_rel(`>=`)
  out <- tryCatch(eval(expr, env), error = function(e) rep(NA, n))
  out <- as.logical(out)
  if (length(out) == 1 && n != 1) out <- rep(out, n)
  out
}

field_blank <- function(d, f, type) {
  if (type == "checkbox") {
    cols <- grep(paste0("^", f, "___"), names(d), value = TRUE)
    if (!length(cols)) return(rep(NA, nrow(d)))
    m <- do.call(cbind, lapply(cols, function(cc) as.character(d[[cc]]) %in% "1"))
    return(rowSums(m) == 0)
  }
  if (!f %in% names(d)) return(rep(NA, nrow(d)))
  x <- as.character(d[[f]])
  is.na(x) | trimws(x) == ""
}

# ------------------------------------------------------------------------------
# Building the issue table
# ------------------------------------------------------------------------------
make_issues <- function(meta, idx, severity, check, details = "", fields = "") {
  idx <- which(idx)
  if (!length(idx)) return(NULL)
  if (length(details) > 1) details <- details[idx]
  if (length(fields) > 1)  fields  <- fields[idx]
  data.frame(meta[idx, , drop = FALSE], Severity = severity, Check = check,
             Details = details, Fields = fields, stringsAsFactors = FALSE, row.names = NULL)
}

record_meta <- function(d, dd, cfg) {
  g <- function(f) if (f %in% names(d)) { x <- as.character(d[[f]]); x[is.na(x)] <- ""; x } else rep("", nrow(d))
  ra_ch   <- parse_choices(dd$select_choices_or_calculations[dd$field_name == cfg$ra])
  hosp_ch <- parse_choices(dd$select_choices_or_calculations[dd$field_name == "q3_hosp_name_ug"])
  ra_code <- g(cfg$ra)
  ra <- unname(ra_ch[ra_code])
  ra[ra_code == "88"] <- paste0("Other: ", g(cfg$ra_other)[ra_code == "88"])
  ra[is.na(ra) | ra == ""] <- "RA not recorded"
  hosp_code <- g("q3_hosp_name_ug")
  hosp <- unname(hosp_ch[hosp_code]); hosp[is.na(hosp) | hosp == ""] <- "Not recorded"
  dt <- to_date(g("q1_date_survey"))
  dt[is.na(dt)] <- to_date(g("date_today"))[is.na(dt)]
  st <- g(paste0(cfg$form, "_complete"))
  data.frame(Record = g("record_id"), Date = dt, Hospital = hosp, hosp_code = hosp_code, RA = ra,
             `Form status` = c("0" = "Incomplete", "1" = "Unverified", "2" = "Complete")[st] %>%
               unname() %>% coalesce("Incomplete"),
             check.names = FALSE, stringsAsFactors = FALSE)
}

# Skip-logic checks driven by the dictionary
skip_logic_checks <- function(d, dd, meta, cfg, consented) {
  fields <- dd[dd$form_name == cfg$form & !dd$not_asked & dd$field_name != "record_id", ]
  exprs <- lapply(fields$branching_logic, logic_to_r)
  long <- list(); extra <- list(); untranslated <- character(0)
  complete <- meta$`Form status` %in% c("Complete", "Unverified")
  for (i in seq_len(nrow(fields))) {
    f <- fields$field_name[i]
    must <- fields$required_field[i] == "y" || f %in% cfg$important_optional
    vis <- eval_logic(exprs[[i]], d)
    if (all(is.na(vis))) { untranslated <- c(untranslated, f); next }
    blank <- field_blank(d, f, fields$field_type[i])
    if (all(is.na(blank))) next
    if (must) {
      idx <- which(vis & consented & blank)
      if (length(idx)) long[[f]] <- data.frame(row = idx, field = f, label = fields$label_short[i],
                                               required = fields$required_field[i] == "y",
                                               stringsAsFactors = FALSE)
    }
    if (!is.null(exprs[[i]])) {
      idx <- which(!vis & consented & !blank)
      if (length(idx)) extra[[f]] <- data.frame(row = idx, field = f, label = fields$label_short[i],
                                                stringsAsFactors = FALSE)
    }
  }
  long  <- bind_rows(long)
  extra <- bind_rows(extra)
  issues <- list()
  if (nrow(long)) {
    per <- long %>% group_by(row) %>%
      summarise(n_req = sum(required), n_opt = sum(!required),
                fields = paste0(field, collapse = ", "), .groups = "drop")
    sev <- ifelse(complete[per$row], ifelse(per$n_req > 0, "High", "Review"), "In progress")
    det <- paste0(per$n_req, " required", ifelse(per$n_opt > 0, paste0(" + ", per$n_opt, " important optional"), ""),
                  " question(s) blank although the skip logic says they should be asked")
    issues$miss <- data.frame(meta[per$row, , drop = FALSE], Severity = sev,
                              Check = "Question left blank (should have been asked)",
                              Details = det, Fields = per$fields,
                              stringsAsFactors = FALSE, row.names = NULL)
  }
  if (nrow(extra)) {
    per <- extra %>% group_by(row) %>%
      summarise(n = n(), fields = paste0(field, collapse = ", "), .groups = "drop")
    issues$extra <- data.frame(meta[per$row, , drop = FALSE], Severity = "Review",
                               Check = "Answered although it should be skipped",
                               Details = paste0(per$n, " answer(s) recorded for questions the skip logic hides; ",
                                                "an earlier answer was probably changed. Check which answer is correct."),
                               Fields = per$fields, stringsAsFactors = FALSE, row.names = NULL)
  }
  missing_long <- if (nrow(long)) cbind(meta[long$row, c("Record", "Date", "Hospital", "hosp_code", "RA", "Form status")],
                                        long[, c("field", "label", "required")]) else NULL
  list(issues = bind_rows(issues), missing_long = missing_long, untranslated = untranslated)
}

# Checks shared by both tools
common_checks <- function(d, meta, cfg, consented) {
  g <- function(f) if (f %in% names(d)) { x <- as.character(d[[f]]); x[is.na(x)] <- ""; x } else rep("", nrow(d))
  cons_raw <- tolower(g("consent"))
  started  <- g(cfg$key_field) != ""
  out <- list(
    make_issues(meta, cons_raw == "" & started, "Critical", "Consent not recorded",
                "Interview answers are present but the consent question is blank."),
    make_issues(meta, cons_raw %in% c("0", "no") & started, "Critical", "Refusal recorded but data entered",
                "Consent = No but interview answers are present. Confirm consent; delete the data if it was a refusal."),
    make_issues(meta, g(cfg$ra) == "", "Critical", "RA not recorded", "Research officer field is blank."),
    make_issues(meta, consented & g("q2_country") != UGANDA_CODE, "Critical", "Country not Uganda",
                paste0("Q2 country code = '", g("q2_country"), "'. Hospital questions for Uganda were skipped.")),
    make_issues(meta, consented & g("q2_country") == UGANDA_CODE & g("q3_hosp_name_ug") == "", "Critical",
                "Hospital missing", "Q3 hospital name is blank.")
  )
  dt  <- meta$Date
  ts  <- to_date(g("survey_timestamp"))
  out <- c(out, list(
    make_issues(meta, consented & !is.na(dt) & dt > Sys.Date(), "High", "Interview date in the future",
                paste0("Q1 date = ", format(dt, "%d %b %Y"))),
    if (exists("FIELD_START")) make_issues(meta, consented & !is.na(dt) & dt < FIELD_START, "Review",
                                           "Interview date before fieldwork started",
                                           paste0("Q1 date = ", format(dt, "%d %b %Y"))),
    make_issues(meta, consented & !is.na(dt) & !is.na(ts) & abs(as.numeric(dt - ts)) > 1, "Review",
                "Interview date differs from data-entry date",
                paste0("Q1 date ", format(dt, "%d %b"), " vs record created ", format(ts, "%d %b"),
                       ". Confirm the interview date (possible back-dating or late entry).")),
    make_issues(meta, consented & meta$`Form status` == "Incomplete" & !is.na(dt) & dt <= Sys.Date() - STALE_DAYS,
                "High", paste0("Form still incomplete after ", STALE_DAYS, " days"),
                paste0("Interview on ", format(dt, "%d %b"), "; form not marked Complete. Ask the RA to finish or close it."))
  ))
  bind_rows(out)
}

straight_line <- function(d, fields, min_items) {
  fields <- intersect(fields, names(d))
  if (!length(fields) || nrow(d) == 0) return(list(flag = rep(FALSE, nrow(d)), n = rep(0, nrow(d)), val = rep("", nrow(d))))
  m <- do.call(cbind, lapply(fields, function(f) { x <- as.character(d[[f]]); x[is.na(x)] <- ""; x }))
  n <- integer(nrow(m)); k <- integer(nrow(m)); val <- character(nrow(m))
  for (i in seq_len(nrow(m))) {
    r <- m[i, ]; r <- r[r != "" & r != "99"]
    n[i] <- length(r); k[i] <- length(unique(r)); val[i] <- if (length(r)) r[1] else ""
  }
  list(flag = n >= min_items & k == 1, n = n, val = val)
}

dup_groups <- function(key, ok, records) {
  key[!ok] <- NA
  dup <- !is.na(key) & (duplicated(key) | duplicated(key, fromLast = TRUE))
  ids <- ave(records, key, FUN = function(x) paste(x, collapse = ", "))
  list(flag = dup, ids = ids)
}

# ------------------------------------------------------------------------------
# HSR-specific checks
# ------------------------------------------------------------------------------
hsr_checks <- function(d, dd, meta, consented) {
  g <- function(f) if (f %in% names(d)) { x <- as.character(d[[f]]); x[is.na(x)] <- ""; x } else rep("", nrow(d))
  type  <- g("q10a_respondent_type")
  adult <- type == "4"
  carer <- type %in% c("3", "5")
  pick  <- function(a, b) ifelse(carer, g(b), g(a))
  age   <- .num(g("q10_age")); cage <- .num(g("q10_age_caregiver"))
  dt    <- meta$Date
  out <- list(
    make_issues(meta, consented & type == "", "Critical", "Respondent type missing (Q10a)",
                "All patient-experience, discharge and payment sections depend on Q10a, so they were all skipped."),
    make_issues(meta, consented & type %in% c("1", "2"), "Critical", "Respondent type uses a retired code",
                paste0("Q10a = ", type, ". Codes 1/2 are hidden; the skip logic only uses 3, 4 and 5, so most sections were skipped. Recode to 4 (adult) or 5 (caregiver of child).")),
    make_issues(meta, consented & type %in% c("3", "4") & !is.na(age) & age < 18, "High",
                "Age does not match respondent type",
                paste0("Respondent type = ", ifelse(type == "4", "adult patient", "caregiver of adult"),
                       " but patient age = ", age, ".")),
    make_issues(meta, consented & type == "5" & !is.na(age) & age >= 18, "High",
                "Age does not match respondent type",
                paste0("Caregiver of a child (under 18) but patient age = ", age, ".")),
    make_issues(meta, consented & carer & !is.na(cage) & cage < 18, "Review", "Caregiver younger than 18",
                paste0("Caregiver age = ", cage, ". Confirm."))
  )
  # Admission date vs interview date
  adm <- to_date(pick("q22_date_adm", "q22_date_adm_u18"))
  los <- as.numeric(dt - adm)
  out <- c(out, list(
    make_issues(meta, consented & !is.na(los) & los < 0, "High", "Admission date after interview date",
                paste0("Admitted ", format(adm, "%d %b %Y"), ", interviewed ", format(dt, "%d %b %Y"), ".")),
    make_issues(meta, consented & !is.na(los) & los > 90, "Review", "Very long stay (> 90 days)",
                paste0(los, " days between admission and interview. Check the admission date.")),
    make_issues(meta, consented & g("q14a_coming_from") == "1" & g("q14c_sought_care_before") == "0", "Review",
                "Referral answers inconsistent",
                "Came from a lower-level facility (Q14a) but 'sought care elsewhere before' (Q14c) = No.")
  ))
  # Payments
  paid <- pick("q44_admission_fee_paid", "q44_admission_fee_paid_u18") == "1" |
          pick("q45_hospital_cost_paid", "q45_hospital_cost_paid_u18") == "1"
  amt_mat <- function(sfx) do.call(cbind, lapply(1:11, function(k) .num(g(paste0("q45b_svc", k, "_amt", sfx)))))
  ma <- amt_mat(""); mu <- amt_mat("_u18")
  any_a <- rowSums(!is.na(ma)) > 0; any_u <- rowSums(!is.na(mu)) > 0
  sum_a <- rowSums(ma, na.rm = TRUE); sum_u <- rowSums(mu, na.rm = TRUE)
  max_a <- suppressWarnings(apply(ma, 1, max, na.rm = TRUE)); max_u <- suppressWarnings(apply(mu, 1, max, na.rm = TRUE))
  any_amt <- ifelse(carer, any_u, any_a)
  item_sum <- ifelse(carer, sum_u, sum_a)
  item_max <- ifelse(carer, max_u, max_a); item_max[!is.finite(item_max)] <- NA
  total <- .num(pick("q50_amount_paid", "q50_amount_paid_u18"))
  asked <- pick("q35a_asked_to_pay", "q35a_asked_to_pay_u18")
  mism  <- !is.na(total) & total > 0 & item_sum > 0 & abs(item_sum - total) / pmax(item_sum, total) > 0.5
  tcost <- .num(g("transport_cost_total")); dist <- .num(g("q20d_distance_km"))
  out <- c(out, list(
    make_issues(meta, consented & paid & !any_amt, "High", "Paid but no service amounts entered",
                "Q44/Q45 = Yes but every amount in the payment table is blank (these fields are not required, so REDCap does not warn)."),
    make_issues(meta, consented & paid & asked == "0", "Review", "Payment answers inconsistent",
                "Q35a 'asked to pay' = No but Q44/Q45 say money was paid."),
    make_issues(meta, consented & mism, "Review", "Itemised payments do not match total",
                paste0("Sum of service amounts = ", format(item_sum, big.mark = ","), " UGX; total (Q50) = ",
                       format(total, big.mark = ","), " UGX.")),
    make_issues(meta, consented & !is.na(item_max) & item_max > 3e6, "Review", "Very large payment amount",
                paste0("Largest single service amount = ", format(item_max, big.mark = ","), " UGX. Check for extra zeros.")),
    make_issues(meta, consented & !is.na(tcost) & tcost > 3e5, "Review", "Very large transport cost",
                paste0("Transport cost = ", format(tcost, big.mark = ","), " UGX.")),
    make_issues(meta, consented & !is.na(dist) & dist > 300, "Review", "Very long distance from home",
                paste0("Distance = ", dist, " km.")),
    make_issues(meta, consented & g("q20c_travel_time_minutes") == "1" & !is.na(dist) & dist > 50, "Review",
                "Travel time and distance inconsistent",
                paste0("Under 30 minutes of travel but ", dist, " km from home."))
  ))
  # Straight-lining on the responsiveness and nursing-care ratings
  grp <- c("grp_dignity", "grp_autonomy", "grp_confidentiality", "grp_communication",
           "grp_prompt_attension", "grp_quality_basics", "grp_access_family", "grp_nursing_care")
  sa <- straight_line(d, dd$field_name[dd$matrix_group_name %in% grp], 15)
  su <- straight_line(d, dd$field_name[dd$matrix_group_name %in% paste0(grp, "_u18")], 15)
  sl_flag <- ifelse(carer, su$flag, sa$flag); sl_n <- ifelse(carer, su$n, sa$n); sl_v <- ifelse(carer, su$val, sa$val)
  out <- c(out, list(
    make_issues(meta, consented & sl_flag, "Review", "Identical answers to all experience ratings",
                paste0("All ", sl_n, " ratings = ", sl_v, ". Possible straight-lining; observe this RA's interviews."))
  ))
  # Possible duplicates
  key <- paste(meta$hosp_code, meta$Date, g("q10_age"), g("q11_gender"), type,
               pick("clinical_ward", "clinical_ward_u18"), sep = "|")
  du <- dup_groups(key, consented & type != "" & g("q10_age") != "", meta$Record)
  out <- c(out, list(
    make_issues(meta, du$flag, "Review", "Possible duplicate interview",
                paste0("Same hospital, date, age, sex, respondent type and ward as record(s): ", du$ids))
  ))
  bind_rows(out)
}

# ------------------------------------------------------------------------------
# HWF-specific checks
# ------------------------------------------------------------------------------
hwf_checks <- function(d, dd, meta, consented) {
  g <- function(f) if (f %in% names(d)) { x <- as.character(d[[f]]); x[is.na(x)] <- ""; x } else rep("", nrow(d))
  dt   <- meta$Date
  age  <- .num(g("q9_age"))
  qual <- to_date(g("q23_date_qualification"))
  start <- to_date(g("q29_start_date_hospital"))
  promo <- to_date(g("promotion_when"))
  age_q <- age - as.numeric(dt - qual) / 365.25
  yrs_q <- as.numeric(dt - qual) / 365.25
  kids <- .num(g("q13_num_children")); kids18 <- .num(g("q14_num_child_un_18"))
  prof <- g("q22_health_profession")
  phone <- g("q19_last_four_digits")
  absd <- .num(g("q63_num_absent_30d"))
  out <- list(
    make_issues(meta, consented & g("q8_department") == "", "High", "Department missing",
                "Q8 department is blank."),
    make_issues(meta, consented & !is.na(start) & !is.na(qual) & start < qual, "High",
                "Started at hospital before qualifying",
                paste0("Qualified ", format(qual, "%d %b %Y"), "; started here ", format(start, "%d %b %Y"),
                       ". (The REDCap minimum for Q29 points to [date_qualification], a field that does not exist, so it is not enforced.)")),
    make_issues(meta, consented & !is.na(age_q) & age_q < 17, "High", "Qualification date implausible for age",
                paste0("Age ", age, " now implies age ", round(age_q, 1), " at qualification.")),
    make_issues(meta, consented & !is.na(promo) & ((!is.na(qual) & promo < qual) | (!is.na(dt) & promo > dt)),
                "Review", "Promotion date outside career dates",
                paste0("Last promotion ", format(promo, "%d %b %Y"), "; qualified ", format(qual, "%d %b %Y"), ".")),
    make_issues(meta, consented & !is.na(kids) & !is.na(kids18) & kids18 > kids, "High",
                "More children under 18 than children in total",
                paste0("Q13 = ", kids, ", Q14 = ", kids18, ".")),
    make_issues(meta, consented & prof %in% c("2", "3", "6", "7", "9"), "Review", "Profession uses a retired code",
                paste0("Q22 = ", prof, " (hidden choice). Recode to the current option.")),
    make_issues(meta, consented & phone != "" & !grepl("^[0-9]{4}$", phone), "Review",
                "Phone digits not 4 digits", paste0("Q19 = '", phone, "'. Needed to identify repeat interviews.")),
    make_issues(meta, consented & !is.na(absd) & absd > 20, "Review", "Very high absenteeism",
                paste0(absd, " days absent in the past 30 days. Confirm.")),
    make_issues(meta, consented & g("q31_employment_status") == "1" & !is.na(yrs_q) & yrs_q > 5, "Review",
                "Intern but qualified more than 5 years ago",
                paste0("Employment = intern/community service; qualified ", round(yrs_q, 1), " years ago."))
  )
  pq <- straight_line(d, paste0("q46_proqol_", 1:30), 25)
  sv <- straight_line(d, dd$field_name[dd$matrix_group_name == "q53_supervision"], 10)
  out <- c(out, list(
    make_issues(meta, consented & pq$flag, "Review", "Identical answers to all ProQOL items",
                paste0("All ", pq$n, " items = ", pq$val, ". ProQOL has reverse-worded items, so this is very unlikely.")),
    make_issues(meta, consented & sv$flag, "Review", "Identical answers to all supervision items",
                paste0("All ", sv$n, " items = ", sv$val, ". Possible straight-lining."))
  ))
  key <- paste(meta$hosp_code, g("q9_age"), g("q10_gender"), prof, phone, sep = "|")
  du <- dup_groups(key, consented & phone != "" & g("q9_age") != "", meta$Record)
  out <- c(out, list(
    make_issues(meta, du$flag, "Critical", "Possible duplicate health worker",
                paste0("Same hospital, age, sex, profession and phone digits as record(s): ", du$ids,
                       ". The same person may have been interviewed twice."))
  ))
  bind_rows(out)
}

# ------------------------------------------------------------------------------
# Run all checks for one tool
# ------------------------------------------------------------------------------
run_tool_checks <- function(tool) {
  cfg <- CHECK_CFG[[tool]]
  token <- get_token(cfg$token_vars)
  if (!nzchar(token)) {
    return(list(ok = FALSE, msg = paste0("No API token found for ", cfg$label, ". Set one of: ",
                                         paste(cfg$token_vars, collapse = ", "), " in .Renviron.")))
  }
  dd <- load_dict(cfg, token)
  d  <- redcap_post(token, "record", list(type = "flat", rawOrLabel = "raw", rawOrLabelHeaders = "raw",
                                          exportCheckboxLabel = "false", exportSurveyFields = "true"))
  if (nrow(d) == 0) return(list(ok = TRUE, issues = NULL, missing_long = NULL, meta = NULL,
                                n_checked = 0, untranslated = character(0), checked_at = Sys.time()))
  d[] <- lapply(d, function(x) { x <- as.character(x); x[is.na(x)] <- ""; x })
  meta <- record_meta(d, dd, cfg)
  consented <- tolower(d$consent %||% rep("", nrow(d))) %in% c("1", "yes")
  sk <- skip_logic_checks(d, dd, meta, cfg, consented)
  specific <- if (tool == "hsr") hsr_checks(d, dd, meta, consented) else hwf_checks(d, dd, meta, consented)
  issues <- bind_rows(common_checks(d, meta, cfg, consented), sk$issues, specific)
  if (nrow(issues)) {
    issues$Severity <- factor(issues$Severity, levels = SEV_LEVELS)
    issues <- issues %>% arrange(Severity, desc(Date), Record)
  }
  list(ok = TRUE, issues = issues, missing_long = sk$missing_long,
       meta = meta[consented, , drop = FALSE], n_checked = sum(consented),
       untranslated = sk$untranslated, checked_at = Sys.time())
}

run_all_checks <- function() {
  safe <- function(tool) tryCatch(run_tool_checks(tool),
                                  error = function(e) list(ok = FALSE, msg = conditionMessage(e)))
  list(hsr = safe("hsr"), hwf = safe("hwf"))
}
