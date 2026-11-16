# ==============================================================================
# AFRHiCARE Uganda -- Round 2 Fieldwork Dashboard (LIVE)
# ==============================================================================
# Tools: HSR (patients), HWF (health workers), HF Mapping (5 hospitals)
# BEFORE RUNNING: put REDCAP_URL and the three tokens in .Renviron
# (see .Renviron.example) and check the SETTINGS block in redcap_connect.R.
# ==============================================================================

library(shiny)
library(bslib)
library(dplyr)
library(tidyr)
library(DT)
library(plotly)
library(lubridate)

source("redcap_connect.R")

# ------------------------------------------------------------------------------
# Colours and small helpers
# ------------------------------------------------------------------------------
TOOL_COL <- c("HSR (patients)" = "#2a78d6", "HWF (health workers)" = "#e07b39")
CAT_PAL  <- c("#2a78d6", "#e07b39", "#1baf7a", "#8e5cd9", "#d64b6a", "#a07c12")
STATUS_BG <- c("Complete" = "#1e8e3e", "Unverified" = "#f2b705", "Incomplete" = "#f6a04d",
               "Not started" = "#d93025", "N/A" = "#eceff1")
STATUS_FG <- c("Complete" = "white", "Unverified" = "black", "Incomplete" = "black",
               "Not started" = "white", "N/A" = "#9aa0a6")

hosp_choices <- c("All hospitals" = "All", setNames(names(HOSPITALS), unname(HOSPITALS)))

pct <- function(a, b) if (is.na(b) || b == 0) "-" else paste0(round(100 * a / b), "%")

empty_plot <- function(msg = "No completed interviews for the current filters") {
  plotly_empty(type = "scatter", mode = "markers") |>
    layout(title = list(text = msg, font = list(size = 13)),
           xaxis = list(visible = FALSE), yaxis = list(visible = FALSE))
}

no_rows_table <- function(msg) {
  datatable(data.frame(Message = msg), rownames = FALSE, options = list(dom = "t"))
}

filter_survey <- function(df, hosp, rng) {
  if (nrow(df) == 0) return(df)
  if (hosp != "All") df <- df %>% filter(!is.na(hospital), hospital == hosp)
  df %>% filter(is.na(date) | (date >= rng[1] & date <= rng[2]))
}

# ---- Value boxes for one survey ----
survey_boxes <- function(df) {
  n    <- nrow(df)
  cons <- sum(df$consented)
  ref  <- sum(df$refused)
  comp <- sum(df$completed)
  inc  <- cons - comp
  layout_column_wrap(
    width = 1 / 5, fill = FALSE,
    value_box("Approached", n, showcase = icon("people-group"), theme = "secondary"),
    value_box("Consented", cons, p(class = "small mb-0", pct(cons, n), " of approached"),
              showcase = icon("signature"), theme = "primary"),
    value_box("Completed", comp, p(class = "small mb-0", pct(comp, cons), " of consented"),
              showcase = icon("circle-check"), theme = "success"),
    value_box("Not yet complete", inc, p(class = "small mb-0", "consented, form still open"),
              showcase = icon("hourglass-half"), theme = "warning"),
    value_box("Refused", ref, p(class = "small mb-0", pct(ref, n), " of approached"),
              showcase = icon("ban"), theme = "danger")
  )
}

# ---- RA x day table ----
ra_day_table <- function(df, basis, rng) {
  d <- if (basis == "completed") df %>% filter(completed) else df
  d <- d %>% filter(!is.na(date))
  if (nrow(d) == 0) return(NULL)
  last <- min(rng[2], max(Sys.Date(), max(d$date)))
  if (last < rng[1]) return(NULL)
  days <- seq(rng[1], last, by = "day")
  if (length(days) > 31) days <- tail(days, 31)
  d <- d %>% filter(date %in% days)
  if (nrow(d) == 0) return(NULL)

  tab <- d %>%
    count(ra, date) %>%
    complete(ra, date = days, fill = list(n = 0L)) %>%
    arrange(date) %>%
    mutate(day = format(date, "%a %d %b")) %>%
    select(-date) %>%
    pivot_wider(names_from = day, values_from = n, values_fill = 0L)

  daycols <- setdiff(names(tab), "ra")
  tab$Total <- rowSums(tab[, daycols, drop = FALSE])
  tab <- tab %>% arrange(desc(Total)) %>% rename(RA = ra)
  total_row <- tab %>% summarise(across(-RA, sum)) %>% mutate(RA = "TOTAL")
  list(tab = bind_rows(tab, total_row), daycols = daycols)
}

render_ra_day <- function(res) {
  if (is.null(res)) return(no_rows_table("No interviews for the current filters"))
  datatable(res$tab, rownames = FALSE, class = "compact cell-border",
            options = list(dom = "t", paging = FALSE, ordering = FALSE, scrollX = TRUE)) %>%
    formatStyle(res$daycols,
                backgroundColor = styleInterval(c(0.5, 2.5, 4.5, 6.5),
                                                c("#ffffff", "#dbe9f8", "#a9cbef", "#5d9ee3", "#2a78d6")),
                color = styleInterval(4.5, c("black", "white")),
                textAlign = "center") %>%
    formatStyle("Total", fontWeight = "bold", textAlign = "center") %>%
    formatStyle("RA", target = "row", fontWeight = styleEqual("TOTAL", "bold"))
}

# ---- RA summary table ----
ra_summary <- function(df) {
  if (nrow(df) == 0) return(no_rows_table("No records for the current filters"))
  out <- df %>%
    group_by(RA = ra) %>%
    summarise(Approached = n(),
              Refused = sum(refused),
              Completed = sum(completed),
              `Not yet complete` = sum(consented & !completed),
              `Days worked` = n_distinct(date[!is.na(date)]),
              `Last interview` = if (all(is.na(date))) as.Date(NA) else max(date, na.rm = TRUE),
              .groups = "drop") %>%
    mutate(`Completed per day` = ifelse(`Days worked` > 0, round(Completed / `Days worked`, 1), NA)) %>%
    arrange(desc(Completed))
  datatable(out, rownames = FALSE, class = "compact",
            options = list(dom = "t", paging = FALSE, scrollX = TRUE)) %>%
    formatDate("Last interview", method = "toDateString")
}

# ---- Charts ----
daily_by_hospital <- function(df) {
  d <- df %>% filter(completed, !is.na(date)) %>% count(date, hosp_short)
  if (nrow(d) == 0) return(empty_plot())
  plot_ly(d, x = ~date, y = ~n, color = ~hosp_short, colors = CAT_PAL, type = "bar") |>
    layout(barmode = "stack",
           xaxis = list(title = "", type = "date", tickformat = "%d %b"),
           yaxis = list(title = "Completed interviews"),
           legend = list(orientation = "h", y = -0.25))
}

stack_by_hospital <- function(df, var) {
  d <- df %>% filter(completed) %>%
    mutate(g = coalesce(.data[[var]], "Not recorded")) %>%
    count(hosp_short, g)
  if (nrow(d) == 0) return(empty_plot())
  plot_ly(d, x = ~hosp_short, y = ~n, color = ~g, colors = CAT_PAL, type = "bar") |>
    layout(barmode = "stack", xaxis = list(title = ""),
           yaxis = list(title = "Completed interviews"),
           legend = list(orientation = "h", y = -0.25))
}

bar_by <- function(df, var, colour = "#2a78d6") {
  d <- df %>% filter(completed) %>%
    mutate(v = coalesce(.data[[var]], "Not recorded")) %>%
    count(v)
  if (nrow(d) == 0) return(empty_plot())
  plot_ly(d, y = ~reorder(v, n), x = ~n, type = "bar", orientation = "h",
          marker = list(color = colour), text = ~n, textposition = "outside") |>
    layout(yaxis = list(title = ""), xaxis = list(title = "Completed interviews"))
}

# ------------------------------------------------------------------------------
# UI
# ------------------------------------------------------------------------------
survey_tab <- function(id, title, note, chart3_title, chart4_title) {
  nav_panel(
    title,
    p(class = "text-muted small", note),
    uiOutput(paste0(id, "_boxes")),
    card(
      card_header(
        div(class = "d-flex justify-content-between align-items-center flex-wrap",
            span("What each RA has done per day"),
            radioButtons(paste0(id, "_basis"), NULL, inline = TRUE,
                         choices = c("Completed interviews" = "completed",
                                     "All approached (incl. refusals)" = "all")))
      ),
      DTOutput(paste0(id, "_ra_day"))
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(card_header("Completed interviews per day, by hospital"),
           plotlyOutput(paste0(id, "_daily"), height = "320px")),
      card(card_header("RA summary"), DTOutput(paste0(id, "_ra_sum")))
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(card_header(chart3_title), plotlyOutput(paste0(id, "_c3"), height = "340px")),
      card(card_header(chart4_title), plotlyOutput(paste0(id, "_c4"), height = "340px"))
    )
  )
}

ui <- page_navbar(
  title = "AFRHiCARE Uganda - Round 2 Fieldwork",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  fillable = FALSE,

  sidebar = sidebar(
    width = 270,
    h5("Filters"),
    selectInput("hosp", "Hospital", choices = hosp_choices, selected = "All"),
    dateRangeInput("dates", "Interview date", start = FIELD_START, end = FIELD_END,
                   format = "dd M yyyy"),
    p(class = "text-muted small mt-n2",
      "Refusals have no hospital recorded, so they only appear under 'All hospitals'. ",
      "HF Mapping uses the Hospital filter only."),
    actionButton("refresh", "Refresh now", icon = icon("rotate"),
                 class = "btn-sm btn-outline-primary w-100"),
    hr(),
    uiOutput("sync_status")
  ),

  nav_panel(
    "Overview",
    uiOutput("demo_banner"),
    uiOutput("ov_boxes"),
    card(card_header("Completed interviews per day"),
         plotlyOutput("ov_daily", height = "320px")),
    card(card_header("Progress by hospital"), DTOutput("ov_hosp"))
  ),

  survey_tab("hsr", "HSR - Patients",
             "Health System Responsiveness survey. Completed = consented and form marked Complete.",
             "Respondent type by hospital", "Ward the patient was admitted to"),

  survey_tab("hwf", "HWF - Health workers",
             "Health Workforce survey. Completed = consented and form marked Complete.",
             "Department by hospital", "Health profession"),

  nav_panel(
    "HF Mapping",
    p(class = "text-muted small",
      "One record expected per hospital. Department forms are only expected if the department was ticked ",
      "in 'Departments available'; until that form is filled, all department forms are counted as expected."),
    uiOutput("hfm_boxes"),
    card(card_header("Hospital summary"), DTOutput("hfm_summary")),
    card(
      card_header("Form status by hospital"),
      p(class = "small mb-2",
        span(style = "background:#1e8e3e;color:white;padding:2px 6px;border-radius:3px;", "Complete"), " ",
        span(style = "background:#f2b705;padding:2px 6px;border-radius:3px;", "Unverified"), " ",
        span(style = "background:#f6a04d;padding:2px 6px;border-radius:3px;", "Incomplete"), " ",
        span(style = "background:#d93025;color:white;padding:2px 6px;border-radius:3px;", "Not started"), " ",
        span(style = "background:#eceff1;color:#5f6368;padding:2px 6px;border-radius:3px;", "N/A - department not available")),
      DTOutput("hfm_grid")
    )
  ),

  nav_panel(
    "Data checks",
    p(class = "text-muted small",
      "Records the supervisors should follow up with the RA. Uses the Hospital and date filters ",
      "(records with no date are always shown)."),
    div(class = "mb-2", downloadButton("checks_dl", "Download list (CSV)", class = "btn-sm")),
    card(DTOutput("checks"))
  )
)

# ------------------------------------------------------------------------------
# SERVER
# ------------------------------------------------------------------------------
server <- function(input, output, session) {

  # ---- Data: load at start, then on timer or button ----
  dat <- reactiveVal(load_all_data())
  timer <- reactiveTimer(REFRESH_SECONDS * 1000)
  observeEvent(timer(), dat(load_all_data()), ignoreInit = TRUE)
  observeEvent(input$refresh, {
    showNotification("Refreshing from REDCap...", duration = 2)
    dat(load_all_data())
  })

  rng <- reactive({
    req(input$dates)
    r <- as.Date(input$dates)
    validate(need(!any(is.na(r)), "Pick a start and end date"))
    r
  })

  hsr_f <- reactive(filter_survey(dat()$hsr, input$hosp, rng()))
  hwf_f <- reactive(filter_survey(dat()$hwf, input$hosp, rng()))

  # ---- Sidebar status ----
  output$sync_status <- renderUI({
    d <- dat()
    tags$div(
      class = "small",
      p(class = "mb-1", icon("clock"), " Last synced: ", format(d$synced, "%d %b %H:%M")),
      p(class = "text-muted mb-1", paste0("Auto-refreshes every ", REFRESH_SECONDS / 60, " minutes.")),
      if (any(d$demo)) p(class = "text-warning mb-1", icon("flask"),
                         " Demo data: ", paste(toupper(names(d$demo)[d$demo]), collapse = ", ")),
      if (length(d$errors) > 0)
        lapply(names(d$errors), function(n) p(class = "text-danger mb-1", icon("triangle-exclamation"),
                                                paste0(" ", n, ": ", d$errors[[n]])))
    )
  })

  output$demo_banner <- renderUI({
    d <- dat()
    if (!any(d$demo)) return(NULL)
    div(class = "alert alert-warning py-2",
        strong("Demo data. "),
        "No API token set for: ", paste(toupper(names(d$demo)[d$demo]), collapse = ", "),
        ". These numbers are made up. Add the tokens to .Renviron and restart the app.")
  })

  # ---- Overview ----
  output$ov_boxes <- renderUI({
    h <- hsr_f(); w <- hwf_f(); s <- hfm_s()
    tgt <- TARGETS
    if (input$hosp != "All") tgt <- tgt[tgt$hospital == input$hosp, ]
    t_hsr <- if (all(is.na(tgt$hsr))) NA else sum(tgt$hsr, na.rm = TRUE)
    t_hwf <- if (all(is.na(tgt$hwf))) NA else sum(tgt$hwf, na.rm = TRUE)

    day <- as.integer(Sys.Date() - FIELD_START) + 1
    day_txt <- if (day < 1) paste("Starts", format(FIELD_START, "%d %b")) else
      if (day > FIELD_DAYS) "Fieldwork ended" else paste("Day", day, "of", FIELD_DAYS)

    layout_column_wrap(
      width = 1 / 4, fill = FALSE,
      value_box("HSR completed (patients)", sum(h$completed),
                p(class = "small mb-0",
                  if (!is.na(t_hsr)) paste0(pct(sum(h$completed), t_hsr), " of target ", t_hsr, " | "),
                  sum(h$consented & !h$completed), " open, ", sum(h$refused), " refused"),
                showcase = icon("bed-pulse"), theme = "primary"),
      value_box("HWF completed (health workers)", sum(w$completed),
                p(class = "small mb-0",
                  if (!is.na(t_hwf)) paste0(pct(sum(w$completed), t_hwf), " of target ", t_hwf, " | "),
                  sum(w$consented & !w$completed), " open, ", sum(w$refused), " refused"),
                showcase = icon("user-doctor"), theme = "warning"),
      value_box("HF Mapping fully complete", paste0(sum(s$pct_complete == 100), " / ", nrow(s)),
                p(class = "small mb-0", "hospitals; average ",
                  if (nrow(s) > 0) round(mean(s$pct_complete)) else 0, "% of forms complete"),
                showcase = icon("hospital"), theme = "success"),
      value_box("Fieldwork", day_txt,
                p(class = "small mb-0", format(FIELD_START, "%d %b"), " - ", format(FIELD_END, "%d %b %Y")),
                showcase = icon("calendar-days"), theme = "secondary")
    )
  })

  output$ov_daily <- renderPlotly({
    h <- hsr_f(); w <- hwf_f()
    d <- bind_rows(
      h %>% filter(completed, !is.na(date)) %>% count(date) %>% mutate(tool = "HSR (patients)"),
      w %>% filter(completed, !is.na(date)) %>% count(date) %>% mutate(tool = "HWF (health workers)")
    )
    if (nrow(d) == 0) return(empty_plot())
    plot_ly(d, x = ~date, y = ~n, color = ~tool, colors = TOOL_COL, type = "bar",
            text = ~n, textposition = "outside") |>
      layout(barmode = "group",
             xaxis = list(title = "", type = "date", tickformat = "%d %b"),
             yaxis = list(title = "Completed interviews"),
             legend = list(orientation = "h", y = -0.2))
  })

  output$ov_hosp <- renderDT({
    h <- hsr_f(); w <- hwf_f(); s <- hfm_s()
    base <- tibble(hospital = names(HOSPITALS), Hospital = unname(HOSPITALS))
    if (input$hosp != "All") base <- base %>% filter(hospital == input$hosp)

    hs <- h %>% filter(!is.na(hospital)) %>% group_by(hospital) %>%
      summarise(`HSR completed` = sum(completed), `HSR open` = sum(consented & !completed), .groups = "drop")
    ws <- w %>% filter(!is.na(hospital)) %>% group_by(hospital) %>%
      summarise(`HWF completed` = sum(completed), `HWF open` = sum(consented & !completed), .groups = "drop")
    ms <- s %>% transmute(hospital, `HF Mapping forms` = paste0(forms_complete, " / ", forms_expected),
                          `HF Mapping %` = pct_complete)

    out <- base %>% left_join(hs, by = "hospital") %>% left_join(ws, by = "hospital") %>%
      mutate(across(c(`HSR completed`, `HSR open`, `HWF completed`, `HWF open`), ~ coalesce(as.integer(.), 0L)))

    if (!all(is.na(TARGETS$hsr))) {
      out <- out %>% left_join(TARGETS %>% select(hospital, `HSR target` = hsr), by = "hospital") %>%
        mutate(`HSR % of target` = ifelse(is.na(`HSR target`) | `HSR target` == 0, NA,
                                          round(100 * `HSR completed` / `HSR target`)))
    }
    if (!all(is.na(TARGETS$hwf))) {
      out <- out %>% left_join(TARGETS %>% select(hospital, `HWF target` = hwf), by = "hospital") %>%
        mutate(`HWF % of target` = ifelse(is.na(`HWF target`) | `HWF target` == 0, NA,
                                          round(100 * `HWF completed` / `HWF target`)))
    }
    out <- out %>% left_join(ms, by = "hospital") %>% select(-hospital)

    datatable(out, rownames = FALSE, class = "compact",
              options = list(dom = "t", paging = FALSE, scrollX = TRUE)) %>%
      formatStyle("HF Mapping %",
                  background = styleColorBar(c(0, 100), "#cfe8d5"),
                  backgroundSize = "98% 70%", backgroundRepeat = "no-repeat",
                  backgroundPosition = "center")
  })

  # ---- HSR tab ----
  output$hsr_boxes <- renderUI(survey_boxes(hsr_f()))
  output$hsr_ra_day <- renderDT(render_ra_day(ra_day_table(hsr_f(), input$hsr_basis, rng())))
  output$hsr_ra_sum <- renderDT(ra_summary(hsr_f()))
  output$hsr_daily <- renderPlotly(daily_by_hospital(hsr_f()))
  output$hsr_c3 <- renderPlotly(stack_by_hospital(hsr_f(), "respondent_group"))
  output$hsr_c4 <- renderPlotly(bar_by(hsr_f(), "ward", "#2a78d6"))

  # ---- HWF tab ----
  output$hwf_boxes <- renderUI(survey_boxes(hwf_f()))
  output$hwf_ra_day <- renderDT(render_ra_day(ra_day_table(hwf_f(), input$hwf_basis, rng())))
  output$hwf_ra_sum <- renderDT(ra_summary(hwf_f()))
  output$hwf_daily <- renderPlotly(daily_by_hospital(hwf_f()))
  output$hwf_c3 <- renderPlotly(stack_by_hospital(hwf_f(), "department"))
  output$hwf_c4 <- renderPlotly(bar_by(hwf_f(), "profession", "#e07b39"))

  # ---- HF Mapping tab ----
  hfm_s <- reactive({
    s <- dat()$hfm_summary
    if (input$hosp != "All") s <- s %>% filter(hospital == input$hosp)
    s
  })
  hfm_l <- reactive({
    l <- dat()$hfm_long
    if (input$hosp != "All") l <- l %>% filter(hospital == input$hosp)
    l
  })

  output$hfm_boxes <- renderUI({
    s <- hfm_s()
    layout_column_wrap(
      width = 1 / 3, fill = FALSE,
      value_box("Hospitals visited", paste0(sum(s$records > 0), " / ", nrow(s)),
                showcase = icon("hospital"), theme = "primary"),
      value_box("Fully complete", paste0(sum(s$pct_complete == 100), " / ", nrow(s)),
                showcase = icon("circle-check"), theme = "success"),
      value_box("Forms complete", paste0(sum(s$forms_complete), " / ", sum(s$forms_expected)),
                p(class = "small mb-0", pct(sum(s$forms_complete), sum(s$forms_expected)), " of expected forms"),
                showcase = icon("list-check"), theme = "secondary")
    )
  })

  output$hfm_summary <- renderDT({
    s <- hfm_s()
    if (nrow(s) == 0) return(no_rows_table("No hospitals selected"))
    out <- s %>% transmute(
      Hospital = hosp_short,
      `Record ID` = coalesce(record_id, "-"),
      `Visit date` = ifelse(is.na(visit_date), "-", format(visit_date, "%d %b %Y")),
      `Forms complete` = paste0(forms_complete, " / ", forms_expected),
      `% complete` = pct_complete,
      `Departments recorded?` = ifelse(depts_recorded, "Yes", "Not yet"),
      Note = case_when(records == 0 ~ "No record yet",
                       records > 1 ~ paste(records, "records - check for duplicates"),
                       TRUE ~ "")
    )
    datatable(out, rownames = FALSE, class = "compact",
              options = list(dom = "t", paging = FALSE, scrollX = TRUE)) %>%
      formatStyle("% complete",
                  background = styleColorBar(c(0, 100), "#cfe8d5"),
                  backgroundSize = "98% 70%", backgroundRepeat = "no-repeat",
                  backgroundPosition = "center")
  })

  output$hfm_grid <- renderDT({
    l <- hfm_l()
    if (nrow(l) == 0) return(no_rows_table("No hospitals selected"))
    hosp_cols <- unique(l$hosp_short)
    g <- l %>%
      select(Type = type, Form = form_label, hosp_short, status) %>%
      pivot_wider(names_from = hosp_short, values_from = status)
    datatable(g, rownames = FALSE, class = "compact cell-border",
              options = list(dom = "t", paging = FALSE, ordering = FALSE, scrollX = TRUE)) %>%
      formatStyle(hosp_cols,
                  backgroundColor = styleEqual(names(STATUS_BG), unname(STATUS_BG)),
                  color = styleEqual(names(STATUS_FG), unname(STATUS_FG)),
                  textAlign = "center")
  })

  # ---- Data checks ----
  checks_f <- reactive({
    ck <- dat()$checks
    if (nrow(ck) == 0) return(ck)
    if (input$hosp != "All") ck <- ck %>% filter(Hospital == HOSPITALS[[input$hosp]])
    r <- rng()
    ck %>% filter(is.na(Date) | (Date >= r[1] & Date <= r[2]))
  })

  output$checks <- renderDT({
    ck <- checks_f()
    if (nrow(ck) == 0) return(no_rows_table("Nothing to follow up for the current filters"))
    datatable(ck %>% mutate(Date = format(Date, "%d %b %Y")), rownames = FALSE, class = "compact",
              filter = "top", options = list(pageLength = 25, scrollX = TRUE))
  })

  output$checks_dl <- downloadHandler(
    filename = function() paste0("AFRHiCARE_UG_data_checks_", format(Sys.Date(), "%Y%m%d"), ".csv"),
    content = function(file) write.csv(checks_f(), file, row.names = FALSE, na = "")
  )
}

shinyApp(ui, server)
