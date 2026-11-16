# AFRHiCARE Uganda - Round 2 Fieldwork Dashboard

A small R Shiny dashboard that pulls live data from the three REDCap projects
(HSR, HWF, HF Mapping) and shows supervisors how the team is progressing over
the 12 days of fieldwork.

## Files

| File | What it does |
|---|---|
| `app.R` | The dashboard (screens, filters, charts, tables) |
| `redcap_connect.R` | Pulls the 3 projects from the REDCap API and cleans them. **Settings block at the top.** |
| `.Renviron.example` | Template for the REDCap URL and the three API tokens |
| `install_packages.R` | Installs the R packages (run once) |

## Screens

1. **Overview** - completed interviews for HSR and HWF, HF Mapping progress,
   "Day x of 12", completed interviews per day, and a progress table per hospital.
2. **HSR - Patients** - approached / consented / completed / refused, a
   **RA x day table** (what each RA has done each day), an RA summary
   (days worked, completed per day, last interview), completed per day by
   hospital, respondent type by hospital, and ward.
3. **HWF - Health workers** - the same layout, with department and profession.
4. **HF Mapping** - one screen for the 5 hospitals: visit date, forms
   complete, and a colour grid of every form's status per hospital. Department
   forms are marked N/A when the department was not ticked in "Departments available".
5. **Data checks** - records to follow up (consented but not Complete,
   missing hospital/RA/date, patient age that doesn't match respondent type,
   duplicate HF Mapping records). Downloadable as CSV.

The sidebar filters (Hospital, Interview date) apply to every screen.
HF Mapping uses the Hospital filter only. Luweero is excluded everywhere.

## Setup (about 15 minutes)

1. **Get an API token for each project.** In each REDCap project go to
   *User Rights*, give your user (or a dedicated "dashboard" user) **API Export**
   rights, then open *Applications > API* and generate a token. Read-only
   (export) rights are enough.
2. **Create `.Renviron`.** Copy `.Renviron.example` to `.Renviron` in the same
   folder as `app.R` and paste in the three tokens. Check that `REDCAP_URL`
   is your server's API address (normally `https://<your-server>/api/`).
3. **Install packages.** In R: `source("install_packages.R")`.
4. **Check the settings** at the top of `redcap_connect.R`:
   - `FIELD_START`: first day of fieldwork (currently 17 Nov 2026; 12 days).
   - `TARGETS`: optional completed-interview targets per hospital for HSR and HWF.
     When you fill them in, the dashboard adds "% of target" columns.
   - `COMPLETE_STATUSES`: if RAs save forms as "Unverified" when done, use
     `c("Complete", "Unverified")`.
5. **Run it.** Open `app.R` in RStudio and click *Run App*, or in R:
   `shiny::runApp("path/to/folder")`.

Until a token is added, that tool shows **demo data** and a yellow banner says so.

## Sharing it with supervisors

- **On one laptop:** run it in RStudio as above.
- **Online:** deploy the folder (including `.Renviron`) to shinyapps.io, Posit
  Connect, or a Shiny Server at your institution. On shinyapps.io:
  `rsconnect::deployApp("path/to/folder")`. Restrict access to the team, as the
  app reads study data.

## Things to update in REDCap

- **HWF RA names:** `ra_hwf` currently has the placeholder choices `RA1`, `RA2`,
  `RA3`. Replace them with the real names in the HWF data dictionary
  (keep the codes 1, 2, 3, 88) so the RA tables show names.
- **Refusals:** consent comes before the hospital question, so refused
  interviews have no hospital. They count under "All hospitals" but not under
  a single hospital. If you want refusals per hospital, move the hospital
  question (or the RA's hospital) before consent.

## How the numbers are defined

- **Approached** = every record (consented or not).
- **Consented** = consent = Yes. **Refused** = consent = No.
- **Completed** = consented and the form status is Complete.
- **Not yet complete** = consented, but the form is not marked Complete.
- **Interview date** = Q1 date of survey; if blank (e.g. refusals), the hidden
  "Date today" field or the survey timestamp.
- Data refreshes every 15 minutes (`REFRESH_SECONDS`) and with the
  *Refresh now* button.
