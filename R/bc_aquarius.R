# R/bc_aquarius.R
#
# hydrocan adapter for the BC Government Aquarius hydrometric network.
#
# The Aquarius export endpoint is public and requires no authentication.
# The station list is hardcoded from a portal scrape on 2026-05-19,
# filtered to Classification %in% c("Hydrometric (Other)",
# "Hydrometric (Provincial Network)"). The Data_List endpoint that serves
# the portal's station table requires a session cookie, so it cannot be
# queried programmatically. Refresh the list manually if the station
# network changes.
#
# Daily aggregation is handled server-side by Aquarius (IntervalPoints=Daily).
# Sub-daily data is returned at the native logger interval (typically 5 min).
# There is no separate hourly tier in the hydrocan adapter contract;
# hc_read_flows() covers all sub-daily data.
#
# Approval level is fetched for sub-daily data and mapped to quality_code
# (raw numeric string) and qf_desc (human-readable label). The mapping is
# documented in every CSV header:
#   800=WORKING, 900=IN REVIEW, 950=REVIEWED, 1200=APPROVED
#
# Note: the daily Aquarius export does not include an approval level column,
# so quality_code and qf_desc are NA for daily data.


# ---------------------------------------------------------------------------
# Constants
# ---------------------------------------------------------------------------

.BC_AQUARIUS_UNIT_DISCHARGE <- 350L  # m3/s
.BC_AQUARIUS_UNIT_STAGE     <- 306L  # m

# Approval level mapping documented in Aquarius CSV header line 2
.BC_AQUARIUS_APPROVAL_MAP <- c(
  "800"  = "WORKING",
  "900"  = "IN REVIEW",
  "950"  = "REVIEWED",
  "1200" = "APPROVED"
)

# Station list extracted from BC Gov Aquarius portal Data_List endpoint.
# Retrieved 2026-05-19, filtered to Hydrometric (Other) and
# Hydrometric (Provincial Network). Total: 742 stations.
# The Data_List endpoint requires authentication so cannot be queried
# programmatically. Refresh manually if the station network changes.
.BC_AQUARIUS_STATIONS <- c(
  "07EA0002", "07FA0001", "07FA0002", "07FA0003", "07FB0001", "07FC0001",
  "07FC0002", "07FC0003", "07FC0004", "07FC0009", "07FD0001", "08DC0001",
  "08DC0002", "08EE0001", "08EE0002", "08EE0003", "08GA0001", "08GA0005",
  "08GB0001", "08HA0001", "08HA0002", "08HA0003", "08HA0004", "08HA0005",
  "08HA0006", "08HA0007", "08HA0008", "08HA0009", "08HA0010", "08HA0011",
  "08HA0012", "08HA0013", "08HA0014", "08HA0015", "08HA0016", "08HA0017",
  "08HA0018", "08HA0019", "08HA0020", "08HA0021", "08HA0022", "08HA0023",
  "08HA0024", "08HA0025", "08HA0026", "08HA0027", "08HA0028", "08HA0029",
  "08HA003",  "08HA0032", "08HA0033", "08HA0034", "08HA0035", "08HA0036",
  "08HA0037", "08HA0038", "08HA0039", "08HA0040", "08HA0041", "08HA0042",
  "08HA0043", "08HA0044", "08HA0045", "08HA040",  "08HB0001", "08HB0002",
  "08HB0003", "08HB0004", "08HB0005", "08HB0006", "08HB0007", "08HB0008",
  "08HB0009", "08HB0010", "08HB0011", "08HB0012", "08HB0013", "08HB0014",
  "08HB0015", "08HB0016", "08HB0017", "08HB0018", "08HB0019", "08HB0020",
  "08HB0021", "08HB0022", "08HB0023", "08HB0024", "08HB0025", "08HB0026",
  "08HB0027", "08HB0030", "08HB0031", "08HB0032", "08HB0033", "08HB0034",
  "08HB0035", "08HB0036", "08HB0037", "08HB0038", "08HB0039", "08HB0040",
  "08HB059",  "08HC0001", "08HC0002", "08HC0003", "08HC0004", "08HD0001",
  "08HD0002", "08HD0003", "08HF0001", "08JC0001", "08JC0002", "08JC0003",
  "08JC0004", "08JC0005", "08JC0006", "08JC0007", "08JC0008", "08JC0009",
  "08JC0010", "08JC0011", "08JC0012", "08JC0013", "08JC0014", "08JC0015",
  "08JC0016", "08JC0017", "08JC0018", "08JC0019", "08JC0020", "08LB0001",
  "08LB0002", "08LB0003", "08LB0004", "08LB0005", "08LB0006", "08LB0007",
  "08LB0008", "08LB0009", "08LB0010", "08LB0011", "08LB0012", "08LB0013",
  "08LB0014", "08LB0015", "08LB0016", "08LC0001", "08LC0002", "08LC0003",
  "08LC0004", "08LE0001", "08LE0002", "08LE0003", "08LE0006", "08LEE004",
  "08LF0001", "08LF0002", "08LF0003", "08LF0004", "08LF0005", "08LF0006",
  "08LF0007", "08LF0008", "08LF0009", "08LF0010", "08LF0011", "08LG0001",
  "08LG0002", "08LG0003", "08LG0004", "08LG0005", "08LG0006", "08LG0007",
  "08LG0008", "08LG0009", "08LG0011", "08LG0012", "08LG0013", "08LG0015",
  "08LG0016", "08MB0001", "08MB0002", "08MB0003", "08MB0004", "08MC0001",
  "08MD0001", "08MD0002", "08MD0003", "08MF0001", "08MF0002", "08MF0003",
  "08MF0004", "08MG0001", "08MG0002", "08MG0003", "08MG0004", "08MG0005",
  "08MH0001", "08MH0002", "08MH0003", "08MH0004", "08MH0005", "08MH0006",
  "08MH0007", "08MH0008", "08MH0009", "08MH0010", "08MH0011", "08MH0012",
  "08MH0013", "08MH0014", "08MH0015", "08MH0016", "08MH0017", "08MH0018",
  "08MH0019", "08MH0020", "08MH0021", "08MH0022", "08MH0023", "08MH0024",
  "08MH0025", "08MH0026", "08MH0027", "08MH0028", "08MH0029", "08MH0030",
  "08MH0031", "08MH0032", "08MH0033", "08MH0034", "08MH0035", "08MH0036",
  "08MH0037", "08MH0038", "08MH0039", "08MH0040", "08MH0041", "08MH0042",
  "08MH0044", "08MH0045", "08MH0046", "08MH0047", "08MH0048", "08MH0049",
  "08MH0050", "08MH0051", "08MH0052", "08MH0053", "08MH0054", "08MH0055",
  "08MH0056", "08MH0057", "08MH0058", "08MH0059", "08MH0060", "08MH0061",
  "08MH0062", "08MH0063", "08MH0064", "08NA0002", "08NE0001", "08NH0001",
  "08NH0003", "08NJ0001", "08NK0001", "08NK0002", "08NK0003", "08NK0004",
  "08NK0005", "08NK0006", "08NK0007", "08NK0008", "08NK0009", "08NK0010",
  "08NK0011", "08NK0012", "08NK0013", "08NK0014", "08NL0001", "08NL0002",
  "08NM0002", "08NM0003", "08NM0004", "08NM0005", "08NM0006", "08NM0007",
  "08NM0008", "08NM0009", "08NM0010", "08NM0011", "08NM0012", "08NM0013",
  "08NM0014", "08NM0015", "08NM0017", "08NM0018", "08NM0019", "08NM0020",
  "08NM0021", "08NM0022", "08NM0023", "08NM0024", "08NM0025", "08NM0026",
  "08NM0027", "08NM0028", "08NM0029", "08NM0030", "08NM0031", "08NM0032",
  "08NM0033", "08NM0034", "08NM0035", "08NM0036", "08NM0037", "08NM0038",
  "08NM0039", "08NM0040", "08NM0041", "08NM0043", "08NM0044", "08NM0047",
  "08NM0049", "08NM0050", "08NM0051", "08NM0052", "08NM0053", "08NM0054",
  "08NM0055", "08NM0056", "08NM0057", "08NM0058", "08NM0059", "08NM0060",
  "08NM0064", "08NM0065", "10CD0001", "10CD0002", "1AHA001",  "1AHA002",
  "1AHA003",  "1AHA007",  "1AHA008",  "1AHA009",  "1AHA010",  "1AHA011",
  "1AHA013",  "1AHA015",  "1AHA016",  "1AHA017",  "1AHA018",  "1AHA019",
  "1AHA020",  "1AHA021",  "1AHA022",  "1AHA023",  "1AHA024",  "1AHA025",
  "1AHA026",  "1AHA028",  "1AHA040",  "1AHA040-FV", "1AHA045", "1AHA045-FV",
  "1AHA047",  "1AHA052",  "1AHB001",  "1AHB002",  "1AHB003",  "1AHB004",
  "1AHB005",  "1AHB006",  "1AHB007",  "1AHB008",  "1AHB009",  "1AHB010",
  "1AHB011",  "1AHB012",  "1AHB013",  "1AHB014",  "1AHB015",  "1AHB016",
  "1AHB017",  "1AHB018",  "1AHB019",  "1AHB020",  "1AHB021",  "1AHB022",
  "1AHB023",  "1AHB024",  "1AHB025",  "1AHB026",  "1AHB027",  "1AHB028",
  "1AHB029",  "1AHB030",  "1AHB032",  "1AHB033",  "1AHB036",  "1AHB048",
  "1AHB049",  "1AHC001",  "1AHD001",  "1AHD002",  "1AHD003",  "1AHD004",
  "1AHD005",  "1AHD006",  "1AHD008",  "1AHD009",  "1AHD010",  "1AHD011",
  "1AHD013",  "1AHD014",  "2AGA001",  "2AGA002",  "2AGA003",  "2AGA004",
  "2AGA007",  "2AGA008",  "2AGA009",  "2AGB001",  "2AGB002",  "2AGB003",
  "2AGB004",  "2AGB005",  "2AGB007",  "2AMH002",  "2AMH003",  "2AMH004",
  "2AMH005",  "2AMH006",  "2AMH007",  "2AMH008",  "2AMH009",  "2AMH010",
  "2AMH011",  "2AMH012",  "2AMH013",  "2AMH014",  "2AMH015",  "2AMH016",
  "3ALB001",  "3ALB002",  "3ALB003",  "3ALB004",  "3ALB005",  "3ALB006",
  "3ALB007",  "3ALB008",  "3ALB009",  "3ALB010",  "3ALB011",  "3ALB012",
  "3ALB013",  "3ALB014",  "3ALB015",  "3ALB016",  "3ALB017",  "3ALB018",
  "3ALB019",  "3ALB020",  "3ALB021",  "3ALB023",  "3ALE001",  "3ALE002",
  "3ALE003",  "3ALE004",  "3ALE005",  "3ALE006",  "3ALE007",  "3ALE008",
  "3ALE009",  "3ALE010",  "3ALE011",  "3ALE012",  "3ALE013",  "3ALE015",
  "3ALE016",  "3ALE017",  "3ALE018",  "3ALE019",  "3ALE020",  "3ALE021",
  "3ALE022",  "3ALE023",  "3ALE026",  "3ALE027",  "3ALE028",  "3ALE029",
  "3ALE030",  "3ALE031",  "3ALE032",  "3ALE034",  "3ALE039",  "3ALE040",
  "3ALF001",  "3ALF003",  "3ALF004",  "3ALF005",  "3ALF006",  "3ALF007",
  "3ALF008",  "3ALF009",  "3ALF010",  "3ALF011",  "3ALF012",  "3ALF013",
  "3ALF014",  "3ALF015",  "3ALF016",  "3ALF017",  "3ALF018",  "3ALF019",
  "3ALF020",  "3ALF021",  "3ALF022",  "3ALF023",  "3ALF024",  "3ALF025",
  "3ALF030",  "3ALF032",  "3ALF035",  "3ALG001",  "3ALG002",  "3ALG003",
  "3ALG004",  "3ALG005",  "3ALG006",  "3ALG007",  "3ALG008",  "3ALG010",
  "3ALG011",  "3ALG012",  "3ALG013",  "3ALG014",  "3ALG015",  "3AME001",
  "3AMF001",  "3AMF002",  "3AMF003",  "3AMF004",  "3BLC001",  "3BLC002",
  "3BLC003",  "3BLC004",  "3BLC005",  "3BLC006",  "3BLC007",  "3BLC008",
  "3BLC010",  "3BLC011",  "3BNL001",  "3BNL002",  "3BNL003",  "3BNL004",
  "3BNL005",  "3BNL006",  "3BNL007",  "3BNL008",  "3BNL009",  "3BNL010",
  "3BNL011",  "3BNM006",  "3BNM007",  "3BNM008",  "3BNM009",  "3BNM010",
  "3BNM011",  "3BNM012",  "3BNM013",  "3BNM014",  "3BNM015",  "3BNM016",
  "3BNM017",  "3BNM018",  "3BNM019",  "3BNM020",  "3BNM021",  "3BNM022",
  "3BNM023",  "3BNM024",  "3BNM025",  "3BNM026",  "3BNM027",  "3BNM028",
  "3BNM029",  "3BNM030",  "3BNM031",  "3BNM033",  "3BNM034",  "3BNM035",
  "3BNM036",  "3BNM037",  "3BNM038",  "3BNM039",  "3BNM040",  "3BNM041",
  "3BNM042",  "3BNM043",  "3BNM044",  "3BNM045",  "3BNM046",  "3BNM047",
  "3BNM048",  "3BNM049",  "3BNM050",  "3BNM051",  "3BNM052",  "3BNM053",
  "3BNM054",  "3BNM055",  "3BNM056",  "3BNM057",  "3BNM058",  "3BNM059",
  "3BNM060",  "3BNM061",  "3BNM062",  "3BNM063",  "3BNM064",  "3BNM065",
  "3BNM066",  "3BNN001",  "3BNN002",  "3BNN003",  "3BNN004",  "3BNN005",
  "3BNN006",  "3BNN007",  "3BNN008",  "3BNN009",  "3BNN010",  "3BNN013",
  "4ANA001",  "4ANA002",  "4ANA003",  "4ANA004",  "4ANB001",  "4ANE001",
  "4ANE002",  "4ANE003",  "4ANE004",  "4ANE005",  "4ANE006",  "4ANE007",
  "4ANE008",  "4ANG001",  "4ANH002",  "4ANH003",  "4ANH004",  "4ANH005",
  "4ANH006",  "4ANH007",  "4ANJ001",  "4ANJ002",  "4ANJ003",  "4ANJ004",
  "4ANJ005",  "4ANJ006",  "4ANJ007",  "4ANJ008",  "4ANK002",  "4ANK003",
  "5AKB001",  "5AKB002",  "5BGD001",  "5BKE001",  "5BKE002",  "5BKE003",
  "5BKE004",  "5BKE006",  "5BKE009",  "5BKE010",  "5BKE011",  "5BKE013",
  "5BKH003",  "5BKH004",  "5BKH005",  "5BKH006",  "5BKH008",  "5BLA001",
  "5BLA002",  "5BLA003",  "5BLA004",  "5BLA005",  "5BLA006",  "5BLA007",
  "5BLA008",  "5BLA009",  "5BLA010",  "5BLA011",  "5BLA012",  "5BLA013",
  "5BLA014",  "5BLA015",  "5BLA016",  "5BLA017",  "5BLA018",  "5BLA019",
  "5BLA020",  "5BLA022",  "5BLA023",  "5BLA024",  "5BLA026",  "5BLA029",
  "5BLA030",  "5BLA032",  "5BLA036",  "5BLA037",  "5BLF001",  "5BMB001",
  "5BMC001",  "5BMC002",  "5BMC003",  "5BMC004",  "5BMC005",  "5BMC006",
  "5BMC007",  "5BMC008",  "5BMC009",  "5BMC011",  "5BMC012",  "5BMC014",
  "5BMC018",  "6AEE001",  "6AEE002",  "6AEE003",  "6AEE004",  "6AEE005",
  "6AEE006",  "6AEE007",  "6AEE009",  "BEND01",   "H08KC0626", "H08KC0693",
  "H08KC0703", "H08KC0708", "H08KC0844", "ISHK01"
)

# Suppress R CMD check notes for dplyr column names used in mutate/select
utils::globalVariables(c(
  "timestamp", "end_timestamp", "value", "approval_level",
  "parameter", "unit", "provider_name", "quality_code", "qf_desc"
))


# ---------------------------------------------------------------------------
# Internal helpers — HTTP layer
# ---------------------------------------------------------------------------

# Build the Aquarius export URL for a given station and parameter.
# interval: one of "PointsAsRecorded" (default, native logger rate),
#           "Hourly", or "Daily".
.build_aquarius_url <- function(stn_number, parameter, start_time, end_time,
                                unit_id  = 350L,
                                interval = "PointsAsRecorded") {
  parameter  <- tools::toTitleCase(parameter)
  data_set   <- glue::glue("{parameter}.Working@{stn_number}")
  conversion <- if (interval == "Daily") "Aggregate" else "Instantaneous"

  httr2::url_modify(
    "https://bcmoe-prod.aquaticinformatics.net/Export/DataSet",
    query = list(
      DataSet            = data_set,
      Calendar           = "CALENDARYEAR",
      StartTime          = start_time,
      EndTime            = end_time,
      DateRange          = "Custom",
      UnitID             = unit_id,
      Conversion         = conversion,
      IntervalPoints     = interval,
      ApprovalLevels     = "True",
      Qualifiers         = "False",
      Step               = 1,
      ExportFormat       = "csv",
      Compressed         = "false",
      RoundData          = "False",
      GradeCodes         = "False",
      InterpolationTypes = "False",
      Timezone           = 0
    )
  )
}

# Fetch data from Aquarius and return a tibble with columns:
#   timestamp, value, quality_code, qf_desc
#
# Aquarius produces different CSV formats depending on interval:
#
#   Sub-daily: Timestamp (UTC), Value, Approval Level       (3 columns)
#   Daily:     Start of Interval (UTC), End of Interval (UTC), Value  (3 columns)
#
# Both have 3 columns so we detect format by reading the header line (line 6)
# and checking for "Start of Interval". The daily export does not include an
# approval level column; quality_code and qf_desc are NA for daily data.
.get_aquarius_data <- function(...) {
  url   <- .build_aquarius_url(...)
  tfile <- tempfile(fileext = ".csv")

  resp <- httr2::request(url) |>
    httr2::req_user_agent(
      "https://github.com/HakaiInstitute/hakai-streamflow-data"
    ) |>
    httr2::req_perform(path = tfile)

  httr2::resp_check_status(resp)

  # Read header line (line 6) to detect CSV format
  header_line  <- readLines(tfile, n = 6)[6]
  is_daily     <- grepl("Start of Interval", header_line, fixed = TRUE)
  has_approval <- grepl("Approval", header_line, fixed = TRUE)

  if (is_daily) {
    # Daily: Start of Interval, End of Interval, Value
    # No approval column — quality fields are NA
    readr::read_csv(
      tfile,
      skip      = 6,
      col_types = "TTd",
      col_names = c("timestamp", "end_timestamp", "value")
    ) |>
      dplyr::select(-end_timestamp) |>
      dplyr::mutate(
        quality_code = NA_character_,
        qf_desc      = NA_character_
      )

  } else if (has_approval) {
    # Sub-daily with approval: Timestamp, Value, Approval Level
    readr::read_csv(
      tfile,
      skip      = 6,
      col_types = "Tdc",
      col_names = c("timestamp", "value", "approval_level")
    ) |>
      dplyr::mutate(
        quality_code = approval_level,
        qf_desc      = dplyr::recode(
          approval_level,
          !!!.BC_AQUARIUS_APPROVAL_MAP,
          .default = NA_character_
        )
      ) |>
      dplyr::select(-approval_level)

  } else {
    # Fallback: Timestamp, Value only
    readr::read_csv(
      tfile,
      skip      = 6,
      col_types = "Td",
      col_names = c("timestamp", "value")
    ) |>
      dplyr::mutate(
        quality_code = NA_character_,
        qf_desc      = NA_character_
      )
  }
}


# ---------------------------------------------------------------------------
# Adapter functions
# ---------------------------------------------------------------------------

.bc_aquarius_list_stations <- function() .BC_AQUARIUS_STATIONS

# Sub-daily flows (native logger interval, typically 5 min)
.bc_aquarius_fetch_flows <- function(station_id, start_date, end_date) {
  .get_aquarius_data(
    stn_number = station_id,
    parameter  = "discharge",
    start_time = start_date,
    end_time   = end_date,
    unit_id    = .BC_AQUARIUS_UNIT_DISCHARGE,
    interval   = "PointsAsRecorded"
  ) |>
    dplyr::mutate(
      station_id    = station_id,
      parameter     = "water_discharge",
      unit          = "m3/s",
      provider_name = "bc_aquarius"
    ) |>
    dplyr::select(
      station_id, timestamp, value, parameter,
      unit, provider_name, quality_code, qf_desc
    )
}

# Daily flows (aggregated server-side by Aquarius)
.bc_aquarius_fetch_daily_flows <- function(station_id, start_date, end_date) {
  .get_aquarius_data(
    stn_number = station_id,
    parameter  = "discharge",
    start_time = start_date,
    end_time   = end_date,
    unit_id    = .BC_AQUARIUS_UNIT_DISCHARGE,
    interval   = "Daily"
  ) |>
    dplyr::mutate(
      station_id    = station_id,
      date          = as.Date(timestamp),
      parameter     = "water_discharge",
      unit          = "m3/s",
      provider_name = "bc_aquarius"
    ) |>
    dplyr::select(
      station_id, date, value, parameter,
      unit, provider_name, quality_code, qf_desc
    )
}

# Sub-daily levels (native logger interval, typically 5 min)
.bc_aquarius_fetch_levels <- function(station_id, start_date, end_date) {
  .get_aquarius_data(
    stn_number = station_id,
    parameter  = "stage",
    start_time = start_date,
    end_time   = end_date,
    unit_id    = .BC_AQUARIUS_UNIT_STAGE,
    interval   = "PointsAsRecorded"
  ) |>
    dplyr::mutate(
      station_id    = station_id,
      parameter     = "water_level",
      unit          = "m",
      provider_name = "bc_aquarius"
    ) |>
    dplyr::select(
      station_id, timestamp, value, parameter,
      unit, provider_name, quality_code, qf_desc
    )
}

# Daily levels (aggregated server-side by Aquarius)
.bc_aquarius_fetch_daily_levels <- function(station_id, start_date, end_date) {
  .get_aquarius_data(
    stn_number = station_id,
    parameter  = "stage",
    start_time = start_date,
    end_time   = end_date,
    unit_id    = .BC_AQUARIUS_UNIT_STAGE,
    interval   = "Daily"
  ) |>
    dplyr::mutate(
      station_id    = station_id,
      date          = as.Date(timestamp),
      parameter     = "water_level",
      unit          = "m",
      provider_name = "bc_aquarius"
    ) |>
    dplyr::select(
      station_id, date, value, parameter,
      unit, provider_name, quality_code, qf_desc
    )
}


# ---------------------------------------------------------------------------
# Adapter constructor
# ---------------------------------------------------------------------------

hydrocan_adapter_bc_aquarius <- function() {
  new_hydrocan_adapter(
    "bc_aquarius",
    paste(
      "BC Government Aquarius hydrometric network.",
      "Sub-daily (native logger rate) and daily discharge and stage.",
      "Includes provincial network and other hydrometric stations.",
      "Station list hardcoded from portal scrape 2026-05-19;",
      "refresh manually if network changes.",
      "Approval level available for sub-daily data (quality_code, qf_desc);",
      "NA for daily data as the daily export does not include approval level."
    ),
    .bc_aquarius_list_stations,
    fetch_flows_fn        = .bc_aquarius_fetch_flows,
    fetch_daily_flows_fn  = .bc_aquarius_fetch_daily_flows,
    fetch_levels_fn       = .bc_aquarius_fetch_levels,
    fetch_daily_levels_fn = .bc_aquarius_fetch_daily_levels
  )
}
