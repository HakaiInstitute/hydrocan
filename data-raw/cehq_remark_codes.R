# CEHQ remark code lookup table
#
# Downloads a real CEHQ historical data file and parses the Lexique/Remarque
# section from the file header, where CEHQ documents all remark codes inline.
# Using a real file as the source ensures the descriptions stay authoritative.
#
# Re-run this script if CEHQ updates their remark code vocabulary, then commit
# the resulting R/sysdata.rda alongside this script.

url <- paste0(
  "https://www.cehq.gouv.qc.ca/depot/historique_donnees/fichier/030101_Q.txt"
)

raw <- readLines(url, encoding = "latin1", warn = FALSE)

# The Lexique block runs from the "Lexique:" line to just before the data.
lexique_start <- grep("^Lexique", raw)
data_start    <- grep("^[0-9]{6}[[:space:]]", raw)[1L]
lexique_lines <- raw[lexique_start:(data_start - 1L)]

# Strip the line-prefix labels ("Lexique:" and "(Remarque)") so the code
# entries that follow them are left-aligned for consistent parsing.
lexique_lines <- gsub("^(Lexique:|\\(Remarque\\))", "", lexique_lines)

# Keep only lines that contain a code entry.
entry_lines <- grep(
  "^[[:space:]]*[A-Z][A-Z0-9*]*[[:space:]]*:",
  lexique_lines,
  value = TRUE
)

# Split compound lines: "P:  desc1; P* : desc2" -> two separate entries.
entries <- trimws(unlist(strsplit(entry_lines, ";")))

# Extract "CODE : description" from each entry.
parsed <- regmatches(
  entries,
  regexpr("[A-Z][A-Z0-9*]*[[:space:]]*:[[:space:]]+.+", entries)
)

codes <- trimws(sub("([A-Z][A-Z0-9*]*)[[:space:]]*:.*",  "\\1", parsed))
descs <- trimws(sub("[A-Z][A-Z0-9*]*[[:space:]]*:[[:space:]]+", "", parsed))

cehq_remark_codes <- data.frame(
  quality_code = codes,
  qf_desc      = descs,
  stringsAsFactors = FALSE
)

print(cehq_remark_codes)

usethis::use_data(cehq_remark_codes, internal = TRUE, overwrite = TRUE)
