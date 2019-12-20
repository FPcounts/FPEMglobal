get_survey_data_errors_INTERNAL <- function(data) {
  column_names <-
    c(
      "Country",
      "ISO.code",
      "Age..range",
      "Catalog.ID",
      "Source.name",
      "Data.series.type",
      "Note.on.data",
      "Note.on.methods",
      "Note.on.country",
      "Note.on.population",
      "Notes.on.residuals...Modern.methods.",
      "Notes.on.residuals...Traditional.methods.",
      "Contraceptive.use.ANY",
      "Contraceptive.use.MODERN",
      "Contraceptive.use.TRADITIONAL",
      "Unmet",
      "GEO.biases..unknown.direction.",
      "Non.pregnant.and.other.positive.biases",
      "Negative.bias",
      "Population.type",
      "age.cat.bias",
      "Modern.method.bias",
      "Folk.method.positive.bias",
      "Absence.of.probing.questions.bias",
      "Start.year",
      "End.year",
      "EXCLUDE1isyes",
      "include.trad.ses",
      "include.modern.ses",
      "include.unmet.ses",
      "In.union"
    )

  errors <- character()

  missing_column_names <- setdiff(column_names, names(data))

  if (length(missing_column_names) > 0) {
    errors <- c(errors, paste("Missing fields:", paste(missing_column_names, sep = ", ")))
  }

  additional_column_names <- setdiff(names(data), column_names)

  if (length(additional_column_names) > 0) {
    errors <- c(errors, paste("Unexpected fields:", paste(additional_column_names, sep = ", ")))
  }

  invalid_age_ranges <- Filter(f = function(age_range) !is_valid_age_range(age_range), x = data$Age..range)

  if (length(invalid_age_ranges) > 0) {
    errors <- c(errors, paste("Invalid age ranges:", paste(sort(unique(invalid_age_ranges)), sep = ", ")))
  }

  missing_contraceptive_use_any_values = any(is.na(data$Contraceptive.use.ANY)) || any(is.null(data$Contraceptive.use.ANY))

  if (missing_contraceptive_use_any_values) {
    errors <- c(errors, "Missing Contraceptive.use.ANY values")
  }

  errors
}

is_valid_age_range <- function(age_range) {
  valid_age_ranges <-
    c(
      "10-49",
      "12-49",
      "13-49",
      "14-49",
      ">15",
      "15+",
      "15-39",
      "15-44",
      "15-45",
      "15-46",
      "15-49",
      "15-50",
      "15-54",
      "16-45",
      "16-49",
      "16-59",
      "18-34",
      "18-37",
      "18-39",
      "18-41",
      "18-42",
      "18-44",
      "18-45",
      "18-46",
      "18-49",
      "18-50",
      "20-39",
      "20-40",
      "20-43",
      "20-44",
      "20-45",
      "20-49",
      "21-39",
      "21-49",
      "25-49",
      "<35",
      "<44",
      "<45",
      "<50",
      "See notes",
      "Total"
    )

  any(valid_age_ranges == age_range)
}

get_denominator_count_errors_INTERNAL <- function(data) {
  errors <- character()

  column_names <-
    c(
      "Country.letter.code",
      "ISO.code",
      "Country",
      "In.union"
    )

  missing_column_names <- setdiff(column_names, names(data))

  if (length(missing_column_names) > 0) {
    errors <- c(errors, paste("Missing fields:", paste(missing_column_names, sep = ", ")))
  }

  errors
}
