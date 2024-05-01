#' @title NDRS Prevalence Function
#' @description Calculating prevalence using raw data from the National Disease Registration Service.
#' @details This functions requires 2 data frames: patient level numerator data and raw ONS populations data for the denominator.
#' An end date is also required to calculate the point prevalence and there are further options such as to calculate livebirth or total birth prevalence for congenital anomalies.
#' @param numerator_data A dataframe with row level patient data.
#' @param denominator_data A dataframe with population count per year, lsoa11 and age.
#' @param end_date Character string with date in format YYYY-MM-DD. Marks the point prevalence date.
#' @param start_date Character string with date in format YYYY-MM-DD. Use for subsetting the data over a period of time.
#' @param years_before_point Integer to set number of years before end date to subset the data over a period of time.
#' @param conganom_data Logical. Set to TRUE if using congenital anomalies data, default is FALSE.
#' @param rare_diseases_data Logical. Set to TRUE if using rare diseases data, default is FALSE.
#' @param cancer_data Logical. Set to TRUE if using cancer data, default is FALSE.
#' @param per_people Integer for prevalence per x people. Defaults to 10,000 for congenital anomalies, 100,000 for cancer and 1,000,000 for rare diseases.
#' @param conganom_prev_type Character string to select prevalence calculation type for congenital anomalies. Accepts 'point', 'livebirth' or 'totalbirth' and default is set to 'point'.
#' @param write_csv Logical. Set to TRUE to download a csv copy of output to working directory, default is FALSE.
#' @return A data frame with a row per condition / disease, and columns providing the name, code, prevalence rate, confidence intervals and time period.
#' A .csv copy of the dataframe is also outputted to the working directory if write_csv is set to TRUE.
#' @importFrom lubridate "years"
#' @importFrom PHEindicatormethods "phe_rate"
#' @import dplyr
#' @export
#' @examples
#' # Using raw data from rare diseases to calculate the point prevalence
#' ndrs_prevalence(numerator_data = rare_diseases_raw,
#'                 denominator_data = ons_mid_year_population_data,
#'                 end_date = '2020-12-31',
#'                 rare_diseases_data = TRUE)
#'
#' @examples
#' # Using raw cancer data to calculate 20 year prevalence
#' ndrs_prevalence(numerator_data = rare_cancer_data,
#'                 denominator_data = ons_mid_year_population_data,
#'                 end_date = '2020-06-01',
#'                 years_before_point = 20,
#'                 cancer_data = TRUE)
#'
#' @examples
#' # Using raw data from congenital anomalies to calculate the livebirth prevalence
#' ndrs_prevalence(numerator_data = congenital_anomalies_raw,
#'                 denominator_data = birth_population_data,
#'                 end_date = '2020-12-31',
#'                 conganom_data = TRUE,
#'                 per_people = 10000,
#'                 conganom_prev_type = 'livebirth',
#'                 write_csv = TRUE)
ndrs_prevalence <- function(numerator_data,
                            denominator_data,
                            end_date,
                            start_date = NULL,
                            years_before_point = NULL,
                            conganom_data = FALSE,
                            rare_diseases_data = FALSE,
                            cancer_data = FALSE,
                            per_people = NULL,
                            conganom_prev_type = NULL,
                            write_csv = FALSE) {

  # Error catchers and Warnings for all arguments

  # Check numerator, denominator and end_date have all been provided
  if (missing(numerator_data) || is.null(numerator_data) ||
      missing(denominator_data) || is.null(denominator_data) ||
      missing(end_date) || is.null(end_date)){

    stop("Error: All 3 mandatory arguments ('numerator_data', 'denominator_data', 'end_date') are required and cannot be NULL.")

  } # Check numerator and denominator are data frames
  else if(!is.data.frame(numerator_data) || !is.data.frame(denominator_data)) {

    stop("Error: 'numerator_data' and 'denominator_data' must be data frames.")

  } # Check only one and at least one dataset param is set to TRUE
  else if (sum(c(conganom_data, rare_diseases_data, cancer_data)) != 1) {

    stop("Error: Exactly one of the optional arguments ('conganom_data', 'rare_diseases_data', 'cancer_data') must be set to TRUE.")

  } # Check for right grouping column names
  else if (conganom_data & (!"eurocat_sub_group" %in% names(numerator_data) | !"icd10" %in% names(numerator_data))) {

    stop("Error: 'numerator_data' must have a 'eurocat_sub_group' and 'icd10' columns.")

  } else if (rare_diseases_data & (!"DISEASE_ORPHA" %in% names(numerator_data)  | !"ORPHANAME" %in% names(numerator_data))) {

    stop("Error: 'numerator_data' must have 'DISEASE_ORPHA' and 'ORPHANAME' columns.")

  } else if (cancer_data & (!"DISEASE_GROUP_NAME" %in% names(numerator_data) | !"DISEASE_GROUP_CODE"  %in% names(numerator_data))) {

    stop("Error: 'numerator_data' must have 'DISEASE_GROUP_NAME' and 'DISEASE_GROUP_CODE' columns. ")

  } # Check that the end_date argument is in the correct character format
  else if(!is.character(end_date) || !grepl("^\\d{4}-\\d{2}-\\d{2}$", end_date)){

    stop("Error: 'end_date' must be a character string in the 'YYYY-MM-DD' format")

  }

  # Warning message if conganom_data and no prev type provided
  if (conganom_data & missing(conganom_prev_type)){
    warning("Warning: 'conganom_prev_type' not provided.
            Using default value 'POINT' to calculate point prevalence.
            For other prevalence types consider providing 'livebirth' or 'totalbirth' with a 'start_date'.")

    # sets the default value as point
    conganom_prev_type <- 'POINT'
  }

  # Warning message if no 'per_people' value provided sets defaults
  if (is.null(per_people) | missing(per_people)){
    # sets the default values depending on data set
    per_people <- ifelse (conganom_data, 10000,
                          ifelse(rare_diseases_data, 1000000,
                                 ifelse(cancer_data, 100000, NULL)
                          ))

    warning(paste0("Warning: 'per_people' not provided.
            Using default value to calculate prevalence per ", formatC(per_people, format = "d", big.mark = ",")," people."))
  }

  # Make congenital anomaly prevalence type upper case
  if (!is.null(conganom_prev_type)){
    conganom_prev_type <- toupper(conganom_prev_type)
  }

  # Check start_date is only provided for conganom if 'livebirth' or 'totalbirth' prevalence
  if (conganom_data == TRUE &&
      (conganom_prev_type == 'LIVEBIRTH' | conganom_prev_type == 'TOTALBIRTH') &&
      is.null(start_date)) {

    stop("Error: 'start_date' is required for congenital anomaly livebirth or total birth prevalence.")

  } # Check start_date has been inputted when rare diseases prevalence
  else if (rare_diseases_data & is.null(start_date) & is.null(years_before_point)){

    stop("Error: One of 'start_date' of 'years_before_point' are required for calculating point prevalences for rare diseases.")

  } # Check start_date argument is in the correct character format
  else if(!is.null(start_date) & (!is.character(start_date) || !grepl("^\\d{4}-\\d{2}-\\d{2}$", start_date))) {

    stop("Error: 'start_date' must be NULL or a character string in the 'YYYY-MM-DD' format")

  } # Check for invalid inputs for conganom prevalence type
  else if (conganom_data && !(conganom_prev_type %in% c('POINT', 'LIVEBIRTH', 'TOTALBIRTH'))) {

    stop("Error: Invalid input value for congenital anomaly prevalence type. Please provide one of: 'point', 'livebirth', 'totalbirth'.")

  } # Check the years_before_point is a numerical value
  else if(!is.null(years_before_point) && !(is.integer(years_before_point) | is.numeric(years_before_point))){

    stop("Error: 'years_before_point' has to be an integer to calculate prevalence x years before the 'end_date'.")

  } # Check both start_date and years_before_point have not been provided
  else if (!is.null(start_date) && !is.null(years_before_point)) {

    stop("Error: Only one of 'start_date' and 'years_before_point' can be provided at a time.")

  }

  # set data type as date
  end_date <- as.Date(end_date)

  # calculate start_date if years_before_point is provided
  if(!is.null(years_before_point)) {
    start_date <- end_date - years(years_before_point)
  }

  # function to group and calculate the denominator value
  # depending on whether all or only some ages are required
  denominator_counts <- function(data, up_to_age = NULL) {
    data %>%
      filter(startsWith(LSOA11, "E")) %>% # England data only
      group_by(YEAR) %>%
      summarize(
        population = ifelse(is.null(up_to_age),
                            sum(across(starts_with("A"))), # sum all age columns
                            # or sum only ages required
                            sum(across(num_range("A", seq.int(from = 0, up_to_age))))
        )
      ) %>%
      ungroup()
  }

  # Filtering numerator and denominator data frames
  # if else statements for each type of dataset (conganom/rare diseases/cancer)
  if (conganom_data) {

    # set data types to date
    numerator_data$birthdate <- as.Date(numerator_data$birthdate)
    numerator_data$deathdate <- as.Date(numerator_data$deathdate)

    # conditional filtering of numerator data depending on prevalence type
    if (conganom_prev_type == 'POINT') {

      numerator_filtered_cases <- numerator_data %>%
        # filter for livebirths only
        filter(outcome_type == 1) %>%
        # only keep babies born before the end date
        filter(birthdate <= end_date) %>%
        # only keep babies alive at that date
        filter(deathdate > end_date | is.null(deathdate) | is.na(deathdate))

      # function to calculate years since 2018 to get what ages of the denominator data are required
      years_since_2018 <- function(date_string) {
        date_2018 <- as.Date("2018-01-01")  # January 1, 2018
        spec_date <- as.Date(date_string)  # Convert the character string to a Date object
        time_diff <- as.numeric(difftime(spec_date, date_2018, units = "days"))  # Calculate the difference in days
        years <- time_diff / 365.25  # Divide by the average number of days in a year considering leap years
        years <- floor(years)  # Round down to get the number of full years
        return(years)
      }

      # Filter to get denominator value using custom functions
      denominator_count <- denominator_counts(denominator_data, up_to_age = years_since_2018(end_date)) %>%
        filter(YEAR == format(end_date, "%Y")) %>%
        select(population)

    } else if (conganom_prev_type == 'LIVEBIRTH') {

      numerator_filtered_cases <- numerator_data %>%
        # filter for all births after the inputted start date
        filter(birthdate >= start_date & birthdate <= end_date) %>%
        # filter for livebirths only
        filter(outcome_type == 1)

      # Filter to get denominator value
      denominator_count <- denominator_data %>%
        filter(YEAR == format(end_date, "%Y")) %>%
        select(population)

    } else if (conganom_prev_type == 'TOTALBIRTH') {

      numerator_filtered_cases <- numerator_data %>%
        # filter for all births after the inputted start date
        filter(birthdate >= start_date & birthdate <= end_date)

      # Filter to get denominator value
      denominator_count <- denominator_data %>%
        filter(YEAR == format(end_date, "%Y")) %>%
        select(population)

    }

    # get counts per eurocat sub group with a column of all icd codes in the group
    numerator_data_grouped_counts <- numerator_filtered_cases %>%
      group_by(eurocat_sub_group) %>%
      distinct(icd10) %>%
      summarise(code = paste(icd10, collapse = ", ")) %>%
      ungroup() %>%
      left_join(numerator_filtered_cases %>%
                  group_by(eurocat_sub_group) %>%
                  summarise(num_count = n()),
                by = "eurocat_sub_group"
      ) %>%
      rename(name = eurocat_sub_group)

  } else if (rare_diseases_data) {

    # filter on diagnosis date and death date
    numerator_filtered_cases <- numerator_data %>%
      # set dates as correct data type
      mutate(DIAGNOSIS_DATE = as.Date(DIAGNOSIS_DATE),
             CLINIC_DATE = as.Date(CLINIC_DATE),
             REPORT_DATE = as.Date(REPORT_DATE)) %>%
      # select the right date
      mutate(FINAL_DATE = case_when(
        !is.na(DIAGNOSIS_DATE) ~ DIAGNOSIS_DATE,
        !is.na(CLINIC_DATE) ~ CLINIC_DATE,
        is.na(REPORT_DATE) ~ as.Date(NA),
        TRUE ~ REPORT_DATE
      )) %>%
      # filter out null nhs numbers
      filter(!is.na(NHSNUMBER)) %>%
      # filter out suspected cases
      filter(CERTAINTY != "Suspected") %>%
      # only keep people born and diagnosed before the end date
      filter(BIRTHDATE <= end_date & FINAL_DATE >= start_date & FINAL_DATE <= end_date) %>%
      # only keep all people alive at that date
      filter(DEATH_DATE > end_date | is.null(DEATH_DATE) | is.na(DEATH_DATE))  %>%
      # Filter cases keep only earliest diagnosis date
      group_by(DISEASE_ORPHA, NHSNUMBER) %>%
      arrange(FINAL_DATE) %>%
      distinct(., NHSNUMBER, .keep_all = TRUE) %>%
      ungroup()

    # get counts per orpha code
    numerator_data_grouped_counts <- numerator_filtered_cases %>%
      group_by(DISEASE_ORPHA, ORPHANAME) %>%
      summarise(num_count = n()) %>%
      rename(name = ORPHANAME,
             code = DISEASE_ORPHA)

    # Filter to get denominator value
    denominator_count <- denominator_counts(denominator_data) %>%
      filter(YEAR == format(end_date, "%Y")) %>%
      select(population)

  } else if (cancer_data) {

    # if start_date provided then this is filtered
    # else no start_date filtered to get the complete prevalence
    if(!is.null(start_date)) {
      numerator_data <- numerator_data %>%
        mutate(DIAGNOSISDATE = as.Date(DIAGNOSISDATE)) %>%
        filter(DIAGNOSISDATE >= start_date)
    }

    # Filter cases keep only earliest diagnosis date
    numerator_filtered_cases <- numerator_data %>%
      mutate(BIRTHDATE = as.Date(BIRTHDATE),
             DEATHDATE = as.Date(DEATHDATE),
             DIAGNOSISDATE = as.Date(DIAGNOSISDATE)) %>%
      filter(BIRTHDATE <= end_date & DIAGNOSISDATE <= end_date) %>%
      filter(DEATHDATE > end_date | is.null(DEATHDATE) | is.na(DEATHDATE)) %>%
      group_by(DISEASE_GROUP_CODE, PATIENTID) %>%
      arrange(DIAGNOSISDATE) %>%
      distinct(., PATIENTID, .keep_all = TRUE) %>%
      ungroup()

    # get counts per disease group name
    numerator_data_grouped_counts <- numerator_filtered_cases %>%
      group_by(DISEASE_GROUP_NAME, DISEASE_GROUP_CODE) %>%
      summarise(num_count = n()) %>%
      rename(name = DISEASE_GROUP_NAME,
             code = DISEASE_GROUP_CODE)

    # Filter to get denominator value
    denominator_count <- denominator_counts(denominator_data) %>%
      filter(YEAR == format(end_date, "%Y")) %>%
      select(population)

  }

  # create output data frame
  output_dataframe <- data.frame(
    name = numerator_data_grouped_counts$name,
    code = numerator_data_grouped_counts$code,
    numerator_count = numerator_data_grouped_counts$num_count,
    denominator_count = denominator_count$population

  )

  # calculating confidence intervals
  if (conganom_data | rare_diseases_data) {

    # calculate prevalence
    output_dataframe$prevalence <- round(output_dataframe$numerator_count / output_dataframe$denominator_count * per_people, digits = 3)

    # confidence intervals method calculated using the Poisson distribution (Bégaud et al, 2005)
    # https://files.digital.nhs.uk/94/02F584/NCARDRS%20Congenital%20Anomaly%20Official%20Statistics%202020%20Technical%20Details.pdf
    output_dataframe$LCI <- round(((((1.96/2)-sqrt(numerator_data_grouped_counts$num_count +0.02))^2)/denominator_count$population)*per_people, digits = 3)
    output_dataframe$UCI <- round(((((1.96/2)+sqrt(numerator_data_grouped_counts$num_count+0.96))^2)/denominator_count$population)*per_people, digits = 3)

  } else if (cancer_data) {

    # use phe rates function to calculate rates with confidence intervals for the rare cancers
    cancer_prevalences <- PHEindicatormethods::phe_rate(output_dataframe, numerator_count, denominator_count, multiplier = per_people)

    # put them into the output dataframe
    output_dataframe$prevalence <- round(cancer_prevalences$value, digits = 3)
    output_dataframe$LCI <- round(as.numeric(cancer_prevalences$lowercl), digits = 3)
    output_dataframe$UCI <- round(as.numeric(cancer_prevalences$uppercl), digits = 3)
  }

  # remove counts columns from the output data frame
  output_dataframe <- output_dataframe %>%
    select(-numerator_count, -denominator_count)

  # Set the first and second column in the output data frame depending on data
  first_column_name <- ifelse(conganom_data, 'Eurocat Subgroup',
                              ifelse(rare_diseases_data, 'Rare Disease',
                                     ifelse(cancer_data, 'Cancer',
                                            'name'
                                     )
                              )
  )

  second_column_name <- ifelse(conganom_data, 'ICD10 Codes',
                               ifelse(rare_diseases_data,'Orpha Code',
                                      ifelse(cancer_data, 'Disease Group Code',
                                             'code'
                                      )
                               )
  )

  # add a date column for the output
  output_dataframe$prevalence_date <- ifelse(is.null(start_date), as.character(end_date), paste0(as.character(start_date), " - ", as.character(end_date)))

  # set final column names
  colnames(output_dataframe) <- c(first_column_name,
                                  second_column_name,
                                  paste0('Prevalence per ', formatC(per_people, format = "d", big.mark = ","), ' people'),
                                  'Lower 95% confidence limit',
                                  'Upper 95% confidence limit',
                                  'Prevalence Date')

  # if write_csv has been set to true then saves csv copy
  if (write_csv) {
    # set data type to include in csv filename
    data_type <- ifelse(conganom_data, 'Congenital-Anomalies',
                        ifelse(rare_diseases_data, 'Rare-Diseases',
                               ifelse(cancer_data, 'Cancer', 'NDRS'
                               )
                        )
    )

    # output the csv file to working directory
    write.csv(output_dataframe,
              paste0(data_type,"-Prevalence-",end_date,".csv"),
              row.names = FALSE)
  }

  # outputs the data frame in console
  return(output_dataframe)

}
