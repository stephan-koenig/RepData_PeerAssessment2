replace90Min <- function(x) {
    # Function takes any 4 character long string and replaces any '9' at the
    # third position with '0'.
    if (nchar(x) == 4 & regexpr('[:digit:]{2}9[digit]', x) !=0) {
        paste0(substr(x, 1, 2), "0", substr(x, 4, 4))
    }
}

# List to convert time zones listed in TIME_ZONE to names present in OlsonNames()
timezoneCoversionList <- list('Etc/GMT-3' = 'ADT',
                              'US/Alaska' = 'AKS',
                              'Etc/GMT-4' = 'AST', 
                              'CST6CDT' = 'CDT',
                              'CST6CDT' = 'CSC', # Misspelled, state is IA
                              'CST6CDT' = 'CSt', # Missplelled, all counties in CT
                              'CST6CDT' = 'CST',
                              'EST5EDT' = 'EDT', 
                              'EST5EDT' = 'ESY', # Misspelled, state is PA
                              'Pacific/Guam' = 'GST',
                              'US/Mountain' = 'MDT',
                              'PST8PDT' = 'PDT',
                              'PST8PDT' = 'PST',
                              'CST6CDT' = "SCT", # Misspelled, states IA and WI
                              'US/Samoa' = 'SST')

# Create new column for corrected timezones as character for future manipulations
stormData$CORRECTED_TIME_ZONE <- as.character(stormData$TIME_ZONE)

# Replace any UNK time zones derived from County and State
stormData[which(stormData$TIME_ZONE == 'UNK' & stormData$STATE=='GA' &
                    stormData$COUNTYNAME=='OCONEE'),
          ]$CORRECTED_TIME_ZONE <- 'EST5EDT'

# Do same for other unknown time zones...

# Mutate into character vector.
stormData <- mutate(stormData, CORRECTED_TIME = as.character(BGN_TIME)) %>%
    # Replace any '000' with '0000'.
    mutate(CORRECTED_TIME  = ifelse((CORRECTED_TIME == '000'),
                                    '0000',
                                    CORRECTED_TIME)) %>%
    # Remove ' 0:00:00'
    mutate(CORRECTED_DATE = gsub(' 0:00:00', '', stormData$BGN_DATE,
                                 fixed=TRUE)) %>%
    # Paste date and time together
    mutate(DATE_TIME = paste(CORRECTED_DATE, CORRECTED_TIME)) %>%
    # Fix all time zones to names present in OlsonNames
    mutate(CORRECTED_TIME_ZONE = gsub('PDT', 'XXXXXXX', CORRECTED_TIME_ZONE,
                                      fixed=TRUE)) %>%
    mutate(DATE_TIME = parse_date_time(DATE_TIME,
                                       c("mdy hm", "mdy I:m:s p"),
                                       tz = toupper(as.character(TIME_ZONE))))