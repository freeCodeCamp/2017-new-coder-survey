# Clean and Combine freeCodeCamp's 2017 New Coder Survey
# Description:      This script cleans specifically freeCodeCamp's 2017 New
#                   Coder Survey. The following code is split into four main
#                   sections: Utility function, Sub-Process functions,
#                   Sub-Cleaning functions, Main Process functions. The main
#                   function to perform the entire cleaning and combining is
#                   the `main()` function at the end of this script.
# Author:           Eric Leung (@erictleung)
# Last Updated:     2017 May 25th

# Load in necessary packages
require(dplyr)     # Manipulate data
require(tidyr)     # Reshape data

# Variables
dataPath1 <- "../raw-data/2017-new-coder-survey-part-1.csv"
dataPath2 <- "../raw-data/2017-new-coder-survey-part-2.csv"

# Script Outline
#   0. Exploratory Functions - used to explore and check data for cleaning
#   1. Utility Functions - take in arguments to perform simpler transformations
#   2. Sub-Process Functions - sub components within sub-cleaning functions
#   3. Sub-Cleaning Functions - smaller components for main processing
#   4. Main Processing Functions - big, main components of cleaning
#   5. Main Function - runs entire script to clean data set

# Exploratory Functions -----------------------------------
# Description:
#   These functions help do initial exploration of the data

# Take a peak at data, some unique counts, and push up to view all
sel_count_view <- function(data, col) {
    data %>% select_(col) %>% print
    data %>% count_(col) %>% print
    data %>% count_(col) %>% View
}

# Search for column name between two data frames
both_parts_same_col <- function(data1, data2, col) {
    # Get column names for each data set
    col1names <- data1 %>% colnames
    col2names <- data2 %>% colnames

    # Get indices
    col1idx <- col1names %>% grep(col, ., ignore.case = TRUE)
    col2idx <- col2names %>% grep(col, ., ignore.case = TRUE)

    # Print column names shared if they exist
    if (length(col1names) == 0 || length(col2names) == 0) {
        print("No shared column")
    } else {
        cat("Variable names in first input:\n")
        col1names[col1idx] %>% print()
        cat("Variable names in second input:\n")
        col2names[col2idx] %>% print()
    }
}

# Check if same name column between data sets are the same
is_same_dt <- function(data1, data2, col) {
    # Check for columns in data sets before selection
    dat1cols <- data1 %>% colnames()
    dat2cols <- data2 %>% colnames()
    if (col %in% dat1cols & col %in% dat2cols) {
        # Get columns
        df1df <- data1 %>% select_(col) %>% head
        df2df <- data2 %>% select_(col) %>% head

        # Take a peak of columns
        df1df %>% print
        df2df %>% print

        # Get actual class name
        df1dt <- df1df %>% unlist %>% class
        df2dt <- df2df %>% unlist %>% class

        # Check if data types are the same
        df1dt == df2dt
    } else {
        cat("No such column in the data. Please check spelling.")
    }
}

# Utility Functions ---------------------------------------
# Description:
#   These functions take in arguments to perform simpler transformations

# Title:
#   Fix Truncated Job Apply Answer
# Description:
#   When passing answers from the first part of the survey to the next,
#   apostrophes were thrown out. E.g. if the answer was "I haven't decided",
#   it was truncated to "I"
# Input:
#   String or vector of strings
# Output:
#   String or vector of strings
# Usage:
#   > fix_truncate_job_apply("I")
#   [1] "I'm already applying"
#   > fix_truncate_job_apply("I haven")
#   [1] "I haven't decided"
fix_truncate_job_apply <- function(answer) {
    truncateAns <- c()
    for (i in 1:length(answer)) {
        tempAns <- answer[i] %>%
            unlist %>%
            ifelse(. == "I", "I'm already applying", .) %>%
            ifelse(. == "I haven", "I haven't decided", .)
        truncateAns <- c(truncateAns, tempAns)
    }
    truncateAns
}


# Title:
#   Simple Title Case Function
# Description:
#    Intended to use in mutate functions to title case strings
# Input:
#    List of strings or just a string itself
# Output:
#    List of strings or just a string itself
# Usage:
#   > simple_title_case("hello world")
#   [1] "Hello World"
#   > simple_title_case(c("hello world", "simple title case"))
#   [1] "Hello World"       "Simple Title Case"
# Adapted from: http://stackoverflow.com/a/6364905
simple_title_case <- function(x) {
    titleCase <- c()
    for (i in 1:length(x)) {
        s <- strsplit(x[i], " ")[[1]]
        titleS <- paste(toupper(substring(s, 1,1)), substring(s, 2),
                        sep = "", collapse = " ")
        titleCase <- c(titleCase, titleS)
    }
    titleCase
}


# Title:
#   Average Range Earning
# Description:
#   Take a range string (e.g. "50-60000") and take average (e.g. "55"). In
#   this case, there is a string "50-60000" because there was a "k" at the end
#   I removed earlier
# Input:
#   String or vector
# Output:
#   String or vector
# Usage:
#   > average_range_earning("50-60000")
#   [1] "55000"
#   > average_range_earning("50-60")
#   [1] "55000"
average_range_earning <- function(x) {
    avgRange <- c()
    for (i in 1:length(x)) {
        tempRange <- x[i] %>% strsplit("-") %>%
            unlist %>%
            as.numeric %>%
            ifelse(. < 100, . * 1000, .) %>%
            mean %>%
            as.character()
        avgRange <- c(avgRange, tempRange)
    }
    avgRange
}


# Title:
#   Average String Range
# Description:
#   Take a range string (e.g. "50-60") and take average (e.g. "55").
average_string_range <- function(x) {
    avgRange <- c()
    for (i in 1:length(x)) {
        tempRange <- x[i] %>% strsplit("-|to") %>%
            unlist %>%
            as.numeric %>%
            mean %>%
            as.character()
        avgRange <- c(avgRange, tempRange)
    }
    avgRange
}


# Title:
#   Change Years to Months
# Description:
#   Remove non-numeric characters and change years to months
# Input:
#   String or vector of strings
# Output:
#   String or vector of strings
# Usage:
#   > years_to_months("3")
#   [1] "36"
#   > years_to_months(c("6", "3", "5"))
#   [1] "72" "36" "60"
years_to_months <- function(x) {
    monthsDat <- c()
    for (i in 1:length(x)) {
        tempMonths <- x[i] %>% gsub("[A-Za-z ]", "", .) %>%
            (function(x) as.numeric(x) * 12) %>%
            as.character()
        monthsDat <- c(monthsDat, tempMonths)
    }
    monthsDat
}


# Title:
#   Remove Outliers
# Description:
#   This function remove outliers based on threshold where anything equal to
#   it or above it is changed to an NA
# Input:
#   Numbers and a threshold
# Output:
#   Numbers
# Usage:
#   > remove_outlier(20, 2)
#   [1] NA
#   > remove_outlier(c(1, 2, 3, 4, 5, 6), 4)
#   [1]  1  2  3 NA NA NA
remove_outlier <- function(x, thres) {
    ifelse(test = x >= thres, yes = as.numeric(NA), no = x)
}


# Title:
#   Filter and Diff
# Description:
#   This function takes a filter criteria and applies it. It will remove this
#   row from the data set.
filter_and_diff <- function(data, filt) {
    outlierRows <- data %>% filter_(filt)
    data %>% setdiff(outlierRows)
}


# Sub-Process Functions -----------------------------------
# Description:
#   These functions perform larger grouped data transformations

# Title:
#   Change All Undefined Values to NA
# Description:
#   The second dataset contains values from the first part of the survey.
#   Those were passed as values and the missing values (i.e. NA) were passed
#   as "undefined" and need to be transformed back.
# Input:
#   Designed for the second dataset
# Output:
#   The second dataset with all the undefined changed to NA
# Usage:
#   > part2 <- undefined_to_NA(part2)
undefined_to_NA <- function(part2, changeCols) {
    fixedPart2 <- part2
    for (col in changeCols) {
        varval <- lazyeval::interp(~ ifelse(colName == "undefined",
                                            yes = NA,
                                            no = colName),
                                   colName = as.name(col))
        fixedPart2 <- fixedPart2 %>%
            mutate_(.dots = setNames(list(varval), col))
    }
    fixedPart2
}


# Title:
#   Change All Yes/No to 1/0
# Description:
#   The second dataset contains values from the first part of the survey. Some
#   of the yes/no questions were encoded as yes/no and 1/0. This function
#   serves to make them consistently into 1/0.
# Input:
#   Designed for the second dataset
# Output:
#   The second dataset with all yes/no answers changed to 1/0
# Usage:
#   > part2 <- yesNo_to_oneZero(part2)
yesNo_to_oneZero <- function(part2, changeCols) {
    fixedPart2 <- part2
    for (col in changeCols) {
        varvalYes <- lazyeval::interp(~ ifelse(colName == "Yes",
                                               yes = "1",
                                               no = colName),
                                      colName = as.name(col))
        varvalNo <- lazyeval::interp(~ ifelse(colName == "No",
                                              yes = "0",
                                              no = colName),
                                     colName = as.name(col))
        fixedPart2 <- fixedPart2 %>%
            mutate_(.dots = setNames(list(varvalYes), col)) %>%
            mutate_(.dots = setNames(list(varvalNo), col))
    }
    fixedPart2
}


# Title:
#   Change Characters to One
# Description:
#   Lots of columns need to be changed
# Input:
#   Designed for the second dataset
# Output:
#   The second dataset with all yes/no answers changed to 1/0
# Usage:
#   > part2 <- yesNo_to_oneZero(part2)
char_to_one <- function(part, changeCols) {
    fixedPart <- part
    for (col in changeCols) {
        varvalOne <- lazyeval::interp(~ ifelse(!is.na(colName),
                                               yes = "1",
                                               no = colName),
                                      colName = as.name(col))
        fixedPart <- fixedPart %>%
            mutate_(.dots = setNames(list(varvalOne), col))
    }
    fixedPart
}


# Title:
#   Change to Character
# Description:
#   Changes the data type of the given column names into character
# Input:
#   part1 = the part 1 dataset
#   toChr = string or vector of strings with column names needing change
# Output:
#   Changed part 1 dataset
# Usage:
#   > part1 <- change_to_chr(part1, toChr)
change_to_chr <- function(part1, toChr) {
    for (colName in toChr) {
        varval <- lazyeval::interp(~ as.character(colHere),
                                   colHere = as.name(colName))
        part1 <- part1 %>%
            mutate_(.dots = setNames(list(varval), colName))
    }
    part1
}


# Title:
#   Change to Double
# Description:
#   Changes the data type of the given column names into double
# Input:
#   part2 = the part 2 dataset
#   toDbl = string or vector of strings with column names needing change
# Output:
#   Changed part 2 dataset
# Usage:
#   > part2 <- change_to_dbl(part2, toDbl)
change_to_dbl <- function(part2, toDbl) {
    for (colName in toDbl) {
        varval <- lazyeval::interp(~ as.double(colHere),
                                   colHere = as.name(colName))
        part2 <- part2 %>%
            mutate_(.dots = setNames(list(varval), colName))
    }
    part2
}


# Title:
#   Substitution function [WIP]
# Description:
#   Used directly on a dplyr data frame to grep substitute
# Input:
#   dplyr data frame
# Output:
#   dplyr data frame
sub_and_rm <- function(dirtyDat, colName, findStr, replaceStr) {
    varval <- lazyeval::interp(~ gsub(f, r, c),
                               f = findStr,
                               r = replaceStr,
                               c = as.name(colName))

    dirtyIdx <- dirtyDat %>% select_(colName) %>%
        mutate_each(funs(grepl(findStr, ., ignore.case = TRUE))) %>%
        unlist(use.names = FALSE)
    subDirty <- dirtyDat %>% filter(dirtyIdx) %>%
        mutate_(.dots = setNames(list(varval), colName))

    cleanDat <- dirtyDat %>% filter(!dirtyIdx) %>% bind_rows(subDirty)

    cleanDat
}


# Title:
#   Normalize Text
# Description:
#   Normalize text based on searching list and desired single replacement
#   using non-standard eval in dplyr: http://stackoverflow.com/a/26003971
# Input:
#   inData        = dplyr data frame,
#   columnName    = column you want to change,
#   search Terms  = search terms in a c() vector,
#   replaceWith   = replacement string
# Output:
#   dplyr data frame
# Usage:
#   > cleanPart1 <- normalize_text(inData = cleanPart1,
#   + columnName = "JobRoleInterestOther",
#   + searchTerms = undecidedWords,
#   + replaceWith = "Undecided")
# More on NSE:
#   https://cran.r-project.org/web/packages/dplyr/vignettes/nse.html
# Adapted from:
#   http://stackoverflow.com/a/26766945
normalize_text <- function(inData, columnName, searchTerms, replaceWith) {
    # Setup dynamic naming of variables later in function
    varval <- lazyeval::interp(~ replaceText, replaceText = replaceWith)

    # Gets indices for rows that need to be changed
    searchStr <- paste(searchTerms, collapse = "|")
    wordIdx <- inData %>% select_(columnName) %>%
        mutate_each(funs(grepl(searchStr, ., ignore.case = TRUE))) %>%
        unlist(use.names = FALSE)

    # Change row values to intended words
    wordData <- inData %>% filter(wordIdx) %>%
        mutate_(.dots = setNames(list(varval), columnName))

    # Combine data back together
    cleanData <- inData %>% filter(!wordIdx) %>% bind_rows(wordData)

    cleanData
}


# Title:
#   Create New Column Based on Grep
# Description:
#   This function will search for terms in a given column in the rows. It will
#   then add a column with the name of your choosing and label rows that
#   contain your search term as having that term i.e. give a value of "1".
# Input:
#   inData       = dplyr data frame,
#   colName      = column you want to target,
#   searchTerms  = search terms in a c() vector,
#   newCol       = name for new column
# Output:
#   dplyr data frame with new column
# Usage:
#   > cleanPart1 <- search_and_create(inData = cleanPart1,
#   + colName = "CodeEventOther", searchTerms = c("meetup", "meetup"),
#   + newCol = "CodeEventMeetups")
search_and_create <- function(inData, colName, searchTerms, newCol) {
    # Create new column with new name
    makeNew <- lazyeval::interp(~ as.character(NA))
    cleanData <- inData %>% mutate_(.dots = setNames(list(makeNew), newCol))

    # Create search criteria
    searchStr <- paste(searchTerms, collapse = "|")
    varval <- lazyeval::interp(~ grepl(s,c, ignore.case = TRUE),
                               s = searchStr,
                               c = as.name(colName))

    # Label target rows as belonging to new column group
    mut <- lazyeval::interp(~ ifelse(test = grepl(s, c, ignore.case = TRUE),
                                     yes = "1",
                                     no = NA),
                            s = searchStr,
                            c = as.name(colName))
    cleanData <- cleanData %>%
        mutate_(.dots = setNames(list(mut), newCol))

    cleanData
}


# Title:
#   Helper Function
# Description:
#   Temporary function to use to check if regular expression is targeting the
#   rows we think it should be targeting.
# Input:
#   part      = dplyr data frame,
#   col       = column you want to target,
#   words     = string or vector of strings you want to search for,
#   printYes  = default to NA to just view data, set to 1 to print our data
#               frame, and set to anything else to count number of instances
# Usage:
#   > part <- part2
#   > col <- "MoneyForLearning"
#   > words <- c("[^0-9]")
#   > helper_filter(part, col, words)     # To View
#   > helper_filter(part, col, words, 1)  # To print data to console
#   > helper_filter(part, col, words)     # To print number of instances
helper_filter <- function(part, col, words, printYes = NA) {
    # Helper code to look at data being filtered to be changed
    columnToLookAt <- col # Column name you want to examine
    wordSearch <- words %>% # Array of regular expressions to search
        paste(collapse = "|")
    charIdx <- part %>% select_(columnToLookAt) %>%
        mutate_each(funs(grepl(wordSearch, ., ignore.case = TRUE))) %>%
        unlist(use.names = FALSE)
    if (is.na(printYes)) {
        part %>% filter(charIdx) %>% count_(columnToLookAt) %>% View
    } else if (printYes == 1) {
        part %>% filter(charIdx) %>% select_(columnToLookAt) %>%
            distinct() %>% as.data.frame
    } else {
        part %>% filter(charIdx) %>% count_(columnToLookAt) %>%
            summarise(total = sum(n))
    }
}


# Sub-Cleaning Functions ----------------------------------
# Description:
#   These functions perform cleaning on specific variables in the data

# Title:
#   Clean Job Role Interests
# Description:
#   This year's survey allowed multiple choices for the job roles. So each
#   multiple choice got their own column and needs to be converted to more
#   boolean values.
# Usage:
#   > cleanJob <- clean_job_interest(part)
clean_job_interest <- function(part) {
    cat("Cleaning responses for job interests...\n")

    # Job interest columns that need to be changed
    colsChange <- c("JobInterestFullStack", "JobInterestBackEnd",
                    "JobInterestFrontEnd", "JobInterestMobile",
                    "JobInterestDevOps", "JobInterestDataSci",
                    "JobInterestQAEngr", "JobInterestUX",
                    "JobInterestProjMngr", "JobInterestGameDev",
                    "JobInterestDataEngr", "JobInterestInfoSec")

    cat("Finished cleaning responses for job interests.\n")
    part %>% char_to_one(changeCols = colsChange)
}


# Title:
#   Clean Other Job Role Interest
# Description:
#   This function targets the other job interests people put down and performs
#   some cleaning:
#   - Normalize variants of "Undecided"
#   - Normalize variants of "Cyber Security"
#   - Normalize variants of "Game Developer"
#   - Normalize variants of "Software Engineer"
# Usage:
#   > cleanPart <- clean_job_interest_other(part)
clean_job_interest_other <- function(part) {
    cat("Cleaning responses for other job interests...\n")

    ## Title case answers for other job interests
    ##  See if I can simplify this by just mutating
    jobRoleOtherYes <- part %>% filter(!is.na(JobInterestOther)) %>%
        mutate(JobInterestOther = simple_title_case(JobInterestOther))
    jobRoleOtherNo <- part %>% filter(is.na(JobInterestOther))
    cleanPart <- jobRoleOtherNo %>% bind_rows(jobRoleOtherYes)

    ## Change uncertain job roles to "Undecided"
    undecidedWords <- c("not sure", "don't know", "not certain",
                        "unsure", "dont know", "undecided",
                        "all of the above", "no preference", "not",
                        "any", "no idea", "idk", "dunno")
    cleanPart <- normalize_text(inData = cleanPart,
                                columnName = "JobInterestOther",
                                searchTerms = undecidedWords,
                                replaceWith = "Undecided")

    cat("Finished cleaning responses for other job interests.\n")
    cleanPart
}


# Title:
#   Clean Expected Earnings
# Description:
#   For the expected earnings part of the survey, this function performs all
#   the necessary cleaning on that part of the data
clean_expected_earnings <- function(cleanPart1) {
    cat("Cleaning responses for expected earnings...\n")

    # Change all values to numeric for easier manipulation
    cleanPart1 <- cleanPart1 %>%
        mutate(ExpectedEarning = as.integer(ExpectedEarning))

    # Expected values < 19 set to NA
    # Too weird to be monthly income and too small for yearly
    below19 <- cleanPart1 %>%
        filter(ExpectedEarning < 19)
    change19 <- below19 %>%
        mutate(ExpectedEarning = NA)
    cleanPart1 <- cleanPart1 %>% setdiff(below19) %>% bind_rows(change19)

    # Multiply expected 20--200 by 1000
    # Too small for monthly, large enough to be annual if 1000x
    values20to200 <- cleanPart1 %>%
        filter(ExpectedEarning >= 20) %>%
        filter(ExpectedEarning <= 200)
    change20to200 <- values20to200 %>%
        mutate(ExpectedEarning = ExpectedEarning * 1000)
    cleanPart1 <- cleanPart1 %>% setdiff(values20to200) %>%
        bind_rows(change20to200)

    # Remove expected values 201--499
    # Too high for annual, too small for monthly
    values201to499 <- cleanPart1 %>%
        filter(ExpectedEarning >= 201) %>%
        filter(ExpectedEarning <= 499)
    change201to499 <- values201to499 %>%
        mutate(ExpectedEarning = NA)
    cleanPart1 <- cleanPart1 %>% setdiff(values201to499) %>%
        bind_rows(change201to499)

    # Multiply values 500--5999 by 12
    # Looks like monthly salary for poor and middle-rich countries
    values500to5999 <- cleanPart1 %>%
        filter(ExpectedEarning >= 500) %>%
        filter(ExpectedEarning <= 5999)
    change500to5999 <- values500to5999 %>%
        mutate(ExpectedEarning = ExpectedEarning * 12)
    cleanPart1 <- cleanPart1 %>% setdiff(values500to5999) %>%
        bind_rows(change500to5999)

    # Change to correct integers e.g. change 0000000 to just 0
    cleanPart1 <- cleanPart1 %>%
        mutate(ExpectedEarning = as.character(ExpectedEarning))

    cat("Finished cleaning responses for expected earnings.\n")
    cleanPart1
}


# Title:
#   Clean Code Events
# Description:
#   The function performs various transformations to coding event data to make
#   it consistent (e.g. fix spelling) and normalize instances of answers to be
#   the same. Also, new columns are made for mentions that appear more than
#   1.5% of all other code events.
# Note: honorable mentions to:
#   - "LaunchCode"
# Usage:
#   > cleanPart <- clean_code_events(cleanPart)
clean_code_events <- function(cleanPart1) {
    cat("Cleaning responses for coding events...\n")

    # Convert coding events to binary/boolean
    codeResources <- cleanPart1 %>%
        select(starts_with("CodeEvent"), -CodeEventOther, -CodeEvent) %>%
        mutate_each(funs(ifelse(!is.na(.), "1", NA)))
    cleanPart1 <- cleanPart1 %>%
        select(-starts_with("CodeEvent"), CodeEventOther, CodeEvent) %>%
        bind_cols(codeResources)

    # Title case other coding events
    codingEvents <- cleanPart1 %>% filter(!is.na(CodeEventOther)) %>%
        mutate(CodeEventOther = simple_title_case(CodeEventOther))
    codeEventsElse <- cleanPart1 %>% filter(is.na(CodeEventOther))
    cleanPart1 <- codeEventsElse %>% bind_rows(codingEvents)

    # Normalize variations of "None"
    nones <- c("non", "none", "haven't", "havent", "not", "nothing",
               "didn't", "n/a", "\bna\b", "never", "nil", "nope")
    searchStr <- paste(nones, collapse = "|")
    nonesIdx <- cleanPart1 %>% select(CodeEventOther) %>%
        mutate_each(funs(grepl(searchStr, ., ignore.case = TRUE))) %>%
        unlist(use.names = FALSE)
    nonesData <- cleanPart1 %>% filter(nonesIdx) %>%
        mutate(CodeEventOther = NA) %>%
        mutate(CodeEventNone = "1")
    cleanPart1 <- cleanPart1 %>% filter(!nonesIdx) %>% bind_rows(nonesData)

    cat("Finished cleaning responses for coding events.\n")
    cleanPart1
}


# Title:
#   Clean Podcasts
# Description:
#   Cleans the podcasts answers in general but mostly cleans people's answers
#   for "Other". Performs the following:
#   - Convert Podcasts to binary/boolean
#   - Normalize variations of "None" in Podcast Other back to designated col
#   - Normalize variations of "Software Engineering Daily" in Podcast Other
#     back to designated column
#   - Add new columns for other podcasts greater than 1.5% of mentions in Other
#       - "Ruby Rogues"
#       - "Shop Talk Show"
#       - "Developer Tea"
#       - "Programming Throwdown"
#       - ".NET Rocks"
#       - "Talk Python to Me"
#       - "JavaScript Air"
#       - The Web Ahead"
#   - NOTE: honorable mentions to get their own column
#       - "Code Pen Radio"
#       - "Trav and Los"
#       - "Giant Robots Smashing into other Giant Robots"
clean_podcasts <- function(cleanPart) {
    cat("Cleaning responses for other podcasts...\n")

    # Convert Podcasts to binary/boolean
    podcasts <- cleanPart %>%
        select(starts_with("Podcast"), -PodcastOther, -Podcast) %>%
        mutate_each(funs(ifelse(!is.na(.), "1", NA)))
    cleanPart <- cleanPart %>%
        select(-starts_with("Podcast"), PodcastOther, Podcast) %>%
        bind_cols(podcasts)

    # Normalize variations of "None" in PodcastOther
    nonePod <- c("non", "none", "haven't", "havent", "not a",
                 "nothing", "didn't", "n/a", "\bna\b", "never",
                 "\bnil\b", "nope", "not tried", "have not", "do not",
                 "don't", "\bno\b", "\bno one\b", "iduntlitsentopodcasts",
                 "does not", "no idea", "not applicable")
    searchStr <- paste(nonePod, collapse = "|")
    nonesPodIdx <- cleanPart %>% select(PodcastOther) %>%
        mutate_each(funs(grepl(searchStr, ., ignore.case = TRUE))) %>%
        unlist(use.names = FALSE)
    nonesPodData <- cleanPart %>% filter(nonesPodIdx) %>%
        mutate(PodcastOther = NA) %>%
        mutate(PodcastNone = "1")
    cleanPart <- cleanPart %>% filter(!nonesPodIdx) %>%
        bind_rows(nonesPodData)

    cat("Finished cleaning responses for other podcasts.\n")
    cleanPart
}


# Title:
#   Clean Hours Learned Per Week
# Usage:
#   > cleanPart <- clean_hours_learn(cleanPart)
clean_hours_learn <- function(cleanPart) {
    cat("Cleaning responses for hours of learning per week...\n")

    # Remove the word "hour(s)"
    hoursIdx <- cleanPart %>% select(HoursLearning) %>%
        mutate_each(funs(grepl("hours", ., ignore.case = TRUE))) %>%
        unlist(use.names = FALSE)
    hoursData <- cleanPart %>% filter(hoursIdx) %>%
        mutate(HoursLearning = sub("hours.*", "", HoursLearning))
    cleanPart <- cleanPart %>% filter(!hoursIdx) %>% bind_rows(hoursData)

    # Remove hyphen and "to" for ranges of hours
    rangeHrIdx <- cleanPart %>% select(HoursLearning) %>%
        mutate_each(funs(grepl("-|to", ., ignore.case = TRUE))) %>%
        unlist(use.names = FALSE)
    rangeHrData <- cleanPart %>% filter(rangeHrIdx) %>%
        mutate(HoursLearning = average_string_range(HoursLearning))
    cleanPart <- cleanPart %>% filter(!rangeHrIdx) %>%
        bind_rows(rangeHrData)

    # Remove hours greater than 100 hours
    cleanPart <- cleanPart %>%
        mutate(HoursLearning = as.integer(HoursLearning)) %>%
        mutate(HoursLearning = ifelse(HoursLearning > 100, NA, HoursLearning))

    cat("Finished cleaning responses for hours of learning per week.\n")
    cleanPart
}


# Title:
#   Clean Money Spent on Learning
# Usage:
#   > cleanPart <- clean_money_learning(cleanPart)
clean_money_learning <- function(cleanPart) {
    cat("Cleaning responses for money used for learning...\n")

    # Floor values to nearest dollar using as.integer
    cleanPart <- cleanPart %>%
        mutate(MoneyForLearning = as.integer(MoneyForLearning))

    # Remove outliers of greater than $250,000
    cleanPart <- cleanPart %>%
        mutate(MoneyForLearning = remove_outlier(MoneyForLearning, 250000))

    cat("Finished cleaning responses for money used for learning.\n")
    cleanPart
}


# Title:
#   Clean Age
# Usage:
#   > cleanPart <- clean_age(cleanPart)
clean_age <- function(cleanPart) {
    cat("Cleaning responses for age...\n")

    # Make ages >100 equal to NA
    cleanPart <- cleanPart %>%
        mutate(Age = remove_outlier(Age, 100))

    cat("Finished cleaning responses for age.\n")
    cleanPart
}


# Title:
#   Clean Mortgage Amount
# Usage:
#   > cleanPart <- clean_mortgage_amt(cleanPart)
clean_mortgage_amt <- function(cleanPart) {
    # Make values to integer so that it'll remove odd answers like 00000
    cleanPart <- cleanPart %>%
        mutate(HomeMortgageOwe = as.integer(HomeMortgageOwe))

    # Make minumum mortgage amount $1000
    cleanPart <- cleanPart %>%
        mutate(HomeMortgageOwe = ifelse(HomeMortgageOwe < 1000,
                                        yes = NA,
                                        no = HomeMortgageOwe))

    # Make maximum mortgage amount $1000000
    cleanPart <- cleanPart %>%
        mutate(HomeMortgageOwe = ifelse(HomeMortgageOwe > 1000000,
                                        yes = NA,
                                        no = HomeMortgageOwe))

    cleanPart
}


# Title:
#   Clean Income
# Usage:
#   > cleanPart <- clean_income(cleanPart)
clean_income <- function(cleanPart) {
    cat("Cleaning responses for income...\n")

    # Change all values to numeric for easier manipulation
    cleanPart <- cleanPart %>%
        mutate(Income = as.integer(Income))

    # Expected values < 19 set to NA
    # Too weird to be monthly income and too small for yearly
    below19 <- cleanPart %>%
        filter(Income < 19)
    change19 <- below19 %>%
        mutate(Income = NA)
    cleanPart <- cleanPart %>% setdiff(below19) %>% bind_rows(change19)

    # Multiply expected 20--200 by 1000
    # Too small for monthly, large enough to be annual if 1000x
    values20to200 <- cleanPart %>%
        filter(Income >= 20) %>%
        filter(Income <= 200)
    change20to200 <- values20to200 %>%
        mutate(Income = Income * 1000)
    cleanPart <- cleanPart %>% setdiff(values20to200) %>%
        bind_rows(change20to200)

    # Remove expected values 201--499
    # Too high for annual, too small for monthly
    values201to499 <- cleanPart %>%
        filter(Income >= 201) %>%
        filter(Income <= 499)
    change201to499 <- values201to499 %>%
        mutate(Income = NA)
    cleanPart <- cleanPart %>% setdiff(values201to499) %>%
        bind_rows(change201to499)

    # Multiply values 500--5999 by 12
    # Looks like monthly salary for poor and middle-rich countries
    values500to5999 <- cleanPart %>%
        filter(Income >= 500) %>%
        filter(Income <= 5999)
    change500to5999 <- values500to5999 %>%
        mutate(Income = Income * 12)
    cleanPart <- cleanPart %>% setdiff(values500to5999) %>%
        bind_rows(change500to5999)

    # Set limit to 1,000,000
    values1m <- cleanPart %>%
        filter(Income > 1000000)
    change1m <- values1m %>%
        mutate(Income = 1000000)
    cleanPart <- cleanPart %>% setdiff(values1m) %>%
        bind_rows(change1m)

    cat("Finished cleaning income.\n")
    cleanPart
}


# Title:
#   Clean Other Resources
# Description:
#   Searches for mentions of other resources and creates new columns for
#   mentions greater than 1% of responses for other resources. Columns
#   targeted:
#   - "Treehouse"                      - "Lynda.com"
#   - "Stack Overflow"                 - "Books"
#   - "W3Schools"                      - "Skillcrush"
#   - "YouTube"                        - "Google"
#   - "HackerRank"                     - "Reddit"
#   - "Mozilla MDN"                    - "SoloLearn"
#   - "Blogs"                          - "EggHead"
# Note: honorable mentions to:
#   - "Laracasts"
#   - "StackExchange"
#   - "Project Euler"
#   - "CSS Tricks"
#   - "The New Boston"
#   - "Frontend Masters"
# Usage:
#   > cleanPart <- clean_resources(cleanPart)
clean_resources <- function(cleanPart) {
    cat("Cleaning responses for other resources...\n")

    # Convert Resources to binary/boolean
    resources <- cleanPart %>%
        select(starts_with("Resource"), -ResourceOther, -Resources) %>%
        mutate_each(funs(ifelse(!is.na(.), "1", NA)))
    cleanPart <- cleanPart %>%
        select(-starts_with("Resource"), ResourceOther, Resources) %>%
        bind_cols(resources)

    cat("Finished cleaning responses for other resources.\n")
    cleanPart
}


# Title:
#   Clean Student Debt Amount
# Description:
#   Remove commas, set minimum to $1000, set maximum to $500000, and make
#   individuals' answers who have zero debt consistent with `HasStudentDebt`
# Usage:
#   cleanPart <- clean_student_debt(cleanPart)
clean_student_debt <- function(cleanPart) {
    cat("Cleaning responses for student debt owed...\n")

    # Make mimimum student debt to $100
    cleanPart <- cleanPart %>%
        mutate(StudentDebtOwe = ifelse(StudentDebtOwe < 100,
                                       yes = NA,
                                       no = StudentDebtOwe))

    # Make maximum student debt to $500000
    cleanPart <- cleanPart %>%
        mutate(StudentDebtOwe = ifelse(StudentDebtOwe > 500000,
                                       yes = NA,
                                       no = StudentDebtOwe))

    # Change back to character
    cleanPart <- cleanPart %>%
        mutate(StudentDebtOwe = as.character(StudentDebtOwe))

    cat("Finished cleaning responses for student debt owed.\n")
    cleanPart
}


# Title:
#   Clean Children
# Description:
#   Make sure there's consistency in answering you have no children. In other
#   words, if you answered "No" for not having children, you should not have a
#   response to how many children you have (i.e. NA).
# Usage:
#   cleanPart <- clean_children(cleanPart)
clean_children <- function(cleanPart) {
    cat("Cleaning responses for number of children...\n")

    naChildren <- cleanPart %>%
        filter(is.na(ChildrenNumber))
    children <- cleanPart %>%
        filter(!is.na(ChildrenNumber)) %>%
        mutate(HasChildren = ifelse(ChildrenNumber == 0, 0, HasChildren)) %>%
        mutate(ChildrenNumber =
                   ifelse(ChildrenNumber == 0, NA, ChildrenNumber))
    cleanPart <- bind_rows(naChildren, children)

    # Remove outlier children
    cleanPart <- cleanPart %>%
        mutate(ChildrenNumber = ifelse(ChildrenNumber > 10, NA, ChildrenNumber))

    cat("Finished cleaning responses for number of children.\n")
    cleanPart
}


# Title:
#   Clean YouTube
# Description:
#   This year's survey allowed multiple choices for the YouTube channels you
#   watch. So each multiple choice got their own column and needs to be
#   converted to more boolean values.
# Usage:
#   > cleanJob <- clean_youtube(part)
clean_youtube <- function(part) {
    cat("Cleaning responses for YouTube channels watched...\n")

    # Job interest columns that need to be changed
    colsChange <- c("YouTubeCodeCourse", "YouTubeCodingTrain",
                    "YouTubeCodingTut360", "YouTubeComputerphile",
                    "YouTubeDerekBanas", "YouTubeDevTips",
                    "YouTubeEngineeredTruth", "YouTubeFCC",
                    "YouTubeFunFunFunction", "YouTubeGoogleDev",
                    "YouTubeLearnCode", "YouTubeLevelUpTuts",
                    "YouTubeMIT", "YouTubeMozillaHacks",
                    "YouTubeSimplilearn", "YouTubeTheNewBoston")

    cat("Finished cleaning responses for YouTube channels watched.\n")
    part %>% char_to_one(changeCols = colsChange)
}

# Main Process Functions ----------------------------------
# Description:
#   These functions encompass the bulk work of the cleaning and transformation

# Title:
#   Read in Data
# Description:
#   First function that should be run to read in the data for cleaning
# Usage:
#   > allData <- read_in_data(part1Path, part2Path)
#   > allData$part1 # access first part
#   > allData$part2 # access second part
read_in_data <- function(part1Path, part2Path) {
    cat("Reading in survey data for cleaning...\n")

    # Read in data
    survey1 <- read.csv(
        file = part1Path,
        stringsAsFactors = FALSE,
        na.strings = "") %>% tbl_df()
    survey2 <- read.csv(
        file = part2Path,
        stringsAsFactors = FALSE,
        na.strings = "") %>% tbl_df()

    cat("Finished reading in survey data.\n")
    list(part1 = survey1, part2 = survey2)
}


# Title:
#   Rename Part 1 of Survey
# Description:
#   Changes column/variable names from questions to something easier to
#   remember and use
# Usage:
#   > cleanPart1 <- rename_part_1(dat$part1)
rename_part_1 <- function(part1) {
    cat("Renaming Part 1 of the survey...\n")
    newPart1 <- part1 %>% rename(
        ID = X.
    ) %>% rename(
        IsSoftwareDev = Are.you.already.working.as.a.software.developer.
    ) %>% rename(
        FirstDevJob = Is.this.your.first.software.development.job.
    ) %>% rename(
        JobPref = Would.you.prefer.to...
    ) %>% rename(
        JobInterestFullStack = Full.Stack.Web.Developer
    ) %>% rename(
        JobInterestBackEnd = Back.End.Web.Developer
    ) %>% rename(
        JobInterestFrontEnd = X..Front.End.Web.Developer
    ) %>% rename(
        JobInterestMobile = X..Mobile.Developer
    ) %>% rename(
        JobInterestDevOps = X..DevOps...SysAdmin
    ) %>% rename(
        JobInterestDataSci = X..Data.Scientist
    ) %>% rename(
        JobInterestQAEngr = X..Quality.Assurance.Engineer
    ) %>% rename(
        JobInterestUX = X..User.Experience.Designer
    ) %>% rename(
        JobInterestProjMngr = X..Product.Manager
    ) %>% rename(
        JobInterestGameDev = Game.Developer
    ) %>% rename(
        JobInterestInfoSec = Information.Security
    ) %>% rename(
        JobInterestDataEngr = Data.Engineer
    ) %>% rename(
        JobInterestOther = Other
    ) %>% rename(
        JobApplyWhen = When.do.you.plan.to.start.applying.for.developer.jobs.
    ) %>% rename(
        ExpectedEarning = About.how.much.money.do.you.expect.to.earn.per.year.at.your.first.developer.job..in.US.Dollars..
    ) %>% rename(
        JobWherePref = Would.you.prefer.to.work...
    ) %>% rename(
        JobRelocate = Are.you.willing.to.relocate.for.a.job.
    ) %>% rename(
        ResourceFCC = freeCodeCamp
    ) %>% rename(
        ResourceEdX = EdX
    ) %>% rename(
        ResourceCoursera = Coursera
    ) %>% rename(
        ResourceKA = Khan.Academy
    ) %>% rename(
        ResourcePluralSight = Pluralsight...Code.School
    ) %>% rename(
        ResourceCodecademy = Codecademy
    ) %>% rename(
        ResourceUdacity = Udacity
    ) %>% rename(
        ResourceUdemy = Udemy
    ) %>% rename(
        ResourceCodeWars = Code.Wars
    ) %>% rename(
        ResourceOdinProj = The.Odin.Project
    ) %>% rename(
        ResourceTreehouse = Treehouse
    ) %>% rename(
        ResourceLynda = Lynda.com
    ) %>% rename(
        ResourceSO = Stack.Overflow
    ) %>% rename(
        ResourceW3S = W3Schools
    ) %>% rename(
        ResourceSkillcrush = Skillcrush
    ) %>% rename(
        ResourceHackerRank = HackerRank
    ) %>% rename(
        ResourceMDN = Mozilla.Developer.Network..MDN.
    ) %>% rename(
        ResourceEgghead = Egghead.io
    ) %>% rename(
        ResourceCSS = CSS.Tricks
    ) %>% rename(
        ResourceOther = Other.1
    ) %>% rename(
        CodeEventFCC = freeCodeCamp.study.groups
    ) %>% rename(
        CodeEventHackathons = hackathons
    ) %>% rename(
        CodeEventConferences = conferences
    ) %>% rename(
        CodeEventWorkshops = workshops
    ) %>% rename(
        CodeEventStartUpWknd = Startup.Weekend
    ) %>% rename(
        CodeEventNodeSchool = NodeSchool
    ) %>% rename(
        CodeEventWomenCode = Women.Who.Code
    ) %>% rename(
        CodeEventGirlDev = Girl.Develop.It
    ) %>% rename(
        CodeEventMeetup = Meetup.com.events
    ) %>% rename(
        CodeEventRailsBridge = RailsBridge
    ) %>% rename(
        CodeEventGameJam = Game.Jam
    ) %>% rename(
        CodeEventRailsGirls = Rails.Girls
    ) %>% rename(
        CodeEventDjangoGirls = Django.Girls
    ) %>% rename(
        CodeEventWkdBootcamps = weekend.bootcamps
    ) %>% rename(
        CodeEventOther = Other.2
    ) %>% rename(
        PodcastCodeNewbie = Code.Newbie
    ) %>% rename(
        PodcastChangeLog = The.Changelog
    ) %>% rename(
        PodcastSEDaily = Software.Engineering.Daily
    ) %>% rename(
        PodcastJSJabber = JavaScript.Jabber
    ) %>% rename(
        PodcastRubyRogues = Ruby.Rogues
    ) %>% rename(
        PodcastShopTalk = Shop.Talk.Show
    ) %>% rename(
        PodcastDevTea = Developer.Tea
    ) %>% rename(
        PodcastProgThrowdown = Programming.Throwdown
    ) %>% rename(
        PodcastDotNET = .NET.Rocks
    ) %>% rename(
        PodcastTalkPython = Talk.Python.To.Me
    ) %>% rename(
        PodcastJSAir = JavaScript.Air
    ) %>% rename(
        PodcastTheWebAhead = The.Web.Ahead
    ) %>% rename(
        PodcastCodePen = CodePen.Radio
    ) %>% rename(
        PodcastGiantRobots = Giant.Robots.Smashing.into.Other.Giant.Robots
    ) %>% rename(
        PodcastSERadio = Software.Engineering.Radio
    ) %>% rename(
        PodcastOther = Other.3
    ) %>% rename(
        YouTubeMIT = MIT.Open.Courseware
    ) %>% rename(
        YouTubeTheNewBoston = The.New.Boston
    ) %>% rename(
        YouTubeFCC = freeCodeCamp.1
    ) %>% rename(
        YouTubeComputerphile = Computerphile
    ) %>% rename(
        YouTubeDevTips = DevTips
    ) %>% rename(
        YouTubeEngineeredTruth = Engineered.Truth
    ) %>% rename(
        YouTubeLearnCode = LearnCode.Academy
    ) %>% rename(
        YouTubeCodeCourse = CodeCourse
    ) %>% rename(
        YouTubeLevelUpTuts = LevelUpTuts
    ) %>% rename(
        YouTubeFunFunFunction = funfunfunction
    ) %>% rename(
        YouTubeCodingTut360 = Coding.Tutorials.360
    ) %>% rename(
        YouTubeCodingTrain = Coding.Train..Coding.Rainbow.
    ) %>% rename(
        YouTubeDerekBanas = Derek.Banas
    ) %>% rename(
        YouTubeSimplilearn = Simplilearn
    ) %>% rename(
        YouTubeMozillaHacks = Mozilla.Hacks
    ) %>% rename(
        YouTubeGoogleDev = Google.Developers
    ) %>% rename(
        YouTubeOther = Other.4
    ) %>% rename(
        HoursLearning = About.how.many.hours.do.you.spend.learning.each.week.
    ) %>% rename(
        MonthsProgramming = About.how.many.months.have.you.been.programming.for.
    ) %>% rename(
        BootcampYesNo = Have.you.attended.a.full.time.coding.bootcamp.
    ) %>% rename(
        BootcampName = Which.one.
    ) %>% rename(
        BootcampFinish = Have.you.finished.yet.
    ) %>% rename(
        BootcampLoan = Did.you.take.out.a.loan.to.pay.for.the.bootcamp.
    ) %>% rename(
        BootcampRecommend = Based.on.your.experience..would.you.recommend.this.bootcamp.to.your.friends.
    ) %>% rename(
        MoneyForLearning = Aside.from.university.tuition..about.how.much.money.have.you.spent.on.learning.to.code.so.far..in.US.dollars..
    ) %>% rename(
        Part1StartTime = Start.Date..UTC.
    ) %>% rename(
        Part1EndTime = Submit.Date..UTC.
    ) %>% rename(
        NetworkID = Network.ID
    )

    cat("Finished renaming Part 1 of the survey.\n")
    newPart1
}


# Title:
#   Clean Survey
# Usage:
#   > final <- clean_part(allData)
clean_part <- function(part) {
    cat("Beginning cleaning of data...\n")

    # Clean each column that needs it
    cleanPart <- clean_job_interest(part) # Clean Job Roles to Binary
    cleanPart <- clean_job_interest_other(cleanPart)  # Clean Job Role Interests
    cleanPart <- clean_expected_earnings(cleanPart)  # Clean expected earnings
    cleanPart <- clean_code_events(cleanPart)   # Clean other coding events
    cleanPart <- clean_podcasts(cleanPart)   # Clean Podcasts Other
    cleanPart <- clean_money_learning(cleanPart)  # Clean money for learning
    cleanPart <- clean_age(cleanPart)  # Clean age
    cleanPart <- clean_mortgage_amt(cleanPart)  # Clean mortgage amount
    cleanPart <- clean_income(cleanPart)  # Clean income
    cleanPart <- clean_resources(cleanPart)  # Clean other resources
    cleanPart <- clean_student_debt(cleanPart)  # Clean student debt amount
    cleanPart <- clean_children(cleanPart)  # Clean children responses
    cleanPart <- clean_youtube(cleanPart)  # Clean YouTube channels

    # Remove unnecessary columns
    cleanPart <- cleanPart %>%
        select(-OneTwoDiff, -Resources, -Podcast, -CodeEvent, -YouTube)

    cat("Finished cleaning survey data.\n")
    cleanPart
}


# Title:
#   Rename Part 2 of Survey
# Description:
#   Changes column/variable names from questions to something easier to
#   remember and use
# Usage:
#   > part2 <- rename_part_2(dat$part2)
rename_part_2 <- function(part2) {
    cat("Renaming Part 2 of the survey...\n")
    newPart2 <- part2 %>% rename(
        ID = X.
    ) %>% rename(
        Age = How.old.are.you.
    ) %>% rename(
        Gender = What.s.your.gender.
    ) %>% rename(
        GenderOther = Other
    ) %>% rename(
        CountryCitizen = Which.country.are.you.a.citizen.of.
    ) %>% rename(
        CountryLive = Which.country.do.you.currently.live.in.
    ) %>% rename(
        CityPopulation = About.how.many.people.live.in.your.city.
    ) %>% rename(
        IsEthnicMinority = Are.you.an.ethnic.minority.in.your.country.
    ) %>% rename(
        LanguageAtHome = Which.language.do.you.you.speak.at.home.with.your.family.
    ) %>% rename(
        SchoolDegree = What.s.the.highest.degree.or.level.of.school.you.have.completed.
    ) %>% rename(
        SchoolMajor = What.was.the.main.subject.you.studied.in.university.
    ) %>% rename(
        MaritalStatus = What.s.your.marital.status.
    ) %>% rename(
        HasFinancialDependents = Do.you.financially.support.any.dependents.
    ) %>% rename(
        HasChildren = Do.you.have.children.
    ) %>% rename(
        ChildrenNumber = How.many.children.do.you.have.
    ) %>% rename(
        FinanciallySupporting = Do.you.financially.support.any.elderly.relatives.or.relatives.with.disabilities.
    ) %>% rename(
        DebtAmount = Do.you.have.any.debt.
    ) %>% rename(
        HasHomeMortgage = Do.you.have.a.home.mortgage.
    ) %>% rename(
        HomeMortgageOwe = About.how.much.do.you.owe.on.your.home.mortgage..in.US.Dollars..
    ) %>% rename(
        HasStudentDebt = Do.you.have.student.loan.debt.
    ) %>% rename(
        StudentDebtOwe = About.how.much.do.you.owe.in.student.loans..in.US.Dollars..
    ) %>% rename(
        EmploymentStatus = Regarding.employment.status..are.you.currently...
    ) %>% rename(
        EmploymentStatusOther = Other.1
    ) %>% rename(
        EmploymentField = Which.field.do.you.work.in.
    ) %>% rename(
        EmploymentFieldOther = Other.2
    ) %>% rename(
        Income = About.how.much.money.did.you.make.last.year..in.US.dollars..
    ) %>% rename(
        CommuteTime = About.how.many.minutes.does.it.take.you.to.get.to.work.each.day.
    ) %>% rename(
        IsUnderEmployed = Do.you.consider.yourself.under.employed.
    ) %>% rename(
        HasServedInMilitary = Have.you.served.in.your.country.s.military.before.
    ) %>% rename(
        IsReceiveDisabilitiesBenefits = Do.you.receive.disability.benefits.from.your.government.
    ) %>% rename(
        HasHighSpdInternet = Do.you.have.high.speed.internet.at.your.home.
    ) %>% rename(
        IsSoftwareDev = already_working
    ) %>% rename(
        JobRoleInterest = jobs_interested_in
    ) %>% rename(
        JobPref = want_employment_type
    ) %>% rename(
        ExpectedEarning = expected_earnings
    ) %>% rename(
        JobWherePref = home_or_remote
    ) %>% rename(
        JobRelocate = will_relocate
    ) %>% rename(
        CodeEvent = attended_event_types
    ) %>% rename(
        Resources = learning_resources
    ) %>% rename(
        HoursLearning = hours_learning_week
    ) %>% rename(
        MonthsProgramming = months_learning
    ) %>% rename(
        BootcampYesNo = attend_bootcamp
    ) %>% rename(
        BootcampName = which_bootcamp
    ) %>% rename(
        BootcampFinish = finished_bootcamp
    ) %>% rename(
        BootcampLoan = loan_for_bootcamp
    ) %>% rename(
        BootcampRecommend = recommend_bootcamp
    ) %>% rename(
        MoneyForLearning = total_spent_learning
    ) %>% rename(
        Podcast = podcast
    ) %>% rename(
        JobApplyWhen = how_soon_jobhunt
    ) %>% rename(
        YouTube = youtube
    ) %>% rename(
        Part2StartTime = Start.Date..UTC.
    ) %>% rename(
        Part2EndTime = Submit.Date..UTC.
    ) %>% rename(
        NetworkID = Network.ID
    )

    cat("Finished renaming Part 2 of the survey.\n")
    newPart2
}


# Title:
#   Remove obvious outliers
# Description:
#   This function removes obvious outliers, which most likely are fake because
#   they don't even try to answer the question
# Usage:
# part1 <- rm_obv_outliers(part1)
rm_obv_outliers <- function(part) {
    part
}


# Title:
#   Standardize Data Types
# Description:
#   There were some inconsistent encoding of data between the two datasets.
#       - Values passed from first part of survey were passed in as strings. So
#         empty answers were passed on as "undefined"
#       - The second dataset encoded yes/no answers as those strings, versus
#         the first dataset encoding it as 1/0
#       - For the question on when you plan on applying for jobs, some of the
#         answers were truncated at the apostrophe, so the answers were fixed
#         to what they were.
#       - Lastly, some of the numeric values were read into R as numeric, but
#         some were read in as double. So these numeric data types were
#         standardized to either character or double for ease of use later.
std_data_type <- function(part1, part2) {
    cat("Standardizing variables between data for joining...\n")

    # Change "Yes"/"No" to "1"/"0"
    changeCols <- c("IsSoftwareDev", "JobRelocate", "BootcampYesNo",
                    "BootcampFinish", "BootcampLoan", "BootcampRecommend")
    part2 <- yesNo_to_oneZero(part2, changeCols)

    # Fix truncated answers in when you're applying to jobs question
    part2 <- part2 %>%
        mutate(JobApplyWhen = fix_truncate_job_apply(JobApplyWhen))

    # Standardize data types between data sets to allow joining
    toChr <- c("IsSoftwareDev", "JobRelocate", "BootcampYesNo",
               "BootcampFinish", "BootcampLoan", "BootcampRecommend",
               "ExpectedEarning", "MonthsProgramming", "MoneyForLearning",
               "HoursLearning")
    part1 <- change_to_chr(part1, toChr)

    cat("Finished standardizing variables between data.\n")
    list(part1 = part1, part2 = part2)
}


# Title:
#   Joining Data Sets (WIP)
# Description:
#   Uses shared information in the columns between the data sets to join them
#   together
join_data <- function(part1, part2) {
    # Different shared columns between the two parts
    sharedCols1 <- c("JobInterestOther", "ResourceOther", "CodeEventOther",
                     "PodcastOther", "YouTubeOther")
    sharedCols2 <- c("JobRoleInterest")

    # Static answers for job interest
    jobInterests <- c("Full-Stack Web Developer", "Back-End Web Developer",
                      "Front-End Web Developer", "Mobile Developer",
                      "DevOps / SysAdmin", "Data Scientist",
                      "Quality Assurance Engineer", "User Experience Designer",
                      "Project Manager", "Game Developer",
                      "Information Security", "Data Engineer")
    part2 %>% separate_rows(sharedCols2[1], sep = ",")
}


# Title:
#   Survey Parts Sanity Check
# Description:
#   After joining the two datasets together, there were some inconsistencies
#   with the survey times starting and ending. For example, for some joined
#   rows, the first part end time was more than 5 minutes before the second
#   part of the survey started.
#
#   This function first changes the Part 1 end time and the Part 2 start time
#   to the appropriate date datatype before manipulating them.
#
#   Cases:
#       - Remove negative time differences where 2nd part of survey started
#         before the 1st part ended
#       - Multiple first part ID's used in conjunction with second part ID's
#       - Multiple second part ID's used in conjunction with first part ID's
# Input:
#   dplyr data frame with joined datasets
# Output:
#   dplyr data frame with joined datasets
# Usage:
#   > allData <- time_diff_check(allData)
time_diff_check <- function(allData) {
    cat("Checking for inconsistencies within survey after joining...\n")

    # Change data type to date so we can easily compare times
    newData <- allData %>%
        mutate(Part1EndTime = as.POSIXct(Part1EndTime)) %>%
        mutate(Part2StartTime = as.POSIXct(Part2StartTime)) %>%
        mutate(OneTwoDiff = Part2StartTime - Part1EndTime)
    newData <- newData %>%
        select(noquote(order(colnames(newData))))

    # Separate data to focus on ones that we can look at differences
    newDataNA <- newData %>% filter(is.na(OneTwoDiff))
    newDataData <- newData %>% filter(!is.na(OneTwoDiff))

    # Remove neg times i.e. 2nd part of survey started before 1st part ended
    newDataData <- newDataData %>% filter(OneTwoDiff > 0)

    # Check for multiple first ID's put to second part ID's
    id1Unique <- newDataData %>% group_by(ID.x) %>%
        filter(n() == 1)
    id1Good <- newDataData %>% group_by(ID.x) %>%
        filter(n() > 1) %>%
        mutate(minDiff = min(OneTwoDiff)) %>%
        filter(OneTwoDiff == minDiff)
    newDataData <- bind_rows(id1Unique, id1Good) %>% select(-one_of("minDiff"))

    # Check for multiple second ID's put to first part ID's
    id2Unique <- newDataData %>% group_by(ID.y) %>%
        filter(n() == 1)
    id2Good <- newDataData %>% group_by(ID.y) %>%
        filter(n() > 1) %>%
        mutate(minDiff = min(OneTwoDiff)) %>%
        filter(OneTwoDiff == minDiff)
    newDataData <- bind_rows(id2Unique, id2Good) %>% select(-one_of("minDiff"))

    # Join two pieces back together
    newData <- bind_rows(newDataData, newDataNA)

    # Ungroup data from grouping (group_by()) done above
    newData <- newData %>% ungroup()

    cat("Finished checking inconsistencies within survey after joining.\n")
    newData
}


# Title:
#   Final Polish
# Description:
#   This function does some final polishing like arranging the columns, making
#   sure the columns are of a certain type, and renaming columns.
# Usage:
#   > final <- polish_data(cleanData)
polish_data <- function(cleanData) {
    cat("Putting on finishing touches...\n")

    # Rename and confirm desired data type
    cleanData <- cleanData %>%
        mutate(Age = as.integer(Age)) %>%
        mutate(BootcampFinish = as.integer(BootcampFinish)) %>%
        mutate(BootcampLoan = as.integer(BootcampLoan)) %>%
        rename(BootcampLoanYesNo = BootcampLoan) %>%
        mutate(BootcampRecommend = as.integer(BootcampRecommend)) %>%
        mutate(BootcampYesNo = as.integer(BootcampYesNo)) %>%
        rename(AttendedBootcamp = BootcampYesNo) %>%
        mutate(ExpectedEarning = as.integer(ExpectedEarning)) %>%
        rename(HasDebt = DebtAmount) %>%
        mutate(Income = as.integer(Income)) %>%
        mutate(IsSoftwareDev = as.integer(IsSoftwareDev)) %>%
        rename(JobRelocateYesNo = JobRelocate) %>%
        mutate(JobRelocateYesNo = as.integer(JobRelocateYesNo)) %>%
        mutate(MoneyForLearning = as.integer(MoneyForLearning)) %>%
        mutate(StudentDebtOwe = as.integer(StudentDebtOwe)) %>%
        mutate(MonthsProgramming = as.integer(MonthsProgramming)) %>%
        mutate(HoursLearning = as.integer(HoursLearning))

    # Polish Job Interests
    cleanData <- cleanData %>%
        mutate(JobInterestBackEnd = as.integer(JobInterestBackEnd)) %>%
        mutate(JobInterestDataEngr = as.integer(JobInterestDataEngr)) %>%
        mutate(JobInterestDataSci = as.integer(JobInterestDataSci)) %>%
        mutate(JobInterestDevOps = as.integer(JobInterestDevOps)) %>%
        mutate(JobInterestFrontEnd = as.integer(JobInterestFrontEnd)) %>%
        mutate(JobInterestFullStack = as.integer(JobInterestFullStack)) %>%
        mutate(JobInterestGameDev = as.integer(JobInterestGameDev)) %>%
        mutate(JobInterestInfoSec = as.integer(JobInterestInfoSec)) %>%
        mutate(JobInterestMobile = as.integer(JobInterestMobile)) %>%
        mutate(JobInterestProjMngr = as.integer(JobInterestProjMngr)) %>%
        mutate(JobInterestQAEngr = as.integer(JobInterestQAEngr)) %>%
        mutate(JobInterestUX = as.integer(JobInterestUX))

    # Polish Code Events
    cleanData <- cleanData %>%
        mutate(CodeEventConferences = as.integer(CodeEventConferences)) %>%
        mutate(CodeEventFCC = as.integer(CodeEventFCC)) %>%
        mutate(CodeEventGirlDev = as.integer(CodeEventGirlDev)) %>%
        mutate(CodeEventHackathons = as.integer(CodeEventHackathons)) %>%
        mutate(CodeEventNodeSchool = as.integer(CodeEventNodeSchool)) %>%
        mutate(CodeEventNone = as.integer(CodeEventNone)) %>%
        mutate(CodeEventRailsBridge = as.integer(CodeEventRailsBridge)) %>%
        mutate(CodeEventStartUpWknd = as.integer(CodeEventStartUpWknd)) %>%
        mutate(CodeEventWomenCode = as.integer(CodeEventWomenCode)) %>%
        mutate(CodeEventMeetup = as.integer(CodeEventMeetup)) %>%
        mutate(CodeEventWkdBootcamps = as.integer(CodeEventWkdBootcamps)) %>%
        mutate(CodeEventRailsGirls = as.integer(CodeEventRailsGirls)) %>%
        mutate(CodeEventDjangoGirls = as.integer(CodeEventDjangoGirls)) %>%
        mutate(CodeEventGameJam = as.integer(CodeEventGameJam)) %>%
        mutate(CodeEventWorkshops = as.integer(CodeEventWorkshops))

    # Polish Resources
    cleanData <- cleanData %>%
        mutate(ResourceCodeWars = as.integer(ResourceCodeWars)) %>%
        mutate(ResourceCodecademy = as.integer(ResourceCodecademy)) %>%
        mutate(ResourceCoursera = as.integer(ResourceCoursera)) %>%
        mutate(ResourceCSS = as.integer(ResourceCSS)) %>%
        mutate(ResourceEdX = as.integer(ResourceEdX)) %>%
        mutate(ResourceFCC = as.integer(ResourceFCC)) %>%
        mutate(ResourceKA = as.integer(ResourceKA)) %>%
        mutate(ResourceOdinProj = as.integer(ResourceOdinProj)) %>%
        mutate(ResourcePluralSight = as.integer(ResourcePluralSight)) %>%
        mutate(ResourceUdacity = as.integer(ResourceUdacity)) %>%
        mutate(ResourceUdemy = as.integer(ResourceUdemy)) %>%
        mutate(ResourceTreehouse = as.integer(ResourceTreehouse)) %>%
        mutate(ResourceLynda = as.integer(ResourceLynda)) %>%
        mutate(ResourceSO = as.integer(ResourceSO)) %>%
        mutate(ResourceW3S = as.integer(ResourceW3S)) %>%
        mutate(ResourceSkillcrush = as.integer(ResourceSkillcrush)) %>%
        mutate(ResourceHackerRank = as.integer(ResourceHackerRank)) %>%
        mutate(ResourceMDN = as.integer(ResourceMDN)) %>%
        mutate(ResourceEgghead = as.integer(ResourceEgghead))

    # Polish Podcasts
    cleanData <- cleanData %>%
        mutate(PodcastChangeLog = as.integer(PodcastChangeLog)) %>%
        mutate(PodcastCodeNewbie = as.integer(PodcastCodeNewbie)) %>%
        mutate(PodcastCodePen = as.integer(PodcastCodePen)) %>%
        mutate(PodcastDevTea = as.integer(PodcastDevTea)) %>%
        mutate(PodcastDotNET = as.integer(PodcastDotNET)) %>%
        mutate(PodcastGiantRobots = as.integer(PodcastGiantRobots)) %>%
        mutate(PodcastJSAir = as.integer(PodcastJSAir)) %>%
        mutate(PodcastJSJabber = as.integer(PodcastJSJabber)) %>%
        mutate(PodcastNone = as.integer(PodcastNone)) %>%
        mutate(PodcastRubyRogues = as.integer(PodcastRubyRogues)) %>%
        mutate(PodcastProgThrowdown = as.integer(PodcastProgThrowdown)) %>%
        mutate(PodcastSEDaily = as.integer(PodcastSEDaily)) %>%
        mutate(PodcastSERadio = as.integer(PodcastSERadio)) %>%
        mutate(PodcastShopTalk = as.integer(PodcastShopTalk)) %>%
        mutate(PodcastTalkPython = as.integer(PodcastTalkPython)) %>%
        mutate(PodcastTheWebAhead = as.integer(PodcastTheWebAhead))

    # Polish YouTube Channels
    cleanData <- cleanData %>%
        mutate(YouTubeCodeCourse = as.integer(YouTubeCodeCourse)) %>%
        mutate(YouTubeCodingTrain = as.integer(YouTubeCodingTrain)) %>%
        mutate(YouTubeCodingTut360 = as.integer(YouTubeCodingTut360)) %>%
        mutate(YouTubeComputerphile = as.integer(YouTubeComputerphile)) %>%
        mutate(YouTubeDerekBanas = as.integer(YouTubeDerekBanas)) %>%
        mutate(YouTubeDevTips = as.integer(YouTubeDevTips)) %>%
        mutate(YouTubeEngineeredTruth = as.integer(YouTubeEngineeredTruth)) %>%
        mutate(YouTubeFCC = as.integer(YouTubeFCC)) %>%
        mutate(YouTubeFunFunFunction = as.integer(YouTubeFunFunFunction)) %>%
        mutate(YouTubeGoogleDev = as.integer(YouTubeGoogleDev)) %>%
        mutate(YouTubeLearnCode = as.integer(YouTubeLearnCode)) %>%
        mutate(YouTubeLevelUpTuts = as.integer(YouTubeLevelUpTuts)) %>%
        mutate(YouTubeMIT = as.integer(YouTubeMIT)) %>%
        mutate(YouTubeMozillaHacks = as.integer(YouTubeMozillaHacks)) %>%
        mutate(YouTubeSimplilearn = as.integer(YouTubeSimplilearn)) %>%
        mutate(YouTubeTheNewBoston = as.integer(YouTubeTheNewBoston))

    # Order columns alphabetically
    cleanData <- cleanData %>% select(noquote(order(colnames(cleanData))))

    cat("Finished last polish of data.\n")
    cleanData
}

# Main Function -------------------------------------------

# Title:
#   Main Cleaning Function
# Description:
#   This is the main cleaning and transformation function. It will write a new
#   file in the `clean-data/` directory.
# Usage:
#   > main()
main <- function(dataPath1, dataPath2) {
    dat <- read_in_data(dataPath1, dataPath2) # Read in data

    # Remove unused columns
    dat$part1 <- dat$part1 %>%
        select(-Before.you.got.this.job..how.many.months.did.you.spend.looking.for.a.job.)

    # Change column names to something easier to use
    part1 <- rename_part_1(dat$part1)
    part2 <- rename_part_2(dat$part2)

    # Remove survey-year specific outliers
    allXs <- part2 %>% filter(ExpectedEarning == "xxxxx")
    part2 <- part2 %>% setdiff(allXs)

    # Make variables between datasets consistent for joining
    consistentData <- std_data_type(part1, part2)

    # Join datasets together
    key <- c("IsSoftwareDev", "JobPref", "JobApplyWhen", "ExpectedEarning",
             "JobWherePref", "JobRelocate", "BootcampYesNo",
             "MonthsProgramming", "BootcampFinish",
             "BootcampLoan", "BootcampRecommend", "MoneyForLearning",
             "NetworkID", "HoursLearning", "BootcampName")
    allData <- left_join(consistentData$part1, consistentData$part2, by = key)

    # Check survey times and unique IDs
    allData <- time_diff_check(allData)

    # Clean both parts of the data
    cleanData <- clean_part(allData)

    # Polish data with small changes e.g. give correct data types to columns
    final <- polish_data(cleanData)

    # Combine data and create cleaned data
    write.csv(x = final,
              file = "2017-fCC-New-Coders-Survey-Data.csv",
              na = "NA",
              row.names = FALSE)
}
