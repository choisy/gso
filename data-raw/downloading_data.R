################################################################################

# general parameters:
file_pattern <- "^E\\d{2}\\.\\d{2}(-\\d{2})?\\.csv$|^E\\d{2}\\.csv$"

################################################################################

# gives the list of the names of files that where downloaded used by 'move_file'
# (and 'download').  'from' is the path of a directory
get_file <- function(from) {
    grep(file_pattern, dir(from), value = TRUE)
}

################################################################################

make_bckup <- function(where, file_list) {
    # 'where' is the path of a directory.  'file_list' is a vector of files
    # names.
    bckup0 <- bckup <- paste0(where, "/bckup")
    ind <- 0
    while (dir.exists(bckup)) {
        bckup <- paste0(bckup0, ind)
        ind <- ind + 1
    }
    dir.create(bckup)  # create the directory and then move the files to it:
    file.rename(paste0(where, "/", file_list), paste0(bckup, "/", file_list))
    bckup  # returns the path of the backup directory (for further use)
}

################################################################################

# moves all the files of 'file_name' from 'from' to 'to' used by
# 'remove_bckup' (and 'download')
move_file <- function(file_name, to, from) {
    # 'from' and 'to' are diretory paths.
    # 'file_list' is a vector of files names.
    invisible(file.rename(paste0(from, "/", get_file(from)),
                          paste0(to, "/", file_name)))
}

################################################################################

# gets the full path of the parent directory of 'x'.  used by 'remove_bckup'
remove_basename <- function(x) {
    # 'x' is a directory path.
    sub(paste0("/", basename(x)), "", x)
}

################################################################################

# move the files from the backup directory to their initial position
# (the parent directory), and remove the backup directory.
remove_bckup <- function(bckup) {
    # 'bckup' is the full path of the backup diretory.
    move_file(dir(bckup), remove_basename(bckup), bckup)
    unlink(bckup, recursive = TRUE)
}

################################################################################

# finds all the values of a vector that are duplicated.
# used by 'dealing_with_duplicated'.
find_duplicated <- function(x) {
    # 'x' is a vector of values.
    unique(x[duplicated(x)])
}

################################################################################

# replaces duplicated values by indexed values, this for all duplicated values.
# used by 'dealing_with_duplicated'.
replace_duplicated <- function(x, dupl) {
    # 'x' is a vector of values.  'dupl' is a vector of duplicated values.
    for (i in dupl) {
        sel <- which(x == i)
        x[sel] <- paste(i, seq_along(sel))
    }
    x
}

################################################################################

# deals with duplicated values by adding at their end a incrementing digit.
dealing_with_duplicated_names <- function(x) {
    # 'x' is a vector of values.
    replace_duplicated(x, find_duplicated(x))
}

################################################################################

# the main function that does the downloading.
download <- function(url, remDr, to, from) {
    # 'url' is a vector of URLs
    # 'remDr' is a remote driver 'to' and 'from' are full path of directories.
    # 'to' the directory we want to save the files to.
    # 'from' the directory the browser saves the files to.
    require(purrr)  # for 'safely'
    require(RSelenium)  # for '$navigate()', '$findElements()',
    # '$getElementAttribute()', '$getCurrentUrl()', '$clickElement()',
    # '$selectTag()', '$getElementText()'

    # cleaning the downloading directory:
    files <- get_file(from)
    if (length(files) > 0)
        bckup <- make_bckup(from, files) else bckup <- NULL

    # sets up the url:
    remDr$navigate(url)

    # selects the right pane:
    webElems <- remDr$findElements(using = "xpath", "//iframe")
    remDr$navigate(unlist(webElems[[1]]$getElementAttribute("src")))

    # saves the current URL (because paths on the links are relative paths):
    url0 <- unlist(remDr$getCurrentUrl())

    # retrieving the names of the tables (and the CSV files too eventually):
    webElems <- remDr$findElements(using = "xpath", "//li//a")
    the_texts <- unlist(lapply(webElems, function(x) x$getElementText()))
    the_texts <- gsub("/", "-", the_texts)
    the_texts <- paste0(dealing_with_duplicated_names(the_texts), ".csv")

    # for all the tables of this section: for(i in seq_along(the_texts)) {
    fff <- function(i) {

        # goes to the desired table:
        webel <- remDr$findElements(using = "class", "tableofcontent_link")[i]
        webel[[1]]$clickElement()

        # selects everything for all variables:
        variables <- remDr$findElements(using = "class",
                      "variableselector_valuesselect_select_all_imagebutton")
        for (j in variables) j$clickElement()

        # clicks on the 'Continue' button:
        variables <- remDr$findElements(using = "class",
                       "variableselector_continue_button")
        variables[[1]]$clickElement()

        # chooses the 'Semicolon delimited CSV with heading' format:
        webE <- remDr$findElements(using = "xpath",
                        "//select[@class='commandbar_saveas_dropdownlist']")
        opts <- webE[[1]]$selectTag()
        sel <- grep("CsvWithHeadingAndSemiColon", opts$value)
        opts$elements[[sel]]$clickElement()

        # clicks on the 'Save file' link:
        webE <- remDr$findElements(using = "class",
                        "commandbar_download_file_link")
        webE[[1]]$clickElement()

        # waiting for file downloading to finish:
        while (!any(grepl(file_pattern, dir(from)))) Sys.sleep(1)

        # moving and renaming the downloaded file:
        move_file(the_texts[i], to, from)

    }

    safe_fff <- safely(fff)
    for (i in seq_along(the_texts)) {
        tmp <- suppressMessages(safe_fff(i))

        # if error, scroll down a bit to have access to the rest of the page:
        if (!is.null(tmp$error)) {
            webElem <- remDr$findElement("css", "body")
            webElem$sendKeysToElement(list(key = "down_arrow"))
            tmp <- safe_fff(i)
        }
        # if error, it prints a message:
        if (!is.null(tmp$error))
            message(paste0("Failed to download '", the_texts[i], "'."))
        # going back to the initial URL:
        remDr$navigate(url0)
    }

    # in case we did a backup, replace the files to their original location:
    if (!is.null(bckup))
        remove_bckup(bckup)
}

################################################################################

library(RSelenium)  # for 'rsDriver', '$open()', '$close()', '$stop()'

# starting a Selenium server and the chrome browser:
message("Connecting to remote server...")
rD <- rsDriver(browser = "chrome", verbose = FALSE)
# headless mode using phantomJS via the
driver <- rD[["client"]]
# Selenium server doesn't support file download
driver$setImplicitWaitTimeout(milliseconds = 60000)

# getting the directories names and URLs:
driver$navigate("https://www.gso.gov.vn/Default_en.aspx?tabid=766")
webElems <- driver$findElements(using = "xpath",
                                "//div[@class = 'text_blue3']/a")
dir_names <- unlist(lapply(webElems, function(x) x$getElementText()))
urls <- unlist(lapply(webElems, function(x) x$getElementAttribute("href")))
pattern <- "^\\d\\d\\. "
sel <- grep(pattern, dir_names)
urls <- urls[sel]
dir_names <- dir_names[sel]
dir_names <- gsub(pattern, "", dir_names)

# setting the working directory:
path0 <- getwd()
if (sub(".*/", "", path0) == "gso") setwd("data-raw")

# appending downloading directories names:
to_dir <- paste0(getwd(), "/", dir_names)

# downloading the files:
for (i in seq_along(urls)) {
    cat(paste0("Downloading files to '", to_dir[i], "'...\n"))
    dir.create(to_dir[i])
    download(urls[i], driver, to_dir[i], "/Users/mac/Downloads")
}

# stops the selenium server:
rD[["server"]]$stop()

# remove everything ------------------------------------------------------------

rm(list = ls())
