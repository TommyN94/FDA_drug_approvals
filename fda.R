library(rvest)
library(magrittr)

archive_urls <- list(
  "http://wayback.archive-it.org/7993/20170111075028/http://www.fda.gov/Drugs/DevelopmentApprovalProcess/DrugInnovation/ucm285554.htm", # 2011
  "http://wayback.archive-it.org/7993/20170111075026/http://www.fda.gov/Drugs/DevelopmentApprovalProcess/DrugInnovation/ucm336115.htm", # 2012
  "http://wayback.archive-it.org/7993/20170111075024/http://www.fda.gov/Drugs/DevelopmentApprovalProcess/DrugInnovation/ucm381263.htm", # 2013
  "http://wayback.archive-it.org/7993/20170111075020/http://www.fda.gov/Drugs/DevelopmentApprovalProcess/DrugInnovation/ucm429247.htm"  # 2014
)

base_url <- "https://www.fda.gov/drugs/new-drugs-fda-cders-new-molecular-entities-and-new-therapeutic-biological-products/novel-drug-approvals-"
years <- 2015:2019
active_urls <- lapply(years, function(year) paste0(base_url, year))

urls <- c(archive_urls, active_urls)

fda_approvals <- lapply(urls, function(url) {
  table <- read_html(url) %>%
    html_node("table") %>%
    html_table(fill = TRUE)

  # data from 2011 is missing the No. column
  if (ncol(table) == 4) {
    no <- nrow(table):1
    table <- cbind(no, table)
  }

  colnames(table) <- c("number", "drug_name", "active_ingredient",
                       "approval_date", "fda_approved_use")
  table
}) %>% do.call(rbind, .) %>% subset(drug_name != "")

fda_approvals <- within(fda_approvals, {
  fda_approved_use <- gsub("Press Release|Drug Trials Snapshot", "",
                           fda_approved_use)

  # data from 2011 is missing the year in the approval data column
  approval_date <- ifelse(stringr::str_count(approval_date, "/") == 1,
                          paste(approval_date, "2011", sep = "/"),
                          approval_date)

  approval_year <- strsplit(approval_date, "/") %>%
    vapply(function(x) x[3L], character(1L))

  approval_year <- ifelse(nchar(approval_year) == 4,
                          approval_year,
                          paste0("20", approval_year)) %>% as.numeric()

  number <- gsub("\\.", "", number) %>% as.numeric()
})
