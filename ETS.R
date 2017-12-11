#Loading libraries
library("dplyr")
library('XML')
library('rvest')
library(stringr)
#Define starting URL for ETS
urlDepart <- 'http://ec.europa.eu/environment/ets/oha.do?form=oha&languageCode=en&account.registryCodes=AT&account.registryCodes=BE&account.registryCodes=BG&account.registryCodes=HR&account.registryCodes=CY&account.registryCodes=CZ&account.registryCodes=DK&account.registryCodes=EE&account.registryCodes=EU&account.registryCodes=FI&account.registryCodes=FR&account.registryCodes=DE&account.registryCodes=GR&account.registryCodes=HU&account.registryCodes=IS&account.registryCodes=IE&account.registryCodes=IT&account.registryCodes=LV&account.registryCodes=LI&account.registryCodes=LT&account.registryCodes=LU&account.registryCodes=MT&account.registryCodes=NL&account.registryCodes=NO&account.registryCodes=PL&account.registryCodes=PT&account.registryCodes=RO&account.registryCodes=SK&account.registryCodes=SI&account.registryCodes=ES&account.registryCodes=SE&account.registryCodes=GB&accountHolder=&installationIdentifier=&installationName=&permitIdentifier=&mainActivityType=-1&account.complianceStatusArray=0&account.complianceStatusArray=-&account.complianceStatusArray=A&account.complianceStatusArray=B&account.complianceStatusArray=C&account.complianceStatusArray=D&account.complianceStatusArray=E&account.complianceStatusArray=X&search=Search&searchType=oha&currentSortSettings='
#Creating function gentable that will extract the main dataframe
gentable <- function(page){
  xpathVar <- '//*[contains(concat( " ", @class, " " ), concat( " ", "bordertb", " " ))]'
  tableGen <<- page %>%
    html_nodes(xpath=xpathVar) %>%
    html_table(fill =TRUE)
  tableGen <<- as.data.frame(tableGen[2])
  tableGen <<-dplyr::filter(tableGen,!(is.na(X4))) 
  tableGen <<- tableGen[,(1:10)]
  tableGen <<- tableGen[-(1:3),]
  gc()
}
#Creating function linklist that will extract the interesting link
linklist <- function(page){
  listLinks <<- page %>%
    read_html() %>%
    html_nodes("a") %>%       # find all links
    html_attr("href") %>%     # get the url
    str_subset("accountID=") %>% # find those that contains accountID
    str_subset("action=all")  # find those that contains accountID
  listLinks <<- as.data.frame(listLinks)
}

#guessing how many pages to scrape
page <- as.character(read_html(urlDepart))
xpathVar <- '//*[contains(concat( " ", @class, " " ), concat( " ", "cellinset", " " ))]'
numPage <- page %>%
  read_html() %>%
  html_nodes(xpath=xpathVar)
numPage <- as.character(numPage[2])
numPage <- vapply(strsplit(numPage, 'value=' , fixed = TRUE), "[", "", 2)
numPage <- vapply(strsplit(numPage, 'disabled' , fixed = TRUE), "[", "", 1)
numPage <-gsub('\\"','',numPage)
numPage <-as.numeric(gsub(' ','',numPage))

#Collecting the first dataset starting with page 0
page <- read_html(urlDepart)
gentable(page)

#collecting interesting links on the first page an
page <- as.character(page)
linklist(page)
#Joining both dataframe
listLinks <- as.data.frame(cbind(tableGen,listLinks))
#Create the final main dataframe
listLinksDef <- listLinks
#Parsing next pages
a <-1 
rootURL <- 'http://ec.europa.eu/environment/ets/oha.do?form=oha&languageCode=en&account.registryCodes=AT&account.registryCodes=BE&account.registryCodes=BG&account.registryCodes=HR&account.registryCodes=CY&account.registryCodes=CZ&account.registryCodes=DK&account.registryCodes=EE&account.registryCodes=EU&account.registryCodes=FI&account.registryCodes=FR&account.registryCodes=DE&account.registryCodes=GR&account.registryCodes=HU&account.registryCodes=IS&account.registryCodes=IE&account.registryCodes=IT&account.registryCodes=LV&account.registryCodes=LI&account.registryCodes=LT&account.registryCodes=LU&account.registryCodes=MT&account.registryCodes=NL&account.registryCodes=NO&account.registryCodes=PL&account.registryCodes=PT&account.registryCodes=RO&account.registryCodes=SK&account.registryCodes=SI&account.registryCodes=ES&account.registryCodes=SE&account.registryCodes=GB&accountHolder=&installationIdentifier=&installationName=&permitIdentifier=&mainActivityType=-1&account.complianceStatusArray=0&account.complianceStatusArray=-&account.complianceStatusArray=A&account.complianceStatusArray=B&account.complianceStatusArray=C&account.complianceStatusArray=D&account.complianceStatusArray=E&account.complianceStatusArray=X&searchType=oha&currentSortSettings=&resultList.currentPageNumber='
urlNew <- paste(rootURL, a, '&nextList=Next%3E', sep="")
Sys.sleep(runif(1, 1.5, 2))
#Collecting the first dataset starting with page 1
page <- read_html(urlNew)
gentable(page)
#collecting interesting links on page 1
page <- as.character(page)
linklist(page)
listLinks <- as.data.frame(cbind(tableGen,listLinks))
listLinksDef <- rbind(listLinksDef, listLinks)
a <- a+1
#Let's loop
while(isTRUE(numPage > a)){
  urlNew <- paste(rootURL, a, '&nextList=Next%3E', sep="")
  page <- read_html(urlNew)
  Sys.sleep(runif(1, 1.5, 2))
  xpathVar <- '//*[contains(concat( " ", @class, " " ), concat( " ", "bordertb", " " ))]'
  gentable(page)
  #collecting interesting links on page a
  page <- as.character(page)
  linklist(page)
  listLinks <- as.data.frame(cbind(tableGen,listLinks))
  listLinksDef <- rbind(listLinksDef, listLinks)
  a <- a+1
}
#Naming the columns, replace relative by absolute url, clean environment and save dataset to csv
colnames(listLinksDef) <- c("National Administrator", "Account Type", "Account Holder Name", "Installation/Aircraft ID", "Installation Name/Aircraft Operator Code", "Company Registration No", "Permit/Plan ID", "Permit/Plan Date", "Main Activity Type", "Latest Compliance Code", "Detail URL")
listLinksDef[] <- lapply(listLinksDef, gsub, pattern = '../../../../', replacement = 'http://ec.europa.eu/environment/ets/', fixed = TRUE)
rm(listLinks, tableGen)
write.csv(listLinksDef, file = "listLinksdef.csv")
