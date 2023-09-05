#https://appsilon.com/webscraping-dynamic-websites-with-r/  #https://stackoverflow.com/questions/42468831/how-to-set-up-rselenium-for-r
library(dplyr);library(stringr);library(purrr);library(rvest)
library(RSelenium);library(XML);library(readxl);library(ggplot2);library(cowplot)

remDr <- remoteDriver(
  remoteServerAdd = "localhost",
  port = 4445L,
  browser = "chrome")

remDr$open()

#scrape info tables
remDr$navigate('https://ehrafworldcultures.yale.edu/search?q=%28subjects%3A%22war%22+AND+text%3A%28torture+OR+mutilation+OR+mutilate+OR+trophy+OR+scalp+OR+headhunting+OR+%22head+hunting%22+OR+cannibal+OR+cannibalism%29%29')
remDr$getCurrentUrl()
remDr$getTitle()
remDr$maxWindowSize()
remDr$getPageSource()[[1]]

# Find search box #webElem <- remDr$findElement(using = "class", value = "gLFyf") #google search bar
webElems <- remDr$findElements(using = "class", value = "trad-overview__result")

for (i in length(webElems):1) { 
  print(i) 
  webElems[[i]]$clickElement()}

Sys.sleep(4)
webpage <- remDr$findElement("css", "body")
webpage$sendKeysToElement(list(key = "end"))
Sys.sleep(4)
html <- remDr$getPageSource()[[1]] #tbls_xml <- readHTMLTable(html)
html <- read_html(html)
(df <- html %>% html_table() ) # %>% .[[i]] ) #%>% slice(-1)) datalist[[i]] <- df 
df = do.call(rbind, df)
names(df) <- c('region', 'culture', 'subsistence', 'sources', 'docs', 'paragraphs')
head(df)


#scrape text
library(rvest);library(purrr)
links <- paste0("https://ehrafworldcultures.yale.edu/search/traditional/data?owcs=",df$OWC,"&q=%28subjects%3A%22war%22+AND+text%3A%28torture+OR+mutilation+OR+mutilate+OR+trophy+OR+scalp+OR+headhunting+OR+%22head+hunting%22+OR+cannibal+OR+cannibalism%29%29")

texts <- rep(NA, nrow(df))

for (i in 1:nrow(df)) { 
    #tryCatch({  
    
remDr$navigate(links[[i]])

webElems <- remDr$findElements(using = "class", value = "mdc-data-table__table")

for (j in length(webElems):1) { 
  #print(j) 
  webElems[[j]]$clickElement()}
webElem <- remDr$findElement(using = "css selector", value =  '#mainContent > div > div.trad-data > main > div > div > div.results__toolbar.trad-data__action-row > div.trad-data__action-row--left > span.vert-divider > span > button')
webElem$clickElement()

Sys.sleep(2)
webpage <- remDr$findElement("css", "body")
webpage$sendKeysToElement(list(key = "end"))
html <- remDr$getPageSource()[[1]] #tbls_xml <- readHTMLTable(html)
html <- read_html(html)
(df2 <- html %>% html_table() ) # %>% .[[i]] ) #%>% slice(-1)) datalist[[i]] <- df 
df2 = do.call(rbind, df2)

print(i)
txt <- df2$Samples
txt <- lapply(txt, function(z){ z[!is.na(z) & z != "" & z != 'character(0)']})
txt <- txt[lengths(txt)!=0]
texts[[i]] <- txt # paste(txt, collapse = '')
#  }, error = function(e){})
}

remDr$close()
