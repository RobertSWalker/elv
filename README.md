# elv
Data and code for extralethal violence project

## Paper title
Scripts and data are for the manuscript titled 'Human extra-lethal violence across cultures' by Jonathan Paige et al. 

## Data 
Data are scraped from the electronic Human Relations Area Files (https://ehrafworldcultures.yale.edu/) at Yale  and added to the project (df.csv).

## Phase 1: web scraping
Rselenium is an `R` package for scraping websites

## Phase 1 scraping example of info tables off of eHRAF
```splus
library(RSelenium);library(XML)

remDr <- remoteDriver(
  remoteServerAdd = "localhost",
  port = 4445L,
  browser = "chrome")

remDr$open()

remDr$navigate('https://ehrafworldcultures.yale.edu/search?q=%28subjects%3A%22war%22+AND+text%3A%28torture+OR+mutilation+OR+mutilate+OR+trophy+OR+scalp+OR+headhunting+OR+%22head+hunting%22+OR+cannibal+OR+cannibalism%29%29')
remDr$getCurrentUrl()
remDr$getTitle()
remDr$maxWindowSize()
remDr$getPageSource()[[1]]

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
(df <- html %>% html_table() )
df = do.call(rbind, df)

```
## Phase 2 R package brms
We use the brms package in `R` to run a Bayesian regression to estimate the regression coefficients in our models while adjusting for spatial autocorrelation using a Gaussian process. 

## Phase 2 Bayesian regression model
```splus
library(brms);library(rstan);options(mc.cores = parallel::detectCores());rstan_options(auto_write = TRUE)

priors = c(set_prior("normal(0, .5)", class = "b"))
priors
df$Year <- df$Year/1000
m1 <- brm( 
          bf(elv ~ 1 + Year + subsistence + log(Documents) + gp(latitude, longitude, scale = T)),
          prior = priors, 
          family = bernoulli,
          data = df,
          iter =  1e4, chains = 4, cores = 4, save_all_pars = TRUE,
          control = list(adapt_delta = .999, max_treedepth = 20),
          seed = 14, sample_prior = T)
prior_summary(m1)
m1
pl <- plot(m1, N = 4, ask = FALSE)
posterior_summary(m1)
bayes_R2(m1) #.33 slow
conditional_effects(m1, effects = c("Year"), points=F) #slow
saveRDS(m1,"m1.Rds")

```
