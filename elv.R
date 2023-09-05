setwd("C:/Users/walkerro/Desktop/R scripts/elv")
library(dplyr);library(stringr);library(purrr);library(rvest)
library(RSelenium);library(XML);library(readxl);library(ggplot2);library(cowplot)
df <- read.csv("df.csv")
colSums(is.na(df))

#year plot
(t <- ggplot(df[df$Year > 1400,], aes(x=Year, y=elv))  + geom_jitter(width = 0, height=.05) + 
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  scale_y_continuous(breaks = c(0, 1), 
                     labels = c("No", "Yes"), limits = c(0, 1)) +
  theme_cowplot() + ylab("Extra-lethal violence") )
#ggsave('time.pdf')

#docs plot
(d <- ggplot(df, aes(x=Documents, y=elv))  + scale_x_log10() + theme_cowplot() +
  scale_y_continuous(breaks = c(0, 1), 
                     labels = c("No", "Yes"), limits = c(0, 1)) +
  ylab("Extra-lethal violence") +
  geom_jitter(width = 0, height=.05) + geom_smooth(method = "glm", method.args = list(family = "binomial")))
#ggsave('docs.pdf')

library(ggpubr)
figure <- ggarrange(d, t,
                    #labels = c("A", "B", "C"),
                    ncol = 2, nrow = 1, align="hv")
figure
#ggsave("docstime.pdf", units = "in", height = 4, width=8)


df$subsistence <- factor(df$subsistence, levels = c('hunter-gatherers', 
                                                      'horticulturalists',
                                                      'other',
                                                      'intensive agriculturalists',
                                                      'agro-pastoralists',
                                                      'pastoralists',
                                                      'commercial economy'))
table(df$subsistence)

#percent elv by subsistence barchart
(a <- ggplot(df, aes(x=subsistence, y=elv*100))   +
  geom_bar(stat="summary", fun="mean", width = .7) +
  #geom_errorbar(stat="summary", fun.data="mean_se") + #, fun.args = list(mult = 1.96), width = .2) +
  xlab("") + ylab("Percent societies with extra-lethal violence") +
  theme_cowplot() + coord_flip() +
  theme(text = element_text(size = 8),
        axis.title.x = element_text(size = 12)) )
  #theme(aspect.ratio = 1/1))#  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
#ggsave("subsistence.pdf")

#mean elv docs by subsistence
(b <- ggplot(df, aes(x=subsistence, y=elvdocs))   +
  geom_bar(stat="summary", fun="mean", width = .7) +
  #geom_errorbar(stat="summary", fun.data="mean_se") + #, fun.args = list(mult = 1.96), width = .2) +
  xlab("") + ylab("Mean extra-lethal violence documents") +
  theme_cowplot() + coord_flip() +
  theme(text = element_text(size = 8),
        axis.title.x = element_text(size = 12)) )
  #theme(aspect.ratio = 1/1, axis.text.y=element_blank() ))#  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
#ggsave("subsistencedocs.pdf")

#mean elv paragraphs by subsistence
(c <- ggplot(df, aes(x=subsistence, y=elvparagraphs))   +
  geom_bar(stat="summary", fun="mean", width = .7) +
  #geom_errorbar(stat="summary", fun.data="mean_se") + #, fun.args = list(mult = 1.96), width = .2) +
  xlab("") + ylab("Mean extra-lethal violence paragraphs") +
  theme_cowplot() + coord_flip() +
  theme(text = element_text(size = 8),
        axis.title.x = element_text(size = 12)) )
  #theme(aspect.ratio = 1/1, axis.text.y=element_blank() ))#  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
#ggsave("subsistenceparagraphs.pdf")

library(ggpubr)
figure <- ggarrange(a, b, c ,
                    #labels = c("A", "B", "C"),
                    ncol = 1, nrow = 3, align="h")
figure
#ggsave("subsist.pdf", units = "in", height = 6, width=6)

df$elv2 <- ifelse(df$elv == 1, "Yes", "No")

#map
library(ggplot2);library(sf);library(rnaturalearth);library(dplyr)
library(rnaturalearthdata);library(ggspatial);library(cowplot);theme_set(theme_cowplot())
world <- ne_countries(scale = "small", returnclass = "sf")
class(world);library(maps)
ggplot(data = world) +
  geom_sf(fill="antiquewhite", color = 'gray') + #[!is.na(df$rivtype),] 
  #geom_sf(data = white, colour = "red") +
  #geom_sf(data = black, colour = "black") +
  #geom_sf(data = merged[merged$River_Order > 7,], colour = "lightblue") +
  geom_point(data = df, aes(x = longitude, y = latitude, fill=elv2, size = elvdocs) ,
             colour="black",pch=21, alpha=.8) +
  #scale_size(guide = "none") +
  guides(fill = guide_legend(nrow = 2), #col=guide_legend("My Continents"),
         size=guide_legend("Extra-lethal\nviolence documents")) +
  scale_fill_manual(values=c('black', 'red'), name="Extral-lethal\nviolence") +
  #scale_color_continuous( name="Population density bins") + 
  # geom_sf(data = st_as_sf(river3), colour = "blue") +
  #geom_tile(data = dfpred, mapping = aes(x = x, y = y, fill=Prediction), alpha = .7, size = 1) +
  coord_sf(ylim = c(-60, 85), expand = FALSE) +
  #annotation_scale(location = "bl", width_hint = 0.4) +
  #annotation_north_arrow(location = "bl", which_north = "true", 
  #                       pad_x = unit(.3, "in"), pad_y = unit(.3, "in"),
  #                       style = north_arrow_fancy_orienteering) +
  xlab("") + ylab("") +
  #ggtitle("Manioc map", subtitle = "(with phosphorus)") +
  theme(#legend.position = c(0.37, 0.35),
    #legend.title = NULL,
    legend.text=element_text(size=10),
    legend.title=element_text(size=10),
    axis.title=element_blank(),
    #legend.background = element_blank(),
    #legend.box.background = element_rect(colour = "black"),
    legend.position="bottom",
    axis.line = element_blank(), 
    axis.text=element_blank(),
    axis.ticks=element_blank(),
    #panel.background = element_blank(),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    #axis.line = element_line(colour = "black"),
    #panel.border = element_rect(colour = "black", fill=NA, size=1),
    #panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", size = 0.5), 
    panel.background = element_rect(fill = "aliceblue"))
#ggsave("map.pdf") #, units = "in", width = 7, height=5)

#bayesian models
library(brms);library(rstan);options(mc.cores = parallel::detectCores());rstan_options(auto_write = TRUE)
#https://rsconnect.calvin.edu:3939/Rethinking2-ggformula/adventures-in-covariance.html

priors = c(#set_prior("normal(0, 5)", class = "Intercept"),
  set_prior("normal(0, .5)", class = "b"))
priors
df$Year <- df$Year/1000
m1 <- brm( #family = poisson(link = "identity"),
  # elv | trials(1) ~... family=binomial
          bf(elv ~ 1 + Year + subsistence + log(Documents) + gp(latitude, longitude, scale = T)),
          prior = priors, 
          family = bernoulli,
          data = df,
          iter =  1e4, chains = 4, cores = 4, save_all_pars = TRUE,
          control = list(adapt_delta = .999, max_treedepth = 20),
          seed = 14, sample_prior = T)
prior_summary(m1)
m1
pl <- plot(m1, N = 4, ask = FALSE) #Trace and Density Plots for MCMC Samples
posterior_summary(m1)
bayes_R2(m1) #.33 slow
conditional_effects(m1, effects = c("Year"), points=F) #slow
saveRDS(m1,"m1.Rds")
m1 <- readRDS("m1.Rds") 
#ranef(m1)

plot(hypothesis(m1, "subsistencehorticulturalists > 0"))

m1b <- brm(#family = poisson(link = "identity"),
  bf(elv ~ 1 + Year + subsistence + 
       log(Documents) + 
       s(latitude, longitude)),
  prior = priors,
  family = bernoulli,
  data = df,
  iter =  1e4, chains = 4, cores = 4, save_all_pars = TRUE,
  control = list(adapt_delta = .999, max_treedepth = 20),
  seed = 14, sample_prior = T)
prior_summary(m1b)
m1b
pl <- plot(m1b, N = 4, ask = FALSE) #Trace and Density Plots for MCMC Samples
posterior_summary(m1b)
bayes_R2(m1b) #.24
conditional_effects(m1b, points=T)
saveRDS(m1b,"m1b.Rds")
m1b <- readRDS("m1b.Rds") 
#ranef(m1b)
pp_check(m1b)

plot(hypothesis(m1b, "subsistencehorticulturalists > 0"))

#use this one m2
priors = c(#set_prior("normal(0, 5)", class = "Intercept"),
  set_prior("normal(0, .5)", class = "b"))
m2 <- brm(
  bf(elvdocs ~ 1 + Year + subsistence + log(Documents) + gp(latitude, longitude, scale = T)),
  prior = priors, 
  family = negbinomial,
  data =df,
  iter =  1e4, chains = 4, cores = 4, save_all_pars = TRUE,
  control = list(adapt_delta = .999, max_treedepth = 20),
  seed = 14, sample_prior = T)
prior_summary(m2)
m2
pl <- plot(m2, N = 4, ask = FALSE) #Trace and Density Plots for MCMC Samples
posterior_summary(m2)
bayes_R2(m2) #.48
conditional_effects(m2, points=T)
saveRDS(m2,"m2.Rds")
m2 <- readRDS("m2.Rds") 
ranef(m2)
pp_check(m2)

plot(hypothesis(m2, "subsistencehorticulturalists > 0"))

m2b <- brm(#family = poisson(link = "identity"),
  bf(elvdocs ~ 1 + Year + subsistence + log(Documents) + s(latitude, longitude)),
  prior = priors,
  family = negbinomial,
  data = df,
  iter =  1e4, chains = 4, cores = 4, save_all_pars = TRUE,
  control = list(adapt_delta = .999, max_treedepth = 20),
  seed = 14, sample_prior = T)
prior_summary(m2b)
m2b
pl <- plot(m2b, N = 4, ask = FALSE) #Trace and Density Plots for MCMC Samples
posterior_summary(m2b)
bayes_R2(m2b) #.37
conditional_effects(m2b, points=T)
saveRDS(m2b,"m2b.Rds")
m2b <- readRDS("m2b.Rds") 
#ranef(m2b)
pp_check(m2b)

plot(hypothesis(m2b, "subsistencehorticulturalists > 0"))

priors = c(#set_prior("normal(0, 5)", class = "Intercept"),
  set_prior("normal(0, .5)", class = "b"))
m3 <- brm(#family = poisson(link = "identity"),
  bf(elvparagraphs ~ 1 + Year + subsistence + log(Documents) + gp(latitude, longitude, scale = T)),
  prior = priors, 
  family = negbinomial, # hurdle_negbinomial()
  data =df,
  iter =  1e4, chains = 4, cores = 4, save_all_pars = TRUE,
  control = list(adapt_delta = .999, max_treedepth = 20),
  seed = 14, sample_prior = T)
prior_summary(m3)
m3
pl <- plot(m3, N = 4, ask = FALSE) #Trace and Density Plots for MCMC Samples
posterior_summary(m3)
bayes_R2(m3) #.43
conditional_effects(m3, points=T)
saveRDS(m3,"m3.Rds")
m3 <- readRDS("m3.Rds") 
ranef(m3)
pp_check(m3)

plot(hypothesis(m3, "subsistencehorticulturalists > 0"))

m3b <- brm(#family = poisson(link = "identity"),
  bf(elvparagraphs ~ 1 + Year + subsistence + log(Documents) + s(latitude, longitude)),
  prior = priors,
  family = negbinomial,
  data = df,
  iter =  1e4, chains = 4, cores = 4, save_all_pars = TRUE,
  control = list(adapt_delta = .999, max_treedepth = 20),
  seed = 14, sample_prior = T)
prior_summary(m3b)
m3b
pl <- plot(m3b, N = 4, ask = FALSE) #Trace and Density Plots for MCMC Samples
posterior_summary(m3b)
bayes_R2(m3b) #.36
conditional_effects(m3b, points=T)
saveRDS(m3b,"m3b.Rds")
m3b <- readRDS("m3b.Rds") 
pp_check(m3b)
#ranef(m2b)

plot(hypothesis(m3b, "subsistencehorticulturalists > 0"))

#posterior plots https://cran.r-project.org/web/packages/bayesplot/vignettes/plotting-mcmc-draws.html
posterior <- as.array(m2) #full model
dim(posterior)
dimnames(posterior)
library(bayesplot)
plt1 <- mcmc_areas(
  posterior, 
  pars = c(#"b_spearvelocitymph_gendermale",
    #"b_atlatlvelocitymph_gendermale",
    "b_Year"  ,                              
    "b_subsistencehorticulturalists"     ,   # "b_subsistenceother"    ,                
    "b_subsistenceintensiveagriculturalists", "b_subsistenceagroMpastoralists"    ,    
    "b_subsistencepastoralists"     ,         "b_subsistencecommercialeconomy"    ,    
    "b_logDocuments"                
  ),
  prob = 0.95, # 80% intervals
  #prob_outer = 0.99, # 99%
  point_est = "mean"
)
plt1
plot1 <- plt1 +
  annotate(geom="text", x=-.4, y=2.5, label="Commerical\neconomy",color="black", size=4) +
  annotate(geom="text", x=1.1, y=1.45, label="Number of documents\n(log transformed)",color="black", size=4) +
  annotate(geom="text", x=-.4, y=3.5, label="Pastoralists",color="black", size=4) +
  annotate(geom="text", x=-.4, y=4.5, label="Agro-pastoralists",color="black", size=4) +
  annotate(geom="text", x=-.4, y=5.65, label="Intensive\nagriculturalists",color="black", size=4) +
  annotate(geom="text", x=.4, y=6.5, label="Horticulturalists",color="black", size=4) +
  annotate(geom="text", x=-.4, y=7.5, label="Year",color="black", size=4) +
  ylab("Density") + xlab("Posterior estimates") +
  #scale_y_continuous(expand = c(0,0)) +
  #expand_limits(y = 2) +
  geom_vline(xintercept=0, linetype=3) +
  #ylim(0,1)+
  scale_x_continuous(limits = c(-2,1.5), breaks = round(seq(-2, 1, by = 1),2), expand = c(0, 0)) +
  theme(
    #axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank())
#axis.title.x=element_blank(),
#axis.text.x=element_blank(), 
#axis.ticks.x=element_blank())
plot1  
#ggsave("posteriors.pdf") #, units = "in", height = 5, width=5)

