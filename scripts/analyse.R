

#installed.packages('ggplot2')
library('ggplot2')
library(dplyr)
library(tidyverse)
library(anytime)
#library(WDI)
theme_set(theme_bw())


### Clinical data
setwd('./Projects/SARS-CoV-2 2023/scripts/')

cov <- read.csv2('../figures/covid-19.csv', sep=',')
cov = cov[cov$total_cases > 0,]
cov <- cov[,c('continent',"location","date","total_cases","new_cases","total_deaths","new_deaths","new_cases_per_million","total_deaths_per_million","new_deaths_per_million","total_cases_per_million","new_tests","tests_per_case","new_tests_per_thousand","total_tests","total_tests_per_thousand")]


cov$new_cases_per_million <- as.numeric(cov$new_cases_per_million) 
cov$new_deaths_per_million <- as.numeric(cov$new_deaths_per_million) 
cov$date <- anytime::anydate( cov$date )

cov <- cov[cov$continent != '',]

g <- ggplot(data=cov, aes(x=date,y=new_cases_per_million,color=continent))+
  #  geom_point(size = 0.1, color = 'black', alpha=.1)+
  geom_smooth(aes(group=location),size = .1, se = FALSE, linetype='dashed')+
  geom_smooth(aes(fill=continent),se=FALSE)+
  labs(x = 'Date',y = 'New Cases per Million Population')+
  #  facet_wrap(.~continent, ncol = 1)+
  coord_cartesian(ylim=c(0, 1700), expand = FALSE)

g
ggsave( "New Cases per Million Population.svg", dpi = 900) 



ggplot(data=cov, aes(x=date,y=new_deaths_per_million,color=continent))+
  #  geom_point(size = 0.1, color = 'black', alpha=.1)+
  geom_smooth(aes(group=location),size = .1, se = FALSE, linetype='dashed')+
  geom_smooth(aes(fill=continent),se=FALSE)+
  labs(x = 'Date',y = 'New Deaths per Million Population')+
  #  facet_wrap(.~continent, ncol = 1)
  coord_cartesian(ylim=c(0, 10), expand = FALSE)

ggsave( "New Deaths per Million Population.svg", dpi = 500) 


cov$new_tests_per_thousand <- as.numeric(cov$new_tests_per_thousand) 

ggplot(data=cov, aes(x=date,y=new_tests_per_thousand,color=continent))+
  geom_smooth(aes(group=location),size = .1, se = FALSE, linetype='dashed')+
  geom_smooth(aes(fill=continent),se=FALSE)+
  labs(x = 'Date',y = 'New Test per thousand')+
  #  facet_wrap(.~continent, ncol = 1)
  coord_cartesian(ylim=c(0, 15), expand = FALSE)

ggsave( "New Test per thousand.svg", dpi = 500) 


cov$tests_per_case <- as.numeric(cov$tests_per_case)
tcov = cov[cov$tests_per_case > 0 ,]
tcov <- tcov[! is.na(tcov$tests_per_case), ]
ggplot(data=cov, aes(x=date,y= (new_tests_per_thousand*1000) / new_cases_per_million ,color=continent))+
  geom_smooth(aes(fill=continent),se=FALSE)+
  labs(x = 'Date',y = 'Number of tests per cases') +
  coord_cartesian(ylim=c(0, NA), expand = FALSE)+
  facet_wrap(.~continent, ncol = 1, scales = 'free')

ggsave( "New Test per cases.svg", dpi = 500) 



ggplot(data=cov, aes(x=date,y= (new_tests_per_thousand*1000) / new_deaths_per_million ,color=continent))+
  geom_smooth(aes(fill=continent),se=FALSE)+
  labs(x = 'Date',y = 'Number of tests per deaths') +
  #  facet_wrap(.~continent, ncol = 1, scales = 'free')+
  theme_bw()+
  coord_cartesian(ylim=c(0, NA), expand = FALSE)

ggsave( "New Test per deatgs.svg", dpi = 500) 


ggplot(data=cov, aes(x=date,y= new_deaths_per_million / new_cases_per_million ,color=continent))+
  geom_smooth(aes(fill=continent),se=FALSE)+
  labs(x = 'Date',y = 'Number of deaths per cases') +
  #facet_wrap(.~continent, ncol = 1, scales = 'free')+
  theme_bw()+
  coord_cartesian(ylim=c(0, NA), expand = FALSE)

ggsave( "New  deatgs per cases.svg", dpi = 500) 



### quarters
library(zoo)
library(dplyr)
library(reshape2)
library("pheatmap")

cov$YM <- zoo::as.yearmon(cov$date)
Cov <- cov[! is.na(cov$new_cases_per_million) , ]
cases_pm <- Cov %>%
  group_by(YM, continent)%>% 
  summarise(Mean=mean(new_cases_per_million), Median=median(new_cases_per_million))
cases_med <- dcast(cases_pm,  continent ~ YM)
rownames(cases_med) <- cases_med[,1]
cases_med <- cases_med[,-1]
pheatmap(as.matrix(cases_med),cluster_cols=FALSE,cluster_rows=FALSE, color=colorRampPalette(c("skyblue", "lightyellow","pink"))(50))
ggsave( "Heatmap cases.svg", dpi = 500) 





cov$YM <- zoo::as.yearmon(cov$date)
Cov <- cov[! is.na(cov$new_deaths_per_million) , ]
cases_pm <- Cov %>%
  group_by(YM, continent)%>% 
  summarise(Mean=mean(new_cases_per_million), Median=median(new_deaths_per_million))
cases_med <- dcast(cases_pm,  continent ~ YM)
rownames(cases_med) <- cases_med[,1]
cases_med <- cases_med[,-1]
pheatmap(as.matrix(cases_med),cluster_cols=FALSE,cluster_rows=FALSE, color=colorRampPalette(c("skyblue", "lightyellow","pink"))(50))
ggsave( "Heatmap cases per mil.svg", dpi = 500) 




cov$YM <- zoo::as.yearmon(cov$date)
cov$ratio <- as.numeric(cov$new_deaths) / (as.numeric(cov$new_cases ) *100)
Cov <- cov[! is.na(cov$ratio) , ]
cases_pm <- Cov %>%
  group_by(YM, continent)%>% 
  summarise(Mean=mean(new_cases_per_million), Median=median(ratio))
cases_med <- dcast(cases_pm,  continent ~ YM)
rownames(cases_med) <- cases_med[,1]
cases_med <- cases_med[,-1]
pheatmap(as.matrix(cases_med),cluster_cols=FALSE,cluster_rows=FALSE, color=colorRampPalette(c("skyblue", "lightyellow","pink"))(50), title = 'Death per  100 cases')
ggsave( "Heatmap deaths per cases.svg", dpi = 500) 


cov$YM <- zoo::as.yearmon(cov$date)
cov$test_per_case <- as.numeric(cov$new_tests) / (as.numeric(cov$new_cases ) )
Cov <- cov[! is.na(cov$test_per_case) , ]
cases_pm <- Cov %>%
  group_by(YM, continent)%>% 
  summarise(Mean=mean(new_cases_per_million), Median=median(test_per_case))
cases_pm[is.infinite(cases_pm$Median),'Median'] <- NA
cases_med <- dcast(cases_pm,  continent ~ YM)
rownames(cases_med) <- cases_med[,1]
cases_med <- cases_med[,-1]
pheatmap(as.matrix(cases_med),cluster_cols=FALSE,cluster_rows=FALSE, color=colorRampPalette(c("skyblue", "lightyellow","pink"))(50), title = 'Death per  100 cases')



cov$YM <- zoo::as.yearmon(cov$date)
cov$test_per_deaths <- as.numeric(cov$new_tests) / (as.numeric(cov$new_deaths ) )
Cov <- cov[! isna(cov$test_per_deaths) , ]
cases_pm <- Cov %>%
  cases_pm[is.infinite(cases_pm$Median),'Median'] <- NA
cases_med <- dcast(cases_pm,  continent ~ YM)
rownames(cases_med) <- cases_med[,1]
cases_med <- cases_med[,-1]
pheatmap(as.matrix(cases_med),cluster_cols=FALSE,cluster_rows=FALSE, color=colorRampPalette(c("skyblue", "lightyellow","pink"))(50), title = 'Death per  100 cases')




### sequencing
library(reshape2)
subs <- read.csv2('../data/monthly_sub.csv', sep=',',colClasses="character",na.strings="?", header = TRUE)

subm <- melt(subs, id='Date')

colnames(subm)[1] = 'location'

md <- read.csv2('../data/Metadata.tsv', sep = '\t', colClasses="character",na.strings="?", header = TRUE)
reg <- md[c('country','region')]
colnames(reg) <- c('location','continent')

Subm <- merge(subm, reg)

Subm <- Subm[Subm$value>0,]
Subm$value <- as.numeric(Subm$value)
submitions_cont <- Subm %>% group_by(continent,variable) %>% 
  summarise(Sequences=sum(value))

submitions_cont_matrix <- dcast(submitions_cont, continent~variable, fill = 0)
rownames(submitions_cont_matrix) <- submitions_cont_matrix[,1]
submitions_cont_matrix <- submitions_cont_matrix[,-1]
library(pheatmap)
pheatmap(as.matrix(submitions_cont_matrix),cluster_cols=FALSE,cluster_rows=FALSE, color=colorRampPalette(c("pink1", "lightyellow","skyblue1"))(50))
ggsave( "submitions_cont_matrix  .svg", dpi = 900) 


### MAP PLOT
{R}
library(rworldmap)
subs <- read.csv2('../data/monthly_sub.csv', sep=',', header = TRUE)
rownames(subs) <- subs[,1]
subs <- subs[,-1]
submition_country <- as.data.frame(rowSums(subs))
colnames(submition_country) <- c('submitions')
submition_country$country <- rownames(submition_country)

joinData <- joinCountryData2Map( submition_country,
                                 joinCode = "NAME",
                                 nameJoinColumn = "country")

theMap <- mapCountryData( joinData, nameColumnToPlot="submitions", addLegend=FALSE )
do.call( addMapLegend, c(theMap, legendWidth=1, legendMar = 2))

ggsave( "mapCountryData submition_country  .svg", dpi = 900) 





SUbS <- read.csv2('../data/subs.csv', sep=',', header = FALSE)
colnames(SUbS)[1] = 'id'

library(reshape2)
SUbSS <- melt(SUbS, 'id')[c('id','value')]
colnames(SUbSS) <- c('gisaid_epi_isl','Mutation')
length(unique(SUbSS$value))




samples <- read.csv2('../data/all_samples.tsv', sep='\t', header = TRUE)
aa <- read.csv2('../data/aa_mutations.csv', sep=',', header = FALSE)
colnames(aa)[1] = 'id'

library(reshape2)
aaM <- melt(aa, 'id')[c('id','value')]
colnames(aaM) <- c('gisaid_epi_isl','Mutation')
length(unique(aaM$Mutation))

#aaM$gisaid_epi_isl <- aaM[,1]
aaM <- unique(aaM)
aaMM <- separate(data = aaM, col = Mutation, into = c("gene", "change"), sep = ":")
table(aaMM$gene)

AA <- merge(aaM,samples,by = 'gisaid_epi_isl')


AA$date <- anytime::anydate(AA$date)
AA$YM <- zoo::as.yearmon(AA$date)


write_csv(AA,'AA.csv')

#AA <- read_csv('AA.csv')
mutation_region <- AA[c('Mutation','continent','date','YM')]
mutation_region <- mutation_region[mutation_region$Mutation!='',]

mutation_region_count <- as.data.frame(table(mutation_region[c('YM','continent')]))

n_sample_by_region <- unique(AA[c('gisaid_epi_isl','YM','continent')])
n_sample_by_region <- as.data.frame(table(n_sample_by_region[c('YM','continent')]))
colnames(n_sample_by_region) <- c('YM','continent','Nsamples')

mutation_region_counts <- merge(n_sample_by_region, mutation_region_count)
mutation_region_counts$Nmutation <- mutation_region_counts$Freq / mutation_region_counts$Nsamples

ggplot(mutation_region_counts, aes(x = as.numeric(YM), y = Nmutation, color = continent))+
  geom_point()+
  labs(x='Months')+
  geom_smooth(se=FALSE, size=.5, span=.9)

ggsave( "mutation_region_counts  .svg", dpi = 900) 

library(reshape2)
mutation_region_MATRIX <- dcast(mutation_region_counts[c('YM','continent','Nmutation')],  continent ~ YM, fill = 0)
rownames(mutation_region_MATRIX) <- mutation_region_MATRIX[,1]
mutation_region_MATRIX <- mutation_region_MATRIX[,-1]
library(pheatmap)
pheatmap(as.matrix(mutation_region_MATRIX),cluster_cols=FALSE,cluster_rows=FALSE, color=colorRampPalette(c("skyblue", "lightyellow","pink"))(50))
ggsave( "mutation_region_MATRIX  .svg", dpi = 900) 



# Mutation country

#AA <- read_csv('AA.csv')
mutation_region <- AA[c('Mutation','continent', 'country','date','YM')]
mutation_region <- mutation_region[mutation_region$Mutation!='',]

mutation_region_count <- as.data.frame(table(mutation_region[c('YM','country')]))

n_sample_by_region <- unique(AA[c('gisaid_epi_isl','YM','country')])
n_sample_by_region <- as.data.frame(table(n_sample_by_region[c('YM','country')]))
colnames(n_sample_by_region) <- c('YM','country','Nsamples')

mutation_region_counts <- merge(n_sample_by_region, mutation_region_count)
mutation_region_counts$Nmutation <- mutation_region_counts$Freq / mutation_region_counts$Nsamples

REGIONS <- unique(samples[,c('country','continent')])

mutation_region_counts <- merge(mutation_region_counts, REGIONS)

ggplot(mutation_region_counts, aes(x = as.numeric(YM), y = Nmutation, color = continent))+
  # geom_point(size=.5)+
  geom_boxplot(aes(x =YM ) )+
  #  geom_smooth(se=FALSE, size=.5, span=.9)+
  facet_wrap(.~continent, ncol=1)
ggsave( "mutation_region_counts  .svg", dpi = 900) 




### SELECT FIRST APP


app_mut <- mutation_region %>%
  group_by(Mutation) %>%
  arrange(date) %>%
  filter(row_number()==1 )

app_mut_reg <- as.data.frame(table(app_mut[c('continent','YM')]))
samples$date <- anytime::anydate(samples$date)
samples$YM <- zoo::as.yearmon(samples$date)
sample_per_cont <- as.data.frame(table(samples[c('continent','YM')]))

colnames(app_mut_reg) <- c('continent','YM','Nsample')
colnames(sample_per_cont) <- c('continent','YM','SamplesCont')
app_mut2 <- merge(sample_per_cont , app_mut_reg)

app_mut2$Frequency <- app_mut2$Nsample / app_mut2$SamplesCont

mutation_app_reg <- dcast(app_mut2[c('YM','continent','Frequency')],  continent ~ YM, fill = 0)
rownames(mutation_app_reg) <- mutation_app_reg[,1]
mutation_app_reg <- mutation_app_reg[,-1]
pheatmap(as.matrix(mutation_app_reg),cluster_cols=FALSE,cluster_rows=FALSE, color=colorRampPalette(c("skyblue", "lightyellow","pink"))(50))
ggsave( "mutation_app_reg  .svg", dpi = 900) 


############# BY COUNTRY


samples <- read.csv2('../data/all_samples.tsv', sep='\t', header = TRUE)
aa <- read.csv2('../data/aa_mutations.csv', sep=',', header = FALSE)
colnames(aa)[1] = 'id'




library(reshape2)
aaM <- melt(aa, 'id')[c('id','value')]
colnames(aaM) <- c('gisaid_epi_isl','Mutation')

AA <- merge(aaM,samples,by = 'gisaid_epi_isl')


mutation_region <- AA[c('Mutation','country','date')]
mutation_region <- mutation_region[mutation_region$Mutation!='',]
mutation_region$date <- anytime::anydate(mutation_region$date)
mutation_region$YM <- zoo::as.yearmon(mutation_region$date)
mutation_region_count <- as.data.frame(table(mutation_region[c('YM','country')]))

AA$YM <- zoo::as.yearmon(anytime::anydate(AA$date))
n_sample_by_region <- unique(AA[c('gisaid_epi_isl','YM','country')])
n_sample_by_region <- as.data.frame(table(n_sample_by_region[c('YM','country')]))
colnames(n_sample_by_region) <- c('YM','country','Nsamples')

mutation_region_counts <- merge(n_sample_by_region, mutation_region_count)
mutation_region_counts$Nmutation <- mutation_region_counts$Freq / mutation_region_counts$Nsamples
mutation_region_counts <- mutation_region_counts[mutation_region_counts$Nsamples>0,]


app_mut <- mutation_region %>%
  group_by(Mutation) %>%
  arrange(date) %>%
  filter(row_number()==1 )

app_mut_reg <- as.data.frame(table(app_mut$country))
colnames(app_mut_reg) <- c('country','Nmutation')

samples_country <- as.data.frame(table(samples$country))
colnames(samples_country) <- c('country','Samples')

app_mut_reG <- merge(app_mut_reg, samples_country)
app_mut_reG$freq <- app_mut_reG$Nmutation / app_mut_reG$Samples

library(rworldmap)
joinData <- rworldmap::joinCountryData2Map( app_mut_reG,
                                            joinCode = "NAME",
                                            nameJoinColumn = "country")

colourPalette <- rev(RColorBrewer::brewer.pal(11,'Spectral'))
mapCountryData( joinData, nameColumnToPlot="freq", addLegend=TRUE , colourPalette=colourPalette)
ggsave( "map app_mut_reG  .svg", dpi = 900) 

### LINEAGE FIRST APP
 

#AA <- read_csv('AA.csv')
samples <- unique(samples)
samples$YM <- zoo::as.yearmon(anytime::anydate(samples$date))
lineage_region <- samples[,c('pango_lineage','continent','date','YM')]
lineage_region <- lineage_region[lineage_region$pango_lineage!='',]
lineage_region <- unique(lineage_region)
lineage_region_count <- as.data.frame(table(lineage_region[c('YM','continent')]))

n_sample_by_region <- unique(samples[c('gisaid_epi_isl','YM','continent')])
n_sample_by_region <- as.data.frame(table(n_sample_by_region[c('YM','continent')]))
colnames(n_sample_by_region) <- c('YM','continent','Nsamples')

lineage_region_counts <- merge(n_sample_by_region, lineage_region_count)
lineage_region_counts$Nlineages <- lineage_region_counts$Freq / lineage_region_counts$Nsamples


app_lin <- lineage_region %>%
  group_by(pango_lineage) %>%
  arrange(date) %>%
  filter(row_number()==1 )

app_lin_reg <- as.data.frame(table(app_lin[c('continent','YM')]))
samples$date <- anytime::anydate(samples$date)
samples$YM <- zoo::as.yearmon(samples$date)
sample_per_cont <- as.data.frame(table(samples[c('continent','YM')]))

colnames(app_lin_reg) <- c('continent','YM','Nsample')
colnames(sample_per_cont) <- c('continent','YM','SamplesCont')
app_lin2 <- merge(sample_per_cont , app_lin_reg)

app_lin2$Frequency <- app_lin2$Nsample / app_lin2$SamplesCont

app_lin_reg <- dcast(app_lin2[c('YM','continent','Frequency')],  continent ~ YM, fill = 0)
rownames(app_lin_reg) <- app_lin_reg[,1]
app_lin_reg <- app_lin_reg[,-1]
pheatmap(as.matrix(app_lin_reg),cluster_cols=FALSE,cluster_rows=FALSE, color=colorRampPalette(c("skyblue", "lightyellow","pink"))(50))

app_lin2$Frequency
ggplot(app_lin2, aes(as.numeric(YM) , Frequency, color = continent))+
  geom_point()+
  labs(x='Months')+
  geom_smooth(se=FALSE, span=.9)
ggsave( "map app_lin2  .svg", dpi = 900) 


############# BY COUNTRY


pango_lineage_region <- samples[c('pango_lineage','country','date')]
pango_lineage_region <- pango_lineage_region[pango_lineage_region$pango_lineage!='',]
pango_lineage_region$date <- anytime::anydate(pango_lineage_region$date)
pango_lineage_region$YM <- zoo::as.yearmon(pango_lineage_region$date)
pango_lineage_region_count <- as.data.frame(table(pango_lineage_region[c('YM','country')]))

samples$YM <- zoo::as.yearmon(anytime::anydate(samples$date))
n_sample_by_region <- unique(samples[c('gisaid_epi_isl','YM','country')])
n_sample_by_region <- as.data.frame(table(n_sample_by_region[c('YM','country')]))
colnames(n_sample_by_region) <- c('YM','country','Nsamples')

pango_lineage_region_counts <- merge(n_sample_by_region, pango_lineage_region_count)
pango_lineage_region_counts$Npango_lineage <- pango_lineage_region_counts$Freq / pango_lineage_region_counts$Nsamples
pango_lineage_region_counts <- pango_lineage_region_counts[pango_lineage_region_counts$Nsamples>0,]


app_mut <- pango_lineage_region %>%
  group_by(pango_lineage) %>%
  arrange(date) %>%
  filter(row_number()==1 )

app_mut_reg <- as.data.frame(table(app_mut$country))
colnames(app_mut_reg) <- c('country','Npango_lineage')

samples_country <- as.data.frame(table(samples$country))
colnames(samples_country) <- c('country','Samples')

app_mut_reG <- merge(app_mut_reg, samples_country)
app_mut_reG$freq <- app_mut_reG$Npango_lineage / app_mut_reG$Samples

library(rworldmap)
joinData <- rworldmap::joinCountryData2Map( app_mut_reG,
                                            joinCode = "NAME",
                                            nameJoinColumn = "country")

colourPalette <- rev(RColorBrewer::brewer.pal(11,'Spectral'))
mapCountryData( joinData, nameColumnToPlot="freq", addLegend=TRUE , colourPalette=colourPalette)

ggsave( "maps app_mut_reG  .svg", dpi = 900) 

app_mut_regg <- merge(app_mut_reG, REGIONS)
ggplot(app_mut_regg, aes(x=continent, fill=continent, y=freq))+
  geom_boxplot()
ggsave( "app_mut_regg  .svg", dpi = 900) 




lineage_per_country <- app_mut_regg[,c('country','freq','continent')]

gdp <- read.csv('../data/gdp.csv',sep=',')
gdp <- gdp[c('Country.Name','X2021')]
colnames(gdp) <- c('country','gdp')
lineages_gdp <- merge(lineage_per_country, gdp)
ggplot(lineages_gdp, aes(x = gdp, y = freq, color = continent))+
  geom_point()+
  facet_wrap(.~continent, ncol = 1, scales = 'free')+
  coord_cartesian(ylim=c(0, NA), xlim=c(0, NA), expand = FALSE)+
  geom_smooth(method = 'glm', se= FALSE, fullrange= TRUE)

ggsave( "gdo  .svg", dpi = 900) 



pop <- read.csv('../data/pop.csv',sep=',')
pop <- pop[c('Country.Name','X2021')]
colnames(pop) <- c('country','pop')
lineages_pop <- merge(lineage_per_country, pop)
ggplot(lineages_pop, aes(x = pop, y = freq, color = continent))+
  geom_point()+
  facet_wrap(.~continent, ncol = 1, scales = 'free_x')+
  coord_cartesian(ylim=c(0, NA), expand = FALSE)+
  geom_smooth(method = 'glm', se= FALSE, fullrange= TRUE)
ggsave( "lineages_pop  .svg", dpi = 900) 



aa_time <- AA[,c('gisaid_epi_isl','Mutation','date')]

aa_time['Date'] <- anytime::anydate (aa_time[,'date'])
aa_time$YM <- zoo::as.yearmon(aa_time[,'Date'])
aa_time <- unique(aa_time)
aa_time <- aa_time[aa_time$Mutation!='',]
mutations_time <- as.data.frame(table(aa_time$date))
mutations_time$date <- anytime::anydate(mutations_time$Var1)
str(mutations_time)
ggplot(mutations_time, aes(x=date,y=Freq))+
  geom_point()+
  geom_smooth(method='lm')
ggsave( "mutations_time   .svg", dpi = 900) 





app_mut <- mutation_region %>%
  group_by(Mutation) %>%
  arrange(date) %>%
  filter(row_number()==1 )




aa_time <- AA[,c('gisaid_epi_isl','Mutation','date')]
aa_time <- unique(aa_time)
aa_time <- aa_time[aa_time$Mutation!='',]
aa_time2 <- aa_time[c('gisaid_epi_isl','date')]

aa_time3 <- as.data.frame(table(aa_time2))
aa_time3 <- aa_time3[aa_time3$Freq>0,]
aa_time3$date <- anytime::anydate(aa_time3$date)

ggplot(aa_time3, aes(x=date, Freq))+
  geom_point(size = .1)+
  geom_smooth(size=2, color='white')+
  geom_smooth(size=1)+
  labs(x='Date',y='Number of mutations')
ggsave( "aa_time3   .svg", dpi = 900) 



summary(aa_time3)

############# SELECT FIRST APPEARENCE OF MUTATIONS ###############

aa_time <- AA[,c('Mutation','date')]
app_mut <- aa_time %>%
  group_by(Mutation) %>%
  arrange(date) %>%
  filter(row_number()==1 )
app_mut2 <- as.data.frame(table(app_mut$date))
app_mut2$date <- anytime::anydate(app_mut2$Var1)
ggplot(app_mut2, aes(date,Freq))+
  geom_point()+
  geom_smooth()

ggsave( "app_mut2   .svg", dpi = 900) 

aa_time <- AA[,c('Mutation','date')]
aa_time$YM <- zoo::as.yearmon(anytime::anydate(aa_time$date))
aa_time$Date <- zoo::as.Date(aa_time$YM)
app_mut <- aa_time %>%
  group_by(Mutation) %>%
  arrange(Date) %>%
  filter(row_number()==1 )

app_mut2 <- as.data.frame(table(app_mut$Date))
app_mut2$Date <- as.character(app_mut2$Var1)
ggplot(app_mut2, aes(as.Date(Date),Freq))+
  geom_point()+
  geom_smooth()
ggsave( "app_mut2.svg", dpi = 900) 


ggplot(app_mut2, aes(as.Date(Date),Freq, fill=Freq))+
  geom_col()+
  labs(x='Date (months)', y='First appearence of mutations')

ggsave( "bar app_mut2.svg", dpi = 900) 


##########

Deletions <- read.csv('../data/dels.csv',header=FALSE)
colnames(Deletions)[1] = 'id'
dels <- melt(Deletions, 'id')[c('id','value')]
length(unique(dels$value))
samples$id <- samples$gisaid_epi_isl
Smpl <- samples[c('id','country','date')]
DEls <- merge(dels, Smpl)

DEls_time <- DEls[,c('id','value','date')]
DEls_time <- DEls_time[DEls_time$value!='',]
DEls_time <- unique(DEls_time)
DEls_time2 <- DEls_time[c('id','date')]
DEls_time3 <- as.data.frame(table(DEls_time2))
DEls_time3 <- DEls_time3[DEls_time3$Freq>0,]
DEls_time3$date <- anytime::anydate(DEls_time3$date)

ggplot(DEls_time3, aes(x=date, Freq))+
  geom_point(size = .1)+
  geom_smooth(size=1, method='loess')+
  labs(x='Date',y='Number of deletions')
ggsave( "DEls_time3 2.svg", dpi = 900) 

summary(aa_time3)







### Heatmap mutations
{R}
Muts <- AA[c('Mutation','continent')]
N_region <- unique(AA[,c('gisaid_epi_isl','continent')])
N_region <- as.data.frame(table(N_region$continent))
colnames(N_region) <- c('continent', 'samples')
Muts <- as.data.frame(table(Muts))
colnames(Muts) <- c('Muts', 'continent','Freq')
Muts <- Muts[Muts$Muts!= Muts[1,'Muts'],]
Mutations <- merge(Muts, N_region)
Mutations$Prevalence <- Mutations$Freq / Mutations$samples

PREV <- Mutations[Mutations$Prevalence>0.25,]$Muts
Mutations <- Mutations[Mutations$Muts %in% PREV , ]
MutationsCast <- dcast(Mutations[c('Muts','continent','Prevalence')],  continent ~ Muts, fill = 0)
rownames(MutationsCast) <- MutationsCast[,1]
MutationsCast <- MutationsCast[,-1]
pheatmap(as.matrix(MutationsCast),cluster_cols=FALSE,cluster_rows=TRUE, color=colorRampPalette(c("skyblue", "lightyellow","pink"))(50))

ggsave( "pheatmap MutationsCast .svg", dpi = 900) 



### mutations stats
length(unique(AA$Mutation))

# DELS
Deletions <- read.csv('../data/dels.csv',header=FALSE)
colnames(Deletions)[1] = 'id'
dels <- melt(Deletions, 'id')[c('id','value')]
length(unique(dels$value))

# samples
length(unique(samples$pango_lineage))
length(unique(samples$Nextstrain_clade))




