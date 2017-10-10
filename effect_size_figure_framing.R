library(devtools)
library(tidyverse)
library(car)
library(effsize)
library(compute.es)
#install_github('alightner/framingdata2017')   ## uncomment and run to install
library(framingdata2017)
#install_github('alightner/dataAMT2017')  ## uncomment and run to install
library(dataAMT2017)

df <- dataAMT2017::UG.data
## Hyper-fair (>80%) offers (see Discussion) ####
mean(df$accept[df$condition=='banker' & df$offer >= 80])
ggplot(subset(df, condition=='banker' & playerid==2), aes(x=offer)) + 
  geom_histogram() + facet_wrap(~accept)
ggplot(subset(df, condition=='customer' & playerid==2), aes(x=offer)) + 
  geom_histogram() + facet_wrap(~accept)

ggplot(subset(df, (condition=='banker' | condition=='customer') & 
                playerid==2), aes(x=offer)) + 
  geom_histogram() + facet_grid(condition~accept)
ggplot(subset(df, (condition=='control' | condition=='windfall') & 
                playerid==2), aes(x=offer)) + 
  geom_histogram() + facet_grid(condition~accept)



### Consistency of offers (see Discussion) #####
df$payexp <- df$offerexp
df$fairoffer <- df$offerexp
df$fairoffer[df$playerid==1] <- 1 - df$fairoffer[df$playerid==1]
df$currencyfee <- df$currencyfee/100
df$currencyfee[df$condition=='banker'] <- 1 - df$currencyfee[df$condition=='banker']

cf1 <- df$currencyfee[df$condition=='banker'& df$playerid==1]
off1 <- df$offer2[df$condition=='banker'& df$playerid==1]
cf2 <- df$currencyfee[df$condition=='customer' & df$playerid==1]
off2 <- df$offer2[df$condition=='customer'& df$playerid==1]

sum(cf1==off1, na.rm = TRUE)/
  length(cf1)
sum(cf2==off2, na.rm = TRUE)/
  length(cf2)

diff1 <- abs(cf1-off1)
diff2 <- abs(cf2-off2)


### Effect size conversions (see Discussion, Fig. 13) ##########
## eriksson and strimling (2014)
## (hypothetical)
# teamwork (spontaneous) 
tes(3.08, 118, 75)  #d [ 95 %CI] = 0.45 [ 0.16 , 0.75 ] 
# teamwork vs. paying taxes (labeled) 
tes(2.92, 47, 43)  #d [ 95 %CI] = 0.62 [ 0.19 , 1.05 ]
#pes(0.004, 47, 43)
## (paid decisions)
# teamwork (spontaneous)
tes(2.59, 79,279) #d [ 95 %CI] = 0.33 [ 0.08 , 0.58 ] 
# teamwork vs. taxes (labeled)
tes(1.85, 84, 84)  #d [ 95 %CI] = 0.29 [ -0.02 , 0.59 ] 
## compiled team vs. no-team
tes(2.84, 363, 163) #d [ 95 %CI] = 0.27 [ 0.08 , 0.45 ]

## liberman (2004)
# community game vs. wall street game
tes(3.2, 84, 84) # d [ 95 %CI] = 0.49 [ 0.18 , 0.8 ]

## leibbrandt et al. (2015)
pes(0.009, 45, 45, tail='two') #d [ 95 %CI] = 0.56 [ 0.14 , 0.99 ] 

### Effect size graph (with CIs - Oct. 3) ######

d.vals <- c(0.33, 0.29, 0.27, 0.49, 0.56,0.46,0.47,0.6,0.35,0.57,
            0.5,0.5888977,
            0.6411336,1.09095,2.035155)
d.inf <- c(0.08, -0.02,0.08,0.18,0.14,0.06,-0.1,0.02,0.01,0.09,
           0.01,-0.02540322,
           0.04482694,0.6857637,1.571529)
d.sup <- c(0.58,0.59,0.45,0.8,0.99,0.86,1.05,1.18, 0.69,1.06,0.98,
           1.20319868,1.23744036,1.4961360,2.4987)
frames <- c('Teamwork\n(Spontaneous)','Teamwork vs. Taxes\n(Labeled)',
            'Teamwork vs. No-teamwork\n(Comprehensive)',
            'Community vs. Wall Street', 'Giving vs. Taking',
            'Osotua (Kenya, all)', 'Osotua (Kenya, player 1)',
            'Osotua (Kenya, player 2)', 'Osotua (U.S., all)', 
            'Osotua (U.S., player 1)', 'Osotua (U.S., player 2)',
            'Sovkhoz','Obschina','**Customer','**Banker')
frames <- factor(frames, levels= sort(frames, decreasing = TRUE))
Games <- c(rep('Public Goods',4),'Dictator',rep('Trust',6),rep('Public Goods',2), rep('Ultimatum',2))
d.table <- data.frame(d.vals,d.inf,d.sup,frames,Games)
d.table <- arrange(d.table, d.vals)

## Figure 13 plot ####
ggplot(d.table, aes(x=frames,y=d.vals)) +
  geom_point(aes(colour=Games)) +
  #geom_bar(aes(fill=games),position=position_dodge(0.9),stat="identity") +
  # geom_hline(yintercept = 0.2, linetype=3) +
  #geom_hline(yintercept = 0.5, linetype=3) +
  geom_hline(yintercept = 0.8, linetype=3) +
  geom_errorbar(aes(ymin=d.inf, ymax=d.sup), width=.2,
                position=position_dodge(.9)) + 
  coord_flip() + labs(x='Frames', y='Effect size (|d|)')



