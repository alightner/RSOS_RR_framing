
#   ____________________________________________________________________________
#   Load libraries and data                                                 ####

library(devtools)
library(tidyverse)
library(car)
library(effsize)
library(lawstat)
library(readr)
library(knitr)
library(reshape2)
library(ggmosaic)
library(effects)
library(gamlss.mx)
library(arm)
library(reshape2)
library(Rmixmod)
library(multcomp)
library(binomTools)
library(gridExtra)
library(broom)
library(AICcmodavg)
library(diagram)
library(visreg)
library(viridis)
# install_github('alightner/framingdata2017')
library(framingdata2017)
# install_github('alightner/dataAMT2017')
library(dataAMT2017)
#load("~/Desktop/Docs/RSOS data AMT/dataAMT2017/data/UG.data.rda")
df <- dataAMT2017::UG.data
df$offer[df$condition=='control' | df$condition=='windfall'] <- 
  df$offer[df$condition=='control' | df$condition=='windfall'] * 100

df$condition <- factor(df$condition, levels=c('banker', 'control', 
                                              'windfall', 'customer'))

#load("~/Desktop/Docs/RSOS data AMT/dataAMT2017/data/UG.preatt.rda")
#load("~/Desktop/Docs/RSOS data AMT/dataAMT2017/data/UG.presurv.rda")

dfpa <- dataAMT2017::UG.preatt
dfps <- dataAMT2017::UG.presurv
dfps$offer[dfps$condition=='control' | dfps$condition=='windfall'] <- 
  dfps$offer[dfps$condition=='control' | dfps$condition=='windfall'] * 100

dfps$condition <- factor(dfps$condition, levels=c('banker', 'control', 
                                                  'windfall', 'customer'))

## p value reporting function
# p.function <- function(p) {
#   if (p >= 0.001) {
#     paste(c("p =", signif(p,3)), collapse = " ")
#   } else {
#     c("p < 0.001")
#   }
# }

p.function <- function(p, sigfigs = 2 ) {
  p <- signif(p, sigfigs)
  if (length(p) == 1){
    return(paste("p = ", knitr:::format_sci(p, format='html')))
  } else {
    return(paste0("$", knitr:::format_sci(p), "$"))
  }
}


#   ____________________________________________________________________________
#   Tests                                                                   ####

  
  #sd(df$offer[df$condition=='control'])
  #sd(df$offer[df$condition=='windfall'])
  a <- c(df$offer[df$condition=='control' & df$playerid==1], df$offer[df$condition=='windfall' & df$playerid==1])
b <- c(rep(1, length(df$offer[df$condition=='control' & df$playerid==1])), 
       rep(2, length(df$offer[df$condition=='windfall' & df$playerid==1])))
lv <- leveneTest(a,b)
lvF <- lv$`F value`[1]
lvP <- lv$`Pr(>F)`[1]
flig <- fligner.test(a,b)

shap1 <- shapiro.test(df$offer[df$condition=='banker' & df$playerid==1])
shap2 <- shapiro.test(df$offer[df$condition=='customer' & df$playerid==1])
shap3 <- shapiro.test(df$offer[df$condition=='windfall' & df$playerid==1])
shap4 <- shapiro.test(df$offer[df$condition=='control' & df$playerid==1])

k <- kruskal.test(df$offer[ df$playerid==1] ~ df$condition[ df$playerid==1])

a1 <- wilcox.test(df$offer[df$condition=='control' & df$playerid==1], df$offer[df$condition=='banker' & df$playerid==1])
a2 <- wilcox.test(df$offer[df$condition=='control' & df$playerid==1], df$offer[df$condition=='customer' & df$playerid==1])

m <- lm(offer ~ condition, data=subset(df, playerid==1))
ao <- Anova(m)
aoF <- ao$`F value`[1]
aoP <- ao$`Pr(>F)`[1]

t1 <- t.test(df$offer[df$condition=='control'], 
             df$offer[df$condition=='banker'])
t2 <- t.test(df$offer[df$condition=='control'], 
             df$offer[df$condition=='customer'])


akg1 <- c(df$offer[df$condition=='control' & df$playerid==1],
          df$offer[df$condition=='banker' & df$playerid==1])
akg2 <- c(df$offer[df$condition=='control' & df$playerid==1],
          df$offer[df$condition=='customer' & df$playerid==1])
groups_1 <- as.factor(c(rep(1, length(df$offer[df$condition=='control' & 
                                                 df$playerid==1])), 
                        rep(2, length(df$offer[df$condition=='banker' & 
                                                 df$playerid==1]))))
groups_2 <- as.factor(c(rep(1, length(df$offer[df$condition=='control' & 
                                                 df$playerid==1])), 
                        rep(2, length(df$offer[df$condition=='customer' & 
                                                 df$playerid==1]))))
cliff.bank <- cliff.delta(akg1, groups_1)
cliff.cust <- cliff.delta(akg2, groups_2)
cohen.bank <- cohen.d(akg1, groups_1)
cohen.cust <- cohen.d(akg2, groups_2)


#   ____________________________________________________________________________
#   Attrition plot                                                          ####

phases <- c("Viewed consent form", "Played UG", "Started survey", "Completed survey\nand attention checks")
dff <- data.frame("Studyphase" = factor(phases, levels = phases), "Number" = c(1040, 770, 552, 472))

plot_attrition <-
  ggplot(dff, aes(x=Studyphase, y=Number)) + geom_bar(stat='identity') +
  geom_text(aes(x=Studyphase, label=Number), size=3.25, vjust=-1) +
  labs(x='Study phase') + coord_cartesian(ylim=c(0,1250))


#   ____________________________________________________________________________
#   Offer distributions in excluded vs included                             ####

mps <- df$userid

p1 <- ggplot(data=subset(dfps, playerid==1 & !(is.na(offer)) & !(userid %in% mps)), aes(x=offer)) + geom_histogram(binwidth = 5) +
  facet_wrap(~condition, ncol=4) + coord_cartesian(ylim=c(0,55)) + labs(x='')

p2 <- ggplot(data=subset(df, playerid==1), aes(x=offer)) + geom_histogram(binwidth = 5) +
  facet_wrap(~condition, ncol=4) + coord_cartesian(ylim=c(0,55)) + labs(x='Offer (%)')


#   ____________________________________________________________________________
#   Model of offers by condition                                            ####

dt <- df
dt$offer <- dt$offer/100
dt$condition <- factor(dt$condition, levels=c('banker', 'control', 
                                              'windfall', 'customer'))
m_offers <- glm(offer ~ condition, family=binomial, subset(dt, playerid==1))

#   ____________________________________________________________________________
#   Model of acceptances                                                    ####

ar1 <- (mean(df$accept[df$playerid==1 & df$condition=='control']))*100
ar2 <- (mean(df$accept[df$playerid==1 & df$condition=='windfall']))*100
fc1 <- (length(df$offer[df$playerid==1 & df$condition=='control' & 
                          df$offer==50])/
          length(df$offer[df$playerid==1 & df$condition=='control']))*100
fc2 <- (length(df$offer[df$playerid==1 & df$condition=='windfall' & 
                          df$offer==50])/
          length(df$offer[df$playerid==1 & df$condition=='windfall']))*100

m_accept <- glm(accept ~ condition*offer, family=binomial, subset(df, playerid==2))
rsq <- Rsq(m_accept)


#   ____________________________________________________________________________
#   Compute some acceptance values                                          ####

over90 <- (length(df$offer[df$offer >= 90 & df$condition=='banker' & df$playerid==1])/length(df$offer[df$condition=='banker' & df$playerid==1]))*100  ## 64.9%

hypoacc <- (length(df$accept[df$accept==1 & df$condition=='customer' & df$playerid==2 & df$offer < 50])/length(df$accept[df$condition=='customer' & df$playerid==2 & df$offer < 50]))*100  ## 68.6%

bankrej <- (length(df$accept[df$accept==0 & df$condition=='banker' & df$playerid==2 & df$offer > 50])/length(df$accept[df$condition=='banker' & df$playerid==2 & df$offer > 50]))*100  ## 20.4%


#   ____________________________________________________________________________
#   Acceptance plot                                                         ####

d2 <- data.frame(piHat=rsq$piHat, y=rsq$Y)
plot_accept <-
  ggplot(d2, aes(x=piHat)) + geom_histogram(binwidth = 0.025) + facet_wrap(~y, ncol=1) + coord_cartesian(xlim=c(0,1)) +
  labs(x='Probability(1)')


#   ____________________________________________________________________________
#   Acceptance mosaic plot                                                  ####

df$fairness <- ifelse(df$offer < 50, 'Hypo', 'Fair')
df$fairness[df$offer > 50] <- 'Hyper'
df$fairness <- factor(df$fairness, levels = c('Hypo', 'Fair', 'Hyper'))
#ggplot(d, aes(offer)) + geom_histogram() + facet_grid(accept ~ condition)

#plot(xtabs(~ accept + condition + fairness, data=df), main='')
plot_accept_mosaic <-
  ggplot(data=subset(df, playerid==2)) + 
  geom_mosaic(aes(x=product(accept,condition), fill=factor(fairness))) + 
  theme(axis.text.x=element_text(angle=-30, hjust= .1)) + 
  #facet_grid(accept~.) +
  facet_grid(~condition) +
  guides(fill=guide_legend(title = "Fairness", reverse = TRUE)) +
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank()) +
  labs(x="Condition", 
       y="Reject                                        Accept")


#   ____________________________________________________________________________
#   Compute values  and tests                                               ####

#load("~/Desktop/Docs/MA Thesis/framingdata2017/data/dataAMT_pilot.rda")

du <- framingdata2017::dataAMT_pilot

a <- du$offer[du$app=='control'& du$playerid==1]
b <- df$offer[df$condition=='control' & df$playerid==1]
pexpconmean <- wilcox.test(a,b)
a <- c(du$offer[du$app=='control'& du$playerid==1], df$offer[df$condition=='control' & df$playerid==1])
b <- factor(c(rep(1, length(du$offer[du$app=='control'& du$playerid==1])), 
              rep(2, length(df$offer[df$condition=='control' & df$playerid==1]))))
c <- leveneTest(a,b)
pexpconsdF <- c$`F value`[1]
pexpconsdP <- c$`Pr(>F)`[1]
###
a <- du$offer[du$app=='customer'& du$playerid==1]
b <- df$offer[df$condition=='customer' & df$playerid==1]
pexpcustmean <- wilcox.test(a,b)
a <- c(du$offer[du$app=='customer'& du$playerid==1], df$offer[df$condition=='customer' & df$playerid==1])
b <- factor(c(rep(1, length(du$offer[du$app=='customer'& du$playerid==1])), 
              rep(2, length(df$offer[df$condition=='customer' & df$playerid==1]))))
c <- leveneTest(a,b)
pexpcustsdF <- c$`F value`[1]
pexpcustsdP <- c$`Pr(>F)`[1]
###
a <- du$offer[du$app=='banker'& du$playerid==1]
b <- df$offer[df$condition=='banker' & df$playerid==1]
pexpbankmean <- wilcox.test(a,b)
a <- c(du$offer[du$app=='banker'& du$playerid==1], df$offer[df$condition=='banker' & df$playerid==1])
b <- factor(c(rep(1, length(du$offer[du$app=='banker'& du$playerid==1])), 
              rep(2, length(df$offer[df$condition=='banker' & df$playerid==1]))))
c <- leveneTest(a,b)
pexpbanksdF <- c$`F value`[1]
pexpbanksdP <- c$`Pr(>F)`[1]


#   ____________________________________________________________________________
#   Fair vs actual offer plot                                               ####
df$offer2 <- df$offer/100
df$currencyfee <- df$currencyfee/100
df$currencyfee[df$condition=='banker'] <- 1 - df$currencyfee[df$condition=='banker']

plot_fair_actual <-
  ggplot(subset(df, playerid==1 & (condition=='banker'| condition=='customer')), aes(x=currencyfee, y=offer2, colour=condition)) + 
  geom_point(alpha=0.75) + #geom_jitter(height=0.025) + 
  geom_abline(linetype='dashed') + 
  scale_colour_viridis(end =0.5, discrete = TRUE) +
  labs(x='Reported fair currency fee', y='Actual offer')


#   ____________________________________________________________________________
#   Pilot vs final offer distributions                                      ####

plot_pilot <- ggplot(data=subset(du, playerid==1), aes(x=offer)) + geom_histogram(binwidth = 5) +
  facet_wrap(~app) + labs(x='')
plot_final <- ggplot(data=subset(df, playerid==1 & condition != 'windfall'), aes(x=offer)) + geom_histogram(binwidth = 5) +
  facet_wrap(~condition) + labs(x='Offer (%)')


#   ____________________________________________________________________________
#   Effect sizes plot                                                       ####

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

plot_effect_sizes <-
  ggplot(d.table, aes(x=frames,y=d.vals)) +
  geom_point(aes(colour=Games)) +
  #geom_bar(aes(fill=games),position=position_dodge(0.9),stat="identity") +
  # geom_hline(yintercept = 0.2, linetype=3) +
  #geom_hline(yintercept = 0.5, linetype=3) +
  # geom_hline(yintercept = 0.8, linetype=3) +
  geom_errorbar(aes(ymin=d.inf, ymax=d.sup), width=.2,
                position=position_dodge(.9)) + 
  coord_flip() + labs(x='Frames', y='Effect size (|d|)')

## Note: see effect_size_figure_framing.R script in this folder to see computations


#   ____________________________________________________________________________
#   Supplementary results                                                   ####

dfps$condition <- factor(dfps$condition, levels=c('control', 'banker',
                                                  'windfall', 'customer'))
dfps$excluded <- factor(!(dfps$userid %in% df$userid))
dfps$offer2 <- dfps$offer/100
omitperc <- (1 - ((dim(df)[1])/(dim(subset(dfps, !is.na(offer2)))[1])))*100


##  ............................................................................
##  Model of pilot data                                                     ####

m_included_excluded <- glm(offer2 ~ condition*excluded, family=binomial, data=dfps)
m_included_excluded_anova <- Anova(m_included_excluded, 3)
m_included_excluded_pvalue <- (m_included_excluded_anova$`Pr(>Chisq)`)[3]

in50 <- length(df$offer[df$playerid==1 & df$offer==50 & (df$condition=='control' | df$condition=='windfall')])/length(df$offer[df$playerid==1 & (df$condition=='control' | df$condition=='windfall')])  ## 79.1%

in0 <- length(df$offer[df$playerid==1 & df$offer==0 & (df$condition=='control' | df$condition=='windfall')])/length(df$offer[df$playerid==1 & (df$condition=='control' | df$condition=='windfall')])  ## 7.0%

out50 <- length(na.omit(dfps$offer[dfps$playerid==1 & dfps$excluded==TRUE & dfps$offer==50 & (dfps$condition=='control' | dfps$condition=='windfall')]))/length(na.omit(dfps$offer[dfps$playerid==1 & dfps$excluded==TRUE & (dfps$condition=='control' | dfps$condition=='windfall')]))  ## 21.4%

out0 <- length(na.omit(dfps$offer[dfps$playerid==1 & dfps$excluded==TRUE & dfps$offer==0 & (dfps$condition=='control' | dfps$condition=='windfall')]))/length(na.omit(dfps$offer[dfps$playerid==1 & dfps$excluded==TRUE & (dfps$condition=='control' | dfps$condition=='windfall')]))  ## 60%


quit <- subset(dfps, !(userid %in% dfpa$userid) & 
                 excluded==TRUE & 
                 (condition=='control' | condition=='windfall'))
quitrej <- (1 - sum(quit$accept, na.rm=TRUE)/length(na.omit(quit$accept)))*100
quitoffer <- mean(quit$offer, na.rm=TRUE)


##  ............................................................................
##  Exploratory results                                                     ####

# nationality (after coding responses)
nUS <- (314/472)*100; nIN <- (49/472)*100  # nEUR <- (18/472)*100
#nOther <- (22/472)*100; 
nUnsp <- (69/472)*100; nOther <- (40/472)*100
# Demographics
mean.age <- mean(df$age, na.rm=TRUE)
sd.age <- sd(df$age, na.rm=TRUE)
min.age <- min(df$age, na.rm = TRUE) 
max.age <- max(df$age, na.rm = TRUE)
#table(df$gender, useNA = 'ifany')
male <- (254/468)*100
female <- (214/468)*100

### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Beliefs and expectations                                                ####

dc <- dataAMT2017::UG.data
cfmean <- (mean(dc$currencyfee))
cfsd <- (sd(dc$currencyfee))
#cfsup <- cfmean + (sd(df$currencyfee))*100
broker.mean <- (mean(dc$brokerfee))
broker.sd <- (sd(dc$brokerfee))
serv.mean <- (mean(dc$servicefee))
serv.sd <- (sd(dc$servicefee))
atm.mean <- (mean(dc$ATMfee))
atm.sd <- (sd(dc$ATMfee))

# "I thought the outcome was fair"
d.temp <- df
d.temp$fairoutcome <- as.integer(d.temp$fairoutcome)
fairout1 <- mean(d.temp$fairoutcome)
fairout2 <- sd(d.temp$fairoutcome)
# "experience with currency conversion"
d.temp$forex_exp <- as.integer(d.temp$forex_exp)
ce1 <- mean(d.temp$forex_exp)
ce2 <- sd(d.temp$forex_exp)
# "experience with international travel"
d.temp$travel_exp <- as.integer(d.temp$travel_exp)
ce3 <- mean(d.temp$travel_exp)
ce4 <- sd(d.temp$travel_exp)
# "did you recognize this as a bargaining game from previous experience?"
p.recUG <- (length(df$recognizeUG[df$recognizeUG=='Yes'])/length(df$recognizeUG))*100

# Fair outcome vs. accept/reject

fairout3p <- wilcox.test(fairoutcome ~ accept, d.temp)$p.value


##  ............................................................................
##  Summary stats                                                           ####

fairoutcome_stats <-
  d.temp %>%
  mutate(`Player type` = ifelse(playerid == 1, 'Proposer', 'Responder')) %>% 
  group_by(`Player type`, condition) %>% 
  summarise(Mean = mean(fairoutcome), SD = sd(fairoutcome)) %>% 
  mutate(condition = as.character(condition)) %>% 
  ungroup()

fairoutcome_stats2 <-
  with(d.temp, {
    data_frame(
        `Player type` = c('Proposer', 'Responder', 'All'),
        condition = c('All', 'All', 'All'),
        Mean = c(mean(fairoutcome[playerid==1]), mean(fairoutcome[playerid==2]), mean(fairoutcome)),
        SD = c(sd(fairoutcome[playerid==1]), sd(fairoutcome[playerid==2]), sd(fairoutcome))
      )
  }
  )

outdf <- rbind(fairoutcome_stats, fairoutcome_stats2)

# m1 <- mean(d.temp$fairoutcome)
# m2 <- mean(d.temp$fairoutcome[d.temp$playerid==1])
# m3 <- mean(d.temp$fairoutcome[d.temp$playerid==2])
# m4 <- mean(d.temp$fairoutcome[d.temp$condition=='control' & d.temp$playerid==1])
# m5 <- mean(d.temp$fairoutcome[d.temp$condition=='control' & d.temp$playerid==2])
# m6 <- mean(d.temp$fairoutcome[d.temp$condition=='windfall' & d.temp$playerid==1])
# m7 <- mean(d.temp$fairoutcome[d.temp$condition=='windfall' & d.temp$playerid==2])
# m8 <- mean(d.temp$fairoutcome[d.temp$condition=='customer' & d.temp$playerid==1])
# m9 <- mean(d.temp$fairoutcome[d.temp$condition=='customer' & d.temp$playerid==2])
# m10 <- mean(d.temp$fairoutcome[d.temp$condition=='banker' & d.temp$playerid==1])
# m11 <- mean(d.temp$fairoutcome[d.temp$condition=='banker' & d.temp$playerid==2])
# 
# s1 <- sd(d.temp$fairoutcome)
# s2 <- sd(d.temp$fairoutcome[d.temp$playerid==1])
# s3 <- sd(d.temp$fairoutcome[d.temp$playerid==2])
# s4 <- sd(d.temp$fairoutcome[d.temp$condition=='control' & d.temp$playerid==1])
# s5 <- sd(d.temp$fairoutcome[d.temp$condition=='control' & d.temp$playerid==2])
# s6 <- sd(d.temp$fairoutcome[d.temp$condition=='windfall' & d.temp$playerid==1])
# s7 <- sd(d.temp$fairoutcome[d.temp$condition=='windfall' & d.temp$playerid==2])
# s8 <- sd(d.temp$fairoutcome[d.temp$condition=='customer' & d.temp$playerid==1])
# s9 <- sd(d.temp$fairoutcome[d.temp$condition=='customer' & d.temp$playerid==2])
# s10 <- sd(d.temp$fairoutcome[d.temp$condition=='banker' & d.temp$playerid==1])
# s11 <- sd(d.temp$fairoutcome[d.temp$condition=='banker' & d.temp$playerid==2])
# 
# means1 <- c(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11)
# sd1 <- c(s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11)
# name1 <- c('All participants and conditions', 'All conditions - proposers',
#            'All conditions - responders', 'Control - proposers', 
#            'Control - responders', 'Windfall - proposers', 
#            'Windfall - responders', 'Customer - proposers', 
#            'Customer - responders', 'Banker - proposers', 
#            'Banker - responders')
# outdf <- data.frame(name1, means1, sd1)
#x <- tidy(m)
# colnames(outdf) <- c('Conditions and roles', 'Mean', 'Standard deviation')

