library(tidyverse)
library(haven)
library(ggplot2)
library(ggthemes)
library(RColorBrewer)
library(varhandle)

data <- read_dta("DTA/weakest_beta.dta")


data41 <- data[,2]
data61 <- data[,3]
data41$game <- "RI"
data61$game <- "WL"
colnames(data41) <- c("beta","game")
colnames(data61) <- c("beta","game")
data2 <- rbind(data41,data61)

ggplot(data2,aes(x=beta,group=game,fill=game)) +
  geom_density(alpha=.3) +
  scale_fill_tableau() +
  theme_minimal()


ggplot(data,aes(x=b_con1_inv61)) +
  geom_density()

# --- beta distribution chart
## -- con1 inv
### - data prep
data$rBeta61 <- round(data$b_con1_inv61/0.05)*0.05
data$rBeta41 <- round(data$b_con1_inv41/0.05)*0.05

data$weight <- 1

data <- data %>% group_by(rBeta41,rBeta61) %>% summarise(weight = sum(weight))
head(data)

  #posData <- subset(data, rBeta61 >= 0 & rBeta41 >= 0)
posData <- data
posData$dev <- posData$rBeta41 - posData$rBeta61
posData$distance <- abs(posData$rBeta41 - posData$rBeta61)
posData$col <- "unchanged"
posData$col[posData$dev > 0] <- "increased"
posData$col[posData$dev < 0] <- "reduced"

head(posData)
posData[order(posData[,7])]

constFR <- subset(posData,rBeta61==0&rBeta41==0)

    #library("scales")
    #posData$rescaleoffset <- posData$rescale + 100*(as.numeric(posData$col)-1)

    #scalerange <- range(posData$rescale)
    #gradientends <- scalerange + rep(c(0,100,200), each=2)
colorends <- c(low="white", high="blue",low="black",high="black", low="white", high="red")

### - plot   
ggplot() + 
    geom_line(data = data.frame(x = seq(-0.2,1,by=.1), y = seq(-.2,1,by=.1)),aes(x=x,y=y),colour="#191970",alpha=.4,linetype=2)+
    geom_hline(yintercept=0,alpha=.3) +
    geom_vline(xintercept=0,alpha=.3) +
    geom_point(data=subset(posData,dev>0),aes(x=rBeta61,y=rBeta41,size=weight,alpha=(.25 + distance)), colour="#4477aa") +
    geom_point(data=subset(posData,dev==0), aes(x=rBeta61, y=rBeta41,size=weight), alpha=.3,shape=21 , colour="#000000") +
    geom_point(data=subset(posData,dev<0), aes(x=rBeta61, y=rBeta41, size=weight, alpha=(.25 + distance)), colour="#cc6677") +
    scale_size(range=c(10,70)) +
    scale_y_continuous(breaks =seq(-.2,.6,by=0.1), limits = c(-.2, .6),name="Slope Coefficient in Repeated Public Goods Game (RI)")+
    scale_x_continuous(breaks =seq(-.2,.6,by=0.1), limits = c(-.2, .6),name="Slope Coefficient in Weakest Link Game (WL)")+
    theme_minimal() +
    theme(legend.position="none") 
    
pdf("img/con1_bubbles.pdf")
con1bubbles
dev.off()


### - plot   *DISS*
con1bubbles <- ggplot() + 
    geom_line(data = data.frame(x = seq(-0.2,1,by=.1), y = seq(-.2,1,by=.1)),aes(x=x,y=y),colour="#191970",alpha=.4,linetype=2)+
    geom_hline(yintercept=0,alpha=.3) +
    geom_vline(xintercept=0,alpha=.3) +
    geom_point(data=subset(posData,dev>0),aes(x=rBeta61,y=rBeta41,size=weight,alpha=(.45 + distance)), colour="#4477aa") +
    geom_point(data=subset(posData,dev==0), aes(x=rBeta61, y=rBeta41,size=weight), alpha=.3,shape=21 , colour="#000000") +
    geom_point(data=subset(posData,dev<0), aes(x=rBeta61, y=rBeta41, size=weight, alpha=(.45 + distance)), colour="#cc6677") +
    scale_size(range=c(10,70)) +
    scale_y_continuous(breaks =seq(-.2,.6,by=0.1), limits = c(-.2, .6),name="Slope Coefficient in RI")+
    scale_x_continuous(breaks =seq(-.2,.6,by=0.1), limits = c(-.2, .6),name="Slope Coefficient in OS")+
    theme_minimal() +
    theme(legend.position="none") 
    
pdf("img/con1_bubbles_diss.pdf")
con1bubbles
dev.off()


## -- sdiff_az (only)
### - data prep
tmp <- data.frame(raw$b_sdiff_az61,raw$b_sdiff_az41,raw$sid)
tmpDat <- group_by(tmp,raw.sid)
sdiffRaw <- data.frame(summarise_each(tmpDat,funs(mean)))

sdiffRaw$rBeta61 <- round(sdiffRaw$raw.b_sdiff_az61/0.05)*0.05
sdiffRaw$rBeta41 <- round(sdiffRaw$raw.b_sdiff_az41/0.05)*0.05
sdiffRaw$weight <- 1
head(sdiffRaw)

data <- summaryBy(weight ~ rBeta61 + rBeta41,FUN=sum,data=sdiffRaw,keep.names=TRUE)

head(data)

posData <- subset(data, rBeta61 >= 0 & rBeta41 >= 0)
posData$dev <- posData$rBeta41 - posData$rBeta61
posData$distance <- abs(posData$rBeta41 - posData$rBeta61)
posData$col <- "unchanged"
posData$col[posData$dev > 0] <- "increased"
posData$col[posData$dev < 0] <- "reduced"

head(posData)
posData[order(posData[,7])]

constFR <- subset(posData,rBeta61==0&rBeta41==0)

    #library("scales")
    #posData$rescaleoffset <- posData$rescale + 100*(as.numeric(posData$col)-1)

    #scalerange <- range(posData$rescale)
    #gradientends <- scalerange + rep(c(0,100,200), each=2)
colorends <- c(low="white", high="blue",low="black",high="black", low="white", high="red")

### - plot   
sdiffBubbles <- ggplot() + 
    geom_line(data = data.frame(x = seq(-0.2,1,by=.1), y = seq(-.2,1,by=.1)),aes(x=x,y=y),colour="#191970",alpha=.4,linetype=2)+
    geom_hline(yintercept=0,alpha=.3) +
    geom_vline(xintercept=0,alpha=.3) +
    geom_point(data=subset(posData,dev>0),aes(x=rBeta61,y=rBeta41,size=weight,alpha=(.45 + distance)), colour="#4477aa") +
    geom_point(data=subset(posData,dev==0), aes(x=rBeta61, y=rBeta41,size=weight), alpha=.3,shape=21 , colour="#000000") +
    geom_point(data=subset(posData,dev<0), aes(x=rBeta61, y=rBeta41, size=weight, alpha=(.45 + distance)), colour="#cc6677") +
    scale_size(range=c(10,70)) +
    scale_y_continuous(breaks =seq(-.2,1,by=0.1), limits = c(-.2, 1),name="Slope Coefficient in Repeated Interaction Setting (RI)")+
    scale_x_continuous(breaks =seq(-.2,1,by=0.1), limits = c(-.2, 1),name="Slope Coefficient in One-Shot Setting (OS)")+
    theme_minimal() +
    theme(legend.position="none") 
    
pdf("img/sdiff_bubbles.pdf")
sdiffBubbles
dev.off()




# --- punishment line graphs by type

wlclass <- read_dta("DTA/weakest_link_class.dta")

## -- treatment 41
  # keep only treatment 41
rpsdat <- subset(wlclass,select=c(PUN,con1_inv,t_con1_inv41),treatment==41)
  # make copy for total line
rpstotal <- rpsdat
rpstotal$Type <- "Total"
rpsdat$Type <- "NPun"
rpsdat$Type[rpsdat$t_con1_inv41 == -1] <- "Pun"
rpsdat$Type[rpsdat$t_con1_inv41 == 2] <- "APun"
rpsdat$Type[rpsdat$t_con1_inv41 == 100] <- "NCL"
  # join lists
rpsdat <- rbind(rpsdat,rpstotal)
  # remove NCL
rpsdat <- subset(rpsdat,Type!="NCL")
  # order types
rpsdat$Type <- factor(rpsdat$Type,levels=c("Pun","APun","Total","NPun"))
  # merge dataset
grps <- rpsdat %>% group_by(Type,con1_inv) %>% summarise(PUN=mean(PUN))
grps$line <- 'solid'
grps$line[grps$Type=="Total"] <- 'dashed'

  # create graph object
rpsg <- ggplot(data=grps,aes(x=con1_inv,y=PUN,
                             linetype=Type,group=Type,colour=Type)) +
  geom_point(aes(shape=Type),size=2.5)+
  geom_line(size=.85)+
  scale_linetype_manual(values =
                          c("Pun" = 1, "NPun" = 1, "APun" = 1, "Total" = 2))+
  theme_minimal() +
  labs(x=expression(20-g[j]),y=expression(mean*d[ij])) +
  scale_y_continuous(limits=c(0,4)) +
  scale_colour_tableau()+
  theme(legend.position="bottom")
  # export to pdf
pdf("img/41_type.pdf",width=6,height=4)
rpsg
dev.off()

## -- treatment 61
  # keep only treatment 61
wpsdat <- subset(wlclass,select=c(PUN,con1_inv,t_con1_inv61),treatment==61)
  # make copy for total line
wpstotal <- wpsdat
wpstotal$Type <- "Total"
wpsdat$Type <- "NPun"
wpsdat$Type[wpsdat$t_con1_inv61 == -1] <- "Pun"
wpsdat$Type[wpsdat$t_con1_inv61 == 2] <- "APun"
wpsdat$Type[wpsdat$t_con1_inv61 == 100] <- "NCL"
  # join lists
wpsdat <- rbind(wpsdat,wpstotal)
  # remove NCL
wpsdat <- subset(wpsdat,Type!="NCL")
  # order types
wpsdat$Type <- factor(wpsdat$Type,levels=c("Pun","APun","Total","NPun"))
  # merge dataset
gwps <- wpsdat %>% group_by(Type,con1_inv) %>% summarise(PUN=mean(PUN))
gwps$line <- 'solid'
gwps$line[gwps$Type=="Total"] <- 'dashed'

  # create graph object
wpsg <- ggplot(data=gwps,aes(x=con1_inv,y=PUN,
                             linetype=Type,group=Type,colour=Type)) +
  geom_point(aes(shape=Type),size=2.5)+
  geom_line(size=.85)+
  scale_linetype_manual(values =
                          c("Pun" = 1, "NPun" = 1, "APun" = 1, "Total" = 2))+
  theme_minimal() +
  labs(x=expression(20-g[j]),y=expression(mean*d[ij])) +
  scale_y_continuous(limits=c(0,4)) +
  scale_colour_tableau()+
  theme(legend.position="bottom")
  # export to pdf
pdf("img/61_type.pdf",width=6,height=4)
wpsg
dev.off()





## -- treatment 51
  # keep only treatment 61
ccclass <- read_dta("DTA/weakest_cell.dta")
ccdat <- subset(ccclass,select=c(condcon,cell,t_cell51),treatment==51)
  # make copy for total line
cctotal <- ccdat
cctotal$Type <- "Total"
ccdat$Type <- "FR"
ccdat$Type[ccdat$t_cell51 == -1] <- "CC"
ccdat$Type[ccdat$t_cell51 == 2] <- "TC"
ccdat$Type[ccdat$t_cell51 == 100] <- "NC"
  # join lists
ccdat <- rbind(ccdat,cctotal)
  # remove NCL
ccdat <- subset(ccdat,Type!="NC")
  # order types
ccdat$Type <- factor(ccdat$Type,levels=c("CC","TC","Total","FR"))
  # merge dataset
gcc <- ccdat %>% group_by(Type,cell) %>% summarise(condcon=mean(condcon))
gcc$line <- 'solid'
gcc$line[gcc$Type=="Total"] <- 'dashed'

  # create graph object
ccg <- ggplot(data=gcc,aes(x=cell,y=condcon,
                             linetype=Type,group=Type,colour=Type)) +
  geom_point(aes(shape=Type),size=2.5)+
  geom_line(size=.85)+
  geom_abline(intercept=0,slope=1,linetype="dashed",colour="grey") +
  scale_linetype_manual(values = c("CC" = 1, "FR" = 1, "TC" = 1, "Total" = 2))+
  theme_minimal() +
  labs(x=expression(bar(g[j])),y=expression(g[i])) +
  scale_y_continuous(limits=c(0,20)) +
  scale_colour_tableau()+
  annotate("text",x=19,y=20,colour="grey",label="45°") +
  theme(legend.position="bottom")
  # export to pdf
pdf("img/51_type.pdf",width=6,height=6)
ccg
dev.off()





# --- bar chart 2D types

twodat <- subset(wlclass,select=c(sid,t_con1_inv41,t_con1_inv61))

twodat <- twodat %>% group_by(sid) %>% summarise(RPSN=mean(t_con1_inv41),WPSN=mean(t_con1_inv61))
twodat$RPS <- 'NCL'
twodat$RPS[twodat$RPSN == 0] <- "NPun"
twodat$RPS[twodat$RPSN == 2] <- "APun"
twodat$RPS[twodat$RPSN == -1] <- "Pun"
twodat$WPS <- 'NCL'
twodat$WPS[twodat$WPSN == 0] <- "NPun"
twodat$WPS[twodat$WPSN == 2] <- "APun"
twodat$WPS[twodat$WPSN == -1] <- "Pun"

twodat$WPS <- factor(twodat$WPS,levels=c('NCL','APun','NPun','Pun'))
twodat$RPS <- factor(twodat$RPS,levels=c('Pun','NPun','APun','NCL'))

twodat$combo <- paste(twodat$RPS,twodat$WPS,sep="")


colorsb=c('#c0c0c0','#ff7f0e','#d62728','#1f77b4')
colorsf=c('#ffffff','#ff7f0e','#d62728','#1f77b4')


twog <- ggplot(twodat,aes(x=RPS,colour=WPS,fill=WPS))+
  geom_bar(aes(y = (..count..)/sum(..count..)))+
  scale_fill_manual(values=colorsf)+
  scale_colour_manual(values=colorsb)+
  labs(y="Types in %") +
  theme_minimal() +
  theme(legend.position="bottom")


  # export to pdf
pdf("img/2d_types.pdf",width=6,height=4)
twog
dev.off()

# --- line graphs punishment and contribution behavior

wlrepeat <- read_dta("DTA/wlrp_R.dta")
rpdat <- subset(wlrepeat,treatment == 21)

head(rpdat)

  # assign few and many to group combinations
rpdat$Punishers <- "none"
rpdat$Punishers[rpdat$count41 == 1 | rpdat$count41 == 2] <- "few"
rpdat$Punishers[rpdat$count41 == 3 | rpdat$count41 == 4] <- "many"
rpdat$Punishers <- factor(rpdat$Punishers,levels=c('none','few','many'))
table(rpdat$Punishers)

rpdat$payoff <- 20 - rpdat$contrib + 1.6 * rpdat$contrib - 10 * rpdat$PUN
  # reduce to combo level
rpdat <- rpdat %>% group_by(period,Punishers) %>% summarise_each(funs(mean,sd,se=sd(.)/sqrt(n())))
head(rpdat)

colBlu <- brewer.pal(n=7,'Blues')[4:7]
colRed <- brewer.pal(n=7,'Reds')[4:7]
colGre <- brewer.pal(n=7,'Greens')[4:7]

## -- punishment
punperiodg <- ggplot(rpdat,aes(x=period,y=PUN_mean,group=Punishers,colour=Punishers,fill=Punishers,linetype=Punishers))+
  geom_ribbon(aes(ymin=PUN_mean-PUN_se,
                  ymax=PUN_mean+PUN_se
                  ),linetype=3,alpha=.1) +
  geom_point(aes(shape=Punishers),size=2.5)+
  geom_line(size=.85)+
  scale_colour_manual(values=colRed)+
  scale_fill_manual(values=colRed)+
  scale_x_continuous(name="Periods",breaks=seq(1,10,1)) +
  scale_y_continuous(limits=c(0,1),name="Average Punishment") +
  theme_minimal()+
  theme(legend.position="bottom")

pdf("img/21_pun.pdf",width=6,height=4)
punperiodg
dev.off()

## -- contribution
contribperiodg <- ggplot(rpdat,aes(x=period,y=contrib_mean,group=Punishers,colour=Punishers,fill=Punishers,linetype=Punishers))+
  geom_ribbon(aes(ymin=contrib_mean-contrib_se,
                  ymax=contrib_mean+contrib_se
                  ),linetype=3,alpha=.1) +
  geom_point(aes(shape=Punishers),size=2.5)+
  geom_line(size=.85)+
  scale_colour_manual(values=colBlu)+
  scale_fill_manual(values=colBlu)+
  scale_x_continuous(name="Periods",breaks=seq(1,10,1)) +
  scale_y_continuous(limits=c(0,20),name="Average Contribution") +
  theme_minimal()+
  theme(legend.position="bottom")

pdf("img/21_contrib.pdf",width=6,height=4)
contribperiodg
dev.off()


## -- payoff
payoffperiodg <- ggplot(rpdat,aes(x=period,y=payoff_mean,group=Punishers,colour=Punishers,fill=Punishers,linetype=Punishers))+
  geom_ribbon(aes(ymin=payoff_mean-payoff_se,
                  ymax=payoff_mean+payoff_se
                  ),linetype=3,alpha=.1) +
  geom_point(aes(shape=Punishers),size=2.5)+
  geom_line(size=.85)+
  scale_colour_manual(values=colGre)+
  scale_fill_manual(values=colGre)+
  scale_x_continuous(name="Periods",breaks=seq(1,10,1)) +
  scale_y_continuous(limits=c(7,32),name="Average Payoff") +
  theme_minimal()+
  theme(legend.position="bottom")

pdf("img/21_payoff.pdf",width=6,height=4)
payoffperiodg
dev.off()

# --- line graphs punishment and contribution behavior

wlrepeat <- read_dta("DTA/wlrp_R.dta")
wpdat <- subset(wlrepeat,treatment == 71)

head(wpdat)

  # assign few and many to group combinations
wpdat$Punishers <- "none"
wpdat$Punishers[wpdat$count61 == 1 | wpdat$count61 == 2] <- "few"
wpdat$Punishers[wpdat$count61 == 3 | wpdat$count61 == 4] <- "many"
wpdat$Punishers <- factor(wpdat$Punishers,levels=c('none','few','many'))
table(wpdat$Punishers)

wpdat$payoff <- 20 - wpdat$contrib + 1.6 * wpdat$minimum - 10 * wpdat$PUN
  # reduce to combo level
wpdat <- wpdat %>% group_by(period,Punishers) %>% summarise_each(funs(mean,sd,se=sd(.)/sqrt(n())))
head(wpdat)

colBlu <- brewer.pal(n=7,'Blues')[4:7]
colRed <- brewer.pal(n=7,'Reds')[4:7]
colGre <- brewer.pal(n=7,'Greens')[4:7]

## -- punishment
punperiodg <- ggplot(wpdat,aes(x=period,y=PUN_mean,group=Punishers,colour=Punishers,fill=Punishers,linetype=Punishers))+
  geom_ribbon(aes(ymin=PUN_mean-PUN_se,
                  ymax=PUN_mean+PUN_se
                  ),linetype=3,alpha=.1) +
  geom_point(aes(shape=Punishers),size=2.5)+
  geom_line(size=.85)+
  scale_colour_manual(values=colRed)+
  scale_fill_manual(values=colRed)+
  scale_x_continuous(name="Periods",breaks=seq(1,10,1)) +
  scale_y_continuous(limits=c(0,1),name="Average Punishment") +
  theme_minimal()+
  theme(legend.position="bottom")

pdf("img/71_pun.pdf",width=6,height=4)
punperiodg
dev.off()



## -- contribution -- average
contribperiodg <- ggplot(wpdat,aes(x=period,y=contrib_mean,group=Punishers,colour=Punishers,fill=Punishers,linetype=Punishers))+
  geom_ribbon(aes(ymin=contrib_mean-contrib_se,
                  ymax=contrib_mean+contrib_se
                  ),linetype=3,alpha=.1) +
  geom_point(aes(shape=Punishers),size=2.5)+
  geom_line(size=.85)+
  scale_colour_manual(values=colBlu)+
  scale_fill_manual(values=colBlu)+
  scale_x_continuous(name="Periods",breaks=seq(1,10,1)) +
  scale_y_continuous(limits=c(0,20),name="Contrib Contribution") +
  theme_minimal()+
  theme(legend.position="bottom")

pdf("img/71_contrib.pdf",width=6,height=4)
contribperiodg
dev.off()


## -- contribution -- minimum
minimumperiodg <- ggplot(wpdat,aes(x=period,y=minimum_mean,group=Punishers,colour=Punishers,fill=Punishers,linetype=Punishers))+
  geom_ribbon(aes(ymin=minimum_mean-minimum_se,
                  ymax=minimum_mean+minimum_se
                  ),linetype=3,alpha=.1) +
  geom_point(aes(shape=Punishers),size=2.5)+
  geom_line(size=.85)+
  scale_colour_manual(values=colBlu)+
  scale_fill_manual(values=colBlu)+
  scale_x_continuous(name="Periods",breaks=seq(1,10,1)) +
  scale_y_continuous(limits=c(0,20),name="Minimum Contribution") +
  theme_minimal()+
  theme(legend.position="bottom")

pdf("img/71_minimum.pdf",width=6,height=4)
minimumperiodg
dev.off()


## -- payoff
payoffperiodg <- ggplot(wpdat,aes(x=period,y=payoff_mean,group=Punishers,colour=Punishers,fill=Punishers))+
  geom_ribbon(aes(ymin=payoff_mean-payoff_se,
                  ymax=payoff_mean+payoff_se
                  ),linetype=3,alpha=.1) +
  geom_point(aes(shape=Punishers),size=2.5)+
  geom_line(size=.85)+
  scale_colour_manual(values=colGre)+
  scale_fill_manual(values=colGre)+
  scale_x_continuous(name="Periods",breaks=seq(1,10,1)) +
  scale_y_continuous(limits=c(4,32),name="Average Payoff") +
  theme_minimal()+
  theme(legend.position="bottom")

pdf("img/71_payoff.pdf",width=6,height=4)
payoffperiodg
dev.off()





# --- bar chart equilibria

wlrepeat <- read_dta("DTA/wlrp_R.dta")
wpdat <- subset(wlrepeat,treatment == 71)

head(wpdat)

  # assign few and many to group combinations
wpdat$Punishers <- "none"
wpdat$Punishers[wpdat$count61 == 1 | wpdat$count61 == 2] <- "few"
wpdat$Punishers[wpdat$count61 == 3 | wpdat$count61 == 4] <- "many"
wpdat$Punishers <- factor(wpdat$Punishers,levels=c('none','few','many'))
table(wpdat$Punishers)


table(wpdat$treatment)

table(wpdat$equi)

table(wpdat$count61)
table(wpdat$equi,wpdat$period)
table(wpdat$Punishers)

wpbar <- wpdat %>% group_by(period,Punishers) %>% summarise(percent = mean(equi))
table(wpdat$equi,wpdat$Punishers)

table(wpbar$period,wpbar$percent)
table(wpbar$percent,wpbar$Punishers)

ggplot(wpbar,aes(x=period,y=percent,group=Punishers,
                    colour=Punishers,fill=Punishers))+
  geom_bar(stat="identity")

p4 <- ggplot() + geom_bar(aes(y = percentage, x = year, fill = product), data = charts.data,
                           stat="identity")


# --- bubbles equilibria  WPS
wlrepeat <- read_dta("DTA/wlrp_R.dta")
wpdat <- subset(wlrepeat,treatment == 71)
head(wpdat) # assign few and many to group combinations
wpdat$Punishers <- "none"
wpdat$Punishers[wpdat$count61 == 1 | wpdat$count61 == 2] <- "few"
wpdat$Punishers[wpdat$count61 == 3 | wpdat$count61 == 4] <- "many"
wpdat$Punishers <- factor(wpdat$Punishers,levels=c('none','few','many'))
table(wpdat$Punishers)


  # assign few and many to group combinations
table(wpdat$Punishers)

wlgroups <- table(wpdat$Punishers)

  # create distribution of equilibria
wpdat$equidist <- wpdat$equi * wpdat$minimum

  # keep only positive equilibra
wpdat <- subset(wpdat,equidist != 0)

table(wpdat$Punishers,wpdat$equi)

none <- subset(wpdat,Punishers=="none")
few <- subset(wpdat,Punishers=="few")
many <- subset(wpdat,Punishers=="many")

noneequi <- data.frame(table(none$equidist,none$period))
noneequi$typ <- "none"
fewequi <- data.frame(table(few$equidist,few$period))
fewequi$typ <- "few"
manyequi <- data.frame(table(many$equidist,many$period))
manyequi$typ <- "many"

equis <- rbind(noneequi,manyequi,fewequi)
colnames(equis) <- c('equi','Periods','Frequency','Type')
equis$Type <- factor(equis$Type,levels=c("none","few","many"))

equis <- subset(equis,Frequency!=0)

wlgroups
equis$frac <- equis$Frequency/(wlgroups[[1]]/10)
equis$frac[equis$Type == "few"] <- equis$Frequency[equis$Type == "few"]/(wlgroups[[2]]/10)
equis$frac[equis$Type == "many"] <- equis$Frequency[equis$Type == "many"]/(wlgroups[[3]]/10)

equis$Percent <- paste(round(equis$frac * 100,0),"%")

equis$equi <- unfactor(equis$equi)
equis$Equilibria <- equis$equi
  # change
equis$Equilibria <- ifelse(equis$Type=='few', equis$equi-equis$frac-0.6, equis$equi+equis$frac+0.6)
equis$Equilibria[equis$Type == "none"] <- equis$equi[equis$Type == "none"]
equis$Equilibria <- ifelse(equis$equi != 20,equis$equi,equis$Equilibria)

  # point location adjustment
equis$labpos <- equis$Equilibria
equis$labpos[equis$Type == "few"] <- equis$Equilibria[equis$Type == "few"]-1
equis$labpos[equis$Type == "few" & equis$Periods == 3] <- equis$Equilibria[equis$Type == "few"]
equis$labpos[equis$Type == "few" & equis$Periods == 4] <- equis$Equilibria[equis$Type == "few"]
equis$labpos[equis$Type == "few" & equis$Periods == 5] <- equis$Equilibria[equis$Type == "few"]
equis$labpos[equis$Type == "few" & equis$Periods == 6] <- equis$Equilibria[equis$Type == "few"]
equis$labpos[equis$Type == "few" & equis$Periods == 7] <- equis$Equilibria[equis$Type == "few"]

equis$labpos[equis$Type == "many" & equis$frac > 0.3] <- equis$Equilibria[equis$Type == "many" & equis$frac > 0.3] + .7
equis$labpos[equis$Type == "many" & equis$frac < 0.3 & equis$frac > 0.1] <- equis$Equilibria[equis$Type == "many" & equis$frac < 0.3 & equis$frac > 0.1] + .5
equis$labpos[equis$Type == "many" & equis$frac < 0.1] <- equis$Equilibria[equis$Type == "many" & equis$frac < 0.1]

equis$Shape = 21
equis$Shape[equis$Type=="many"] = 22
equis$Shape[equis$Type=="none"] = 23

Dodge = position_dodge(width=.3)
equig <- ggplot(equis,aes(x=Periods,y=Equilibria,size=frac,
                          group=Type,label=Percent))+
  geom_point(aes(shape=Type,fill=Type),alpha=.9,colour="black",position=Dodge) +
  geom_text(colour="black",size=3,aes(y=labpos),position=Dodge) +
  scale_x_discrete(limits=c(1,2,3,4,5,6,7,8,9,10)) +
  scale_y_continuous(limits=c(0,25),breaks=seq(0,20,5)) +
  scale_fill_tableau() +
  scale_shape_manual(values=c(23,22,21)) +
  scale_size(range=c(5,20),guide='none') +
  scale_colour_tableau() +
  theme_minimal() +
  guides(fill=guide_legend(override.aes=list(size=5))) +
  theme(legend.position="bottom")

pdf("img/equi_bubble.pdf",width=7,height=5)
equig
dev.off()




# --- bubbles equilibria  C-game
wlrepeat <- read_dta("DTA/wlrp_R.dta")
wpdat <- subset(wlrepeat,treatment == 71)
head(wpdat) # assign few and many to group combinations
wpdat$Punishers <- "none"
wpdat$Punishers[wpdat$count51 == 1 | wpdat$count51 == 2] <- "few"
wpdat$Punishers[wpdat$count51 == 3 | wpdat$count51 == 4] <- "many"
wpdat$Punishers <- factor(wpdat$Punishers,levels=c('none','few','many'))
table(wpdat$Punishers)




  # assign few and many to group combinations
table(wpdat$Punishers)

wlgroups <- table(wpdat$Punishers)

  # create distribution of equilibria
wpdat$equidist <- wpdat$equi * wpdat$minimum

  # keep only positive equilibra
wpdat <- subset(wpdat,equidist != 0)

table(wpdat$count51,wpdat$equi)

wpdat$count = 1
wpdat <- wpdat %>% group_by(count51, period) %>% summarise(equilibria = sum(count),level = mean(equidist))

ggplot(wpdat,aes(x=period,y=level,group=as.factor(count51),
                 colour=as.factor(count51),shape=as.factor(count51),
                 size=equilibria))+
  geom_point(position="jitter")


few <- subset(wpdat,Punishers=="few")
many <- subset(wpdat,Punishers=="many")

noneequi <- data.frame(table(none$equidist,none$period))
noneequi$typ <- "none"
fewequi <- data.frame(table(few$equidist,few$period))
fewequi$typ <- "few"
manyequi <- data.frame(table(many$equidist,many$period))
manyequi$typ <- "many"

equis <- rbind(noneequi,manyequi,fewequi)
colnames(equis) <- c('equi','Periods','Frequency','Type')
equis$Type <- factor(equis$Type,levels=c("none","few","many"))

equis <- subset(equis,Frequency!=0)

wlgroups
equis$frac <- equis$Frequency/(wlgroups[[1]]/10)
equis$frac[equis$Type == "few"] <- equis$Frequency[equis$Type == "few"]/(wlgroups[[2]]/10)
equis$frac[equis$Type == "many"] <- equis$Frequency[equis$Type == "many"]/(wlgroups[[3]]/10)

equis$Percent <- paste(round(equis$frac * 100,0),"%")

equis$equi <- unfactor(equis$equi)
equis$Equilibria <- equis$equi
  # change
equis$Equilibria <- ifelse(equis$Type=='few', equis$equi-equis$frac-0.6, equis$equi+equis$frac+0.6)
equis$Equilibria[equis$Type == "none"] <- equis$equi[equis$Type == "none"]
equis$Equilibria <- ifelse(equis$equi != 20,equis$equi,equis$Equilibria)

equis$Shape = 21
equis$Shape[equis$Type=="many"] = 22
equis$Shape[equis$Type=="none"] = 23

Dodge = position_dodge(width=.3)
equig <- ggplot(equis,aes(x=Periods,y=equi,size=frac,
                          group=count51,label=Percent))+
  geom_point(aes(shape=count51,fill=count51),alpha=.9,colour="black",position=Dodge) +
  #geom_text(colour="black",size=3,aes(y=labpos),position=Dodge) +
  scale_x_discrete(limits=c(1,2,3,4,5,6,7,8,9,10)) +
  scale_y_continuous(limits=c(0,25),breaks=seq(0,20,5)) +
  scale_fill_tableau() +
  #scale_shape_manual(values=c(23,22,21)) +
  scale_size(range=c(5,20),guide='none') +
  scale_colour_tableau() +
  theme_minimal() +
  guides(fill=guide_legend(override.aes=list(size=5))) +
  theme(legend.position="bottom")

pdf("img/equi_bubble.pdf",width=7,height=5)
equig
dev.off()


