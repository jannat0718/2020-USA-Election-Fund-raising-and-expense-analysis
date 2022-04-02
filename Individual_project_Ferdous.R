##Packages and libraries
library(tidyverse)
library(dplyr)
library(ggplot2)
library(plotly)
library(lubridate)
library(stringr)
install.packages("usmap")
library(usmap)

##Import data set-1:All Candidate Funding amount data 
all_candidate <- read.table("Ind project/All candidate.txt", sep = "|",fill = T,quote="")

colnames(all_candidate)<-c('CAND_ID','CAND_NAME','CAND_Incumbent_Status','Party_code','CAND_PTY_AFFILIATION','Total_RECEIPTS',
                           'TRANS_FROM_AUTH_COM','Total_Disbursements','TRANS_TO_AUTH_COM','Begining_cash','Ending_cash','CAND_CONTRIB',
                           'CAND_LOANS','OTHER_LOANS','CAND_LOAN_REPAY','OTHER_LOAN_REPAY',	'DEBTS_OWED_BY','Total_INDIV_CONTRIB',
                           'CAND_OFFICE_ST','CAND_OFFICE_DISTRICT','SPEC_ELECTION','PRIM_ELECTION','RUN_ELECTION','GEN_ELECTION',
                           'GEN_ELECTION_PRECENT','OTHER_Political_CMTE_CONTRIB','POL_Party_CONTRIB','Coverage_END_DT','INDIV_REFUNDS',
                           'CMTE_REFUNDS')

## EDA
summary(all_candidate)
str(all_candidate)

## Data types correction
All_candidate <- all_candidate %>%mutate(CAND_NAME=as.factor(all_candidate$CAND_NAME),
                                         CAND_Incumbent_Status=as.factor(all_candidate$CAND_Incumbent_Status),
                                         Party_code =as.factor(all_candidate$Party_code),
                                         CAND_PTY_AFFILIATION =as.factor(all_candidate$CAND_PTY_AFFILIATION),
                                         CAND_OFFICE_ST =as.factor(all_candidate$CAND_OFFICE_ST),
                                         CAND_OFFICE_DISTRICT =as.factor(all_candidate$CAND_OFFICE_DISTRICT),
                                         Coverage_END_DT=as.Date(all_candidate$Coverage_END_DT,'%m/%d/%Y'))

##Remove Duplicate Value
All_candidate<-All_candidate[!duplicated(All_candidate$CAND_ID),]

##Drop Na's and incorrect negative amount
All_candidate <-All_candidate%>%drop_na(Total_RECEIPTS)%>%filter(Total_RECEIPTS>0&Total_Disbursements>0)

#Correct Receipt and Disbursement amount
All_candidate.list<-All_candidate%>% filter(TRANS_FROM_AUTH_COM!=0.0 & TRANS_TO_AUTH_COM!=0.0)%>%
  mutate(Total_RECEIPTS = Total_RECEIPTS - TRANS_FROM_AUTH_COM)%>%
  mutate(Total_Disbursements =Total_Disbursements - TRANS_TO_AUTH_COM) 

##Left Join
Candidate.join= left_join(All_candidate,All_candidate.list[ ,c(1,6,8) ],by='CAND_ID')
n=dim(Candidate.join)

##Replacing corrected Receipt and Disbursement amount 

for (i in n[1]){
  if(Candidate.join$TRANS_FROM_AUTH_COM[i]!=0.0 & Candidate.join$TRANS_TO_AUTH_COM[i]!=0.0){
    Candidate.join$Total_RECEIPTS.x[i]=Candidate.join$Total_RECEIPTS.y[i]
    Candidate.join$Total_Disbursements.x[i]=Candidate.join$Total_Disbursements.y[i]
  }
}

#split Name into two columns
splits <- str_split_fixed(Candidate.join$CAND_NAME, ", ", 2)

#now merge these two columns the other way round
Candidate.join$CAND_NAME <- paste(splits[,2], splits[,1], sep = ' ')


### 10 Candidate list raised most amount 
Candidate_most_List<-Candidate.join[,c(1:30)]%>%arrange(desc(Total_RECEIPTS.x))%>%
  select("CAND_NAME","CAND_PTY_AFFILIATION","Total_RECEIPTS.x")

##Horizontal barplot

Candidate_plot<-Candidate_most_List[1:10,c(1,3)]
Candidate_plot$Total_RECEIPTS.x<-Candidate_plot$Total_RECEIPTS.x/1000000
Candidate_plot %>%
  ggplot() +
  geom_bar(aes(x = reorder(factor(CAND_NAME), Total_RECEIPTS.x), y = Total_RECEIPTS.x), stat = "identity", fill =Candidate_plot$Total_RECEIPTS.x ) +
  scale_y_continuous(labels = scales::format_format(big.mark = ".", decimal.mark = ",", scientific = FALSE)) +
  labs(x = "Candidate", 
       y = "$ in Milions",
       title = "Candidate raised most Campaign Fund") +
  coord_flip()

#Biden  vs Trump
BvT_Financce_summary<-Candidate.join[,c(1:30)]%>%filter(CAND_NAME=='JOSEPH R JR BIDEN'| CAND_NAME=='DONALD J. TRUMP')%>%
  mutate(Candidate_name =CAND_NAME,
         Party=CAND_PTY_AFFILIATION,
         Candidate_Incumbant_status=CAND_Incumbent_Status,
         Total_Raised_Amount=Total_RECEIPTS.x,
         Total_spent=Total_Disbursements.x,
         Committee_Contribution=TRANS_FROM_AUTH_COM,
         Cash_on_Hand=Ending_cash,
         Total_Individual_contribution=Total_INDIV_CONTRIB,
         Debt_Owed=DEBTS_OWED_BY,
         Coverage_last_date=Coverage_END_DT)
BvT_Financce_summary<-BvT_Financce_summary[,c(31:40)]

##Bar Plot
Summary_plot<-as.matrix(BvT_Financce_summary[,c(4:9)]/1000000)
Summary_plot
rownames(Summary_plot)<-c( 'BIDEN','TRUMP')
barplot(Summary_plot, main="Biden Vs Trump Finance Comparision",
        ylab= "$ in Million", col=c("darkblue","red"),
        legend=rownames(Summary_plot),beside=TRUE,xlim = c(0, 30))


##Data set 2:Individual Contribution transaction from  2020-08-30  to 2020-09-08
memory.limit()
Indi_cont_Aug <- read.table("Ind project/itcont_2020_Aug.txt", sep = "|",fill = TRUE, quote="")
colnames(Indi_cont_Aug)<-c('CMTE_ID','AMNDT_IND','RPT_TP','TRANSACTION_PGI','IMAGE_NUM','TRANSACTION_TP',
                           'ENTITY_TP','NAME','CITY','STATE','ZIP_CODE','EMPLOYER','OCCUPATION','TRANSACTION_DT',
                           'TRANSACTION_AMT','OTHER_ID','TRAN_ID','FILE_NUM','MEMO_CD','MEMO_TEXT','SUB_ID')

##Drop unrequited column
Indi_cont_Aug<-Indi_cont_Aug[,c(1:3,7:15)]

##EDA
str(Indi_cont_Aug)
summary(Indi_cont_Aug)
## Data types correction
Indi_Aug <- Indi_cont_Aug %>%mutate(AMNDT_IND=as.factor(Indi_cont_Aug$AMNDT_IND),
                                    RPT_TP =as.factor(Indi_cont_Aug$RPT_TP),
                                    ENTITY_TP  =as.factor(Indi_cont_Aug$ENTITY_TP),
                                    NAME =as.factor(Indi_cont_Aug$NAME),
                                    CITY  =as.factor(Indi_cont_Aug$CITY),
                                    STATE  =as.factor(Indi_cont_Aug$STATE),
                                    ZIP_CODE =as.factor(Indi_cont_Aug$ZIP_CODE),
                                    EMPLOYER =as.factor(Indi_cont_Aug$EMPLOYER),
                                    OCCUPATION=as.factor(Indi_cont_Aug$OCCUPATION))

##Drop NA's
Indi_Aug <-Indi_Aug%>%group_by(TRANSACTION_AMT)%>%drop_na(TRANSACTION_AMT)%>%filter(TRANSACTION_AMT>0)

##Date formatting correction
x <- Indi_Aug$TRANSACTION_DT
y<-gsub('^([0-9]{3})([0-9]+)$', '\\1,\\2', x)
z<-str_split_fixed(y, "[,]", 2)
x1<-z[,c(1)]
y1<-gsub('^([0-9]{1})([0-9]+)$', '\\1,\\2', x1)
z1<-str_split_fixed(y1, "[,]", 2)
a <- paste(z1[,1], z1[,2],z[,2],sep = '/')
Indi_Aug$TRANSACTION_DT<-as.Date(a,"%m/%d/%Y")
summary(Indi_Aug)

##DATA SET 3 -Committee master detailed of committee affiliation

comittee_master <-read.table("Ind project/Committee master.txt", sep = "|",fill = TRUE,quote="" )
colnames(comittee_master)<-c('CMTE_ID','CMTE_NM','Treasure_name','CMTE_ADR1','CMTE_ADR2','CMTE_CITY','CMTE_ST',
                             'CMTE_ZIP','CMTE_Designation','CMTE_Type','CMTE_PTY_AFFILIATION','CMTE_FILING_FREQ','ORG_TYP',
                             'CONNECTED_ORG_NM','CAND_ID')
head(comittee_master)

#EDA
str(Comittee_master)

## Data types correction
Comittee_master <- comittee_master[,c(1,2,11,13:15)] %>%mutate(CMTE_NM =as.factor(comittee_master$CMTE_NM ),
                                                               CMTE_PTY_AFFILIATION =as.factor(comittee_master$CMTE_PTY_AFFILIATION),
                                                               ORG_TYP=as.factor(comittee_master$ORG_TYP),
                                                               CONNECTED_ORG_NM=as.factor(comittee_master$CONNECTED_ORG_NM),
                                                               CAND_ID=as.factor(comittee_master$CAND_ID))

##EDA
summary(Comittee_master)


##Join  Indi_Aug & Comittee_master and Candidate all  by Committee ID and Candidate ID 
Candidate.join$CAND_ID=as.factor(Candidate.join$CAND_ID)
Contribution.join<-left_join(Indi_Aug,Comittee_master,by='CMTE_ID')
Contribution.join<-left_join(Contribution.join,Candidate.join[,c(1:3,5)],by="CAND_ID")
Contribution.join$CMTE_ID<-as.factor(Contribution.join$CMTE_ID)

##Committee raised amount from Individual contributor
Committ_contri<-Contribution.join%>%group_by(CMTE_ID )%>%summarise(No_of_ID=n(),
                                                                   Total_amount_perID=sum(TRANSACTION_AMT),
                                                                   Average_amount_ID=Total_amount_perID/No_of_ID,
                                                                   Committee_name=CMTE_NM,
                                                                   Candidate_name=CAND_NAME,
                                                                   Party=CAND_PTY_AFFILIATION,
                                                                   Incumbant_satus=CAND_Incumbent_Status)

##Drop Duplicated value
Committ_contri<-Committ_contri[!duplicated(Committ_contri$CMTE_ID),] 

##List 10 most committee from Individual contributor
Most_Comt_fund<-Committ_contri%>%select("Committee_name","Total_amount_perID","Candidate_name",'Incumbant_satus','Party')%>%
  arrange(desc(Total_amount_perID))
head(Most_Comt_fund[,c(2,3)],10 )

##Barplot :Committee raised most Individual Contribution
Most_Comt_fund_plot<-Most_Comt_fund[1:10,c(2,3)]
Most_Comt_fund_plot$Total_amount_perID<-Most_Comt_fund_plot$Total_amount_perID/1000000
Most_Comt_fund_plot%>%
  ggplot() +
  geom_bar(aes(x = reorder(factor(Committee_name ), Total_amount_perID), y = Total_amount_perID), stat = "identity",fill=(Most_Comt_fund_plot$Total_amount_perID )) +
  scale_y_continuous(labels = scales::format_format(big.mark = ".", decimal.mark = ",", scientific = FALSE)) +
  labs(x = "Committee", 
       y = "$ in Milions",
       title = "Committee raised most Individual Contribution") +
  coord_flip()


##State-wise Fundraiser
dim(table(Contribution.join$STATE))

#Filter to get Only US 50 States data
##AE,AA,AP-Military Sates
##DC,GU,MH,MP,PR,VI-commonwealth states
##Other- AS,ZZ,PW

State_contri<-  Contribution.join%>%group_by(STATE)%>%
  filter(STATE!='AE' & STATE!='AA' & STATE!='AP'&STATE!='DC'&STATE!='GU'&STATE!='MH'&STATE!='MP'&STATE!='PR'&STATE!='VI'& STATE!='AS'& STATE!='PW'& STATE!='ZZ')                                                               

State_Contri<-State_contri%>%group_by(STATE)%>%summarise(No_of_Contributor=n_distinct(CMTE_ID),
                                                         Total_amount_perState=sum(TRANSACTION_AMT),
                                                         Average_amount_ID=Total_amount_perState/No_of_Contributor,
                                                         Committee_name=CMTE_NM,
                                                         Candidate_name=CAND_NAME,
                                                         Party=CAND_PTY_AFFILIATION,
                                                         Incumbant_satus=CAND_Incumbent_Status) 


##50 states Raised amount in order

State_order<-State_Contri[!duplicated(State_Contri$Total_amount_perState),] 
State_order<-State_order[2:51,c(1:4)]%>%arrange(desc(Total_amount_perState))

State_Plot<-State_order[,c(1,3)]
##50 states Fund raising amount Plot

State_fund_Plot<-plot_geo(State_Plot,
                          locationmode='USA-states')%>%
  add_trace(locations=~State_Plot$STATE,
            z=~Total_amount_perState,
            zmin=min(State_Plot$Total_amount_perState),
            zmax=max(State_Plot$Total_amount_perState),
            color=~Total_amount_perState)%>%
  layout(geo=list(scope='usa'),
         title='USA Election 2020\n50 States Funds Raising Amount From Aug 30 to Sep 8')
State_fund_Plot


##Fund raising changes by each day over states

Date_fund<-State_contri%>%group_by(STATE,TRANSACTION_DT)%>%summarise(No_of_Contributor=n_distinct(CMTE_ID),
                                                                     Total_amount_State_date=sum(TRANSACTION_AMT))

Date_fund<-Date_fund[-c(1:10), ] 

Date_fund<-Date_fund%>%group_by(TRANSACTION_DT,STATE)%>%summarise(No_of_Contributor=No_of_Contributor,
                                                                  Total_amount_State_date=Total_amount_State_date)
##Convert Date as factor for plotting
Date_fund$TRANSACTION_DT<-as.factor(Date_fund$TRANSACTION_DT)

##50 states day to day funding trend

State_DTD_Plot<-plot_geo(Date_fund,
                         locationmode='USA-states',
                         frame=~Date_fund$TRANSACTION_DT)%>%
  add_trace(locations=~Date_fund$STATE,
            z=~Date_fund$Total_amount_State_date,
            zmin=min(Date_fund$Total_amount_perState),
            zmax=max(Date_fund$Total_amount_perState),
            color=~Total_amount_State_date)%>%
  layout(geo=list(scope='usa'),
         title='USA Election 2020\n50 States Funds Raising Trends\n From Aug 30,20 to Sep 8,20')
State_DTD_Plot

##Thank you