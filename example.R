######################################################################
# Code by Matthew A. Tom, version dated 2023-05-15.                  #
# This code uses base R, the plyr and dplyr packages, and all the    #
#  packages that plyr and dplyr need to actually work.               #
######################################################################
Folder1 <- "";
Date20150201 <- as.Date("2015-02-01",format="%Y-%m-%d");
Date20170101 <- as.Date("2017-01-01",format="%Y-%m-%d");
Date20170131 <- as.Date("2017-01-31",format="%Y-%m-%d");
Date29991231 <- as.Date("12/31/2999",format="%m/%d/%Y");
Every25N <- c(0.00,0.25,0.50,0.75,1.00);
Every25T <- c("Q000","Q025","Q050","Q075","Q100");
Skew7N <- c(0.00,0.25,0.50,0.75,0.95,0.99,1.00);
Skew7T <- c("Q000","Q025","Q050","Q075","Q095","Q099","Q100");
alpha <- 0.001;
library(plyr); library(dplyr)
######################################################################
# Read in all the data files.                                        #
######################################################################
# Get the country codes from GitHub                        #
#  and make a few GVC-based corrections.                   #
# GVC uses a few antiquated codes.                         #
# GVC also has an "Undefined" line.                        #
############################################################
Raw <- read.csv(paste0("https://raw.githubusercontent.com/lukes/",
                       "ISO-3166-Countries-with-Regional-Codes/",
                       "master/all/all.csv"),
                stringsAsFactors=FALSE)
Nations <- data.frame(Number=Raw$country.code,NameEN=Raw$name,
                      Char2=Raw$alpha.2,Char3=Raw$alpha.3,
                      stringsAsFactors=FALSE);
   Nations$NameEN[Nations$Number==626] = "East Timor";
   Nations$Char2[Nations$Number==626] = "TP";
   Nations$Char3[Nations$Number==626] = "TMP";
   Nations$Char3[Nations$Number==620] = "POR";
   Nations$Char3[Nations$Number==642] = "ROM";
   Nations$Char3[Nations$Number==756] = "SUI";
   Nations$NameEN[Nations$Number==248] = "Aland Islands";
Nations <- rbind(Nations,
                 data.frame(Number=1000,NameEN="Undefined",
                            Char2="UD",Char3="UND",
                            stringsAsFactors=FALSE));
############################################################
# Read in the demographics file                            #
#                         Demographics.csv. #
# * There are 5028 rows, each with its unique UserID.      #
# * Every player in the other data files has a row in      #
#  this file.                                              #
# * UserID is a number we assign to uniquely identify the  #
#  players across the data tables. The numbers in this     #
#  column are not related to any actual ID numbers found   #
#  in any of Entain's customer databases. Except in the    #
#  name of this variable, we will be using the word        #
#  "player" instead of "user."                             #
# * SystemAgeAsOfReg is the player's age in whole years at #
#  of the date they first registered with Entain and/or    #
#  one of its platforms.                                   #
# * Gender is either "M" for male or "F" for female.       #
# * CountryID is a numeric code representing playersвЂ™      #
#  countries of residence. Most are the ISO 3166-1 codes.  #
#  Exceptions are listed in the R code.                    #
############################################################
BlockC1 <- read.csv(paste0(Folder1,"Demographics.csv"),
                    header=TRUE,stringsAsFactors=FALSE);
   nC1 <- nrow(BlockC1); nC1
   length(unique(BlockC1$UserID))
############################################################
# Read in the poker cash game (a.k.a. ring game) file      #
#                           SecondSeshData2CashGame.csv.   #
# * There are 51763 rows of data.                          #
# * There are 4232 cash game players.                      #
# * UserID is a number we assigned to the players.         #
# * Date is the date of the cash game activity.            #
# * Windows is the number of cash game sessions the player #
#  entered on that date. From GVC: "[This variable]        #
#  captures the [number] of tables that a player is        #
#  playing in a day. Even if [a] player sits out for some  #
#  time it will still be counted as 1. If a player [logs   #
#  off] and comes and [joins] the same table it will be    #
#  counted as 2."                                          #
# * StakesC is the day's Cash Put into Pots. It's the      #
#  total amount of cash (in Euros) the player has put into #
#  pots in cash game hands that day (blinds and antes      #
#  included).                                              #
# * WinningC is the day's Cash Won from Pots. It's the     #
#  total amount of cash (in Euros) the player won in pots  #
#  in cash game hands that day (including uncalled bets or #
#  portions of bets).                                      #
# * BlockR1 has no blank rows with Windows, StakesC, and   #
#   WinningsC all equal to 0.                              #
############################################################
Raw <- read.csv(file=paste0(Folder1,"CashGames.csv"),
                header=TRUE,stringsAsFactors=FALSE);
BlockR1 <- data.frame(UserID=Raw$UserID,
                      Date=as.Date(Raw$Date,format="%Y-%m-%d"),
                      Windows=Raw$Windows,
                      StakesC=Raw$StakesC,
                      WinningsC=Raw$WinningsC,
                      stringsAsFactors=FALSE);
   nR1 <- nrow(BlockR1); nR1
   length(unique(BlockR1$UserID))
Scrap1 <- 1:nR1;
   Scrap1 <- Scrap1[BlockR1$Windows==0];
   Scrap1 <- Scrap1[BlockR1$StakesC[Scrap1]==0];
   Scrap1 <- Scrap1[BlockR1$WinningsC[Scrap1]==0];
   length(Scrap1)
BlockR1 <- BlockR1[order(BlockR1$UserID,BlockR1$Date),];
############################################################
# Read in the poker tournament file                        #
#                          Tournaments.csv. #
# * There are 82831 rows of data.                          #
# * There are 3448 tournament players.                     #
# * UserID is a number we assigned to the players.         #
# * Date is the date of the tournament activity.           #
# * Trnmnts is the number of tournaments the player        #
#  entered with start times during that date.              #
# * StakesT is the total amount spent (in Euros) on        #
#  tournament entry fees and purchases of additional       #
#  tournament chips that day.                              #
# * WinningsT is the total amount won (in Euros) as prizes #
#  in tournaments that started that day (even for          #
#  tournaments that ended the next day or later).          #
# * BlockT1 has no blank rows with Trnmts, StakesT, and    #
#   WinningsT all equal to 0.                              #
############################################################
Raw <- read.csv(file=paste0(Folder1,
                                   "Tournaments.csv"),
                header=TRUE,stringsAsFactors=FALSE);
BlockT1 <- data.frame(UserID=Raw$UserID,
                      Date=as.Date(Raw$Date,
                                   format="%Y-%m-%d"),
                      Trnmnts=Raw$Trnmnts,
                      StakesT=Raw$StakesT,
                      WinningsT=Raw$WinningsT,
                      stringsAsFactors=FALSE);
   nT1 <- nrow(BlockT1); nT1
   length(unique(BlockT1$UserID))
Scrap1 <- 1:nT1;
   Scrap1 <- Scrap1[BlockT1$Trnmnts==0];
   Scrap1 <- Scrap1[BlockT1$StakesT[Scrap1]==0];
   Scrap1 <- Scrap1[BlockT1$WinningsT[Scrap1]==0];
   length(Scrap1)
BlockT1 <- BlockT1[order(BlockT1$UserID,BlockT1$Date),];
############################################################
# Read in the deposit data file                            #
#                             SecondSeshData2Deposits.csv. #
# * BlockD1 has 295119 rows and 5008 unique UserID.        #
# * UserID is a number we assigned to the players.         #
# * DepositID is a number we assigned                      #
#  to each deposit record.                                 #
# * SummaryDate is the date the player entered the         #
#  financial information and attempted the deposit.        #
# * ProcessDate is the date that Entain processed the      #
#  deposit request.                                        #
# * ProcessTime is the [24-hour] time of the day that      #
#  Entain processed the deposit request.                   #
# * PayMeth is the brand of the payment method             #
#  (e.g., MasterCard, Visa, PayPal, Neteller).             #
# * PayMethCat is the payment method's category            #
#  (bank transfer, EEA debit/credit card, eWallet,         #
#  prepaid card, voucher, other cards, or other).          #
# * CardType is either credit, debit, pre-paid card,       #
#  or undetermined. Deposits from pre-paid cards are all   #
#  dated after the end of the study period.                #
# * Amount is the amount of the deposit (in Euros).        #
# * Status is either Completed or Failed. Only completed   #
#  deposits result in money entering the player's gambling #
#  account (and a charge to the player).                   #
############################################################
Raw <- read.csv(paste0(Folder1,"Deposits.csv"),
                header=TRUE,stringsAsFactors=FALSE);
BlockD1 <- data.frame(UserID=Raw$UserID,
                      DepositID=Raw$DepositID,
                      SummaryDate=as.Date(Raw$SummaryDate,
                                          format="%Y-%m-%d"),
                      ProcessDate=as.Date(Raw$ProcessDate,
                                          format="%Y-%m-%d"),
                      ProcessTime=Raw$ProcessTime,
                      PayMeth=Raw$PayMeth,PayMethCat=Raw$PayMethCat,
                      CardType=Raw$CardType,Amount=Raw$Amount,
                      Status=Raw$Status,stringsAsFactors=FALSE);
   nD1 <- nrow(BlockD1); nD1
   length(unique(BlockD1$UserID))
##################################################
# Check to see if any two rows have the same     #
#  UserID, SummaryDate, ProcessTime, PayMeth,    #
#  PayMethCat, CardType, Amount, and Status.     #
# * There are 40 redundant rows.                 #
# * BlockD2 is BlockD1 without the duplicates.   #
# -- BlockD1 has 295088 rows.                    #
# -- BlockD2 has 295048 rows.                    #
##################################################
BlockD1 <- BlockD1[order(BlockD1$UserID,BlockD1$SummaryDate,
                         BlockD1$ProcessDate,BlockD1$ProcessTime,
                         BlockD1$PayMeth,BlockD1$PayMethCat,
                         BlockD1$CardType,
                         BlockD1$Amount,BlockD1$Status),];
   Scrap1 <- 1:(nD1-1);
   Scrap1 <- Scrap1[BlockD1$UserID[Scrap1]==BlockD1$UserID[Scrap1+1]];
   Scrap1 <- Scrap1[BlockD1$SummaryDate[Scrap1]
                                     ==BlockD1$SummaryDate[Scrap1+1]];
   Scrap1 <- Scrap1[BlockD1$ProcessDate[Scrap1]
                                     ==BlockD1$ProcessDate[Scrap1+1]];
   Scrap1 <- Scrap1[BlockD1$ProcessTime[Scrap1]
                                     ==BlockD1$ProcessTime[Scrap1+1]];
   Scrap1 <- Scrap1[BlockD1$PayMeth[Scrap1]
                                         ==BlockD1$PayMeth[Scrap1+1]];
   Scrap1 <- Scrap1[BlockD1$PayMethCat[Scrap1]
                                      ==BlockD1$PayMethCat[Scrap1+1]];
   Scrap1 <- Scrap1[BlockD1$CardType[Scrap1]
                                        ==BlockD1$CardType[Scrap1+1]];
   Scrap1 <- Scrap1[BlockD1$Amount[Scrap1]==BlockD1$Amount[Scrap1+1]];
   Scrap1 <- Scrap1[BlockD1$Status[Scrap1]==BlockD1$Status[Scrap1+1]];
   length(Scrap1)
   sum(BlockD1$ProcessDate[Scrap1]==BlockD1$ProcessDate[Scrap1+1])
Scrap2 <- setdiff(1:nD1,Scrap1);
   BlockD2 <- BlockD1[Scrap2,];
   nD2 <- nrow(BlockD2); nD1 - nD2
   c(nD1,nD2)
##################################################
# BlockD2$CardType has nine possible values.     #
# * "CCd" and "CCD" are for credit cards.        #
# * "DcD" and "DCD" are for debit cards.         #
# * "Und" and "UND" are undetermined.            #
# * "PP " might mean pre-paid, specifically a    #
#   European Economic Area (EEA) debit/credit    #
#   card, either a MasterCard or Visa.           #
# -- Rows with "PP " have dates in 2020.         #
# * "Dd " means a specific kind of EEA           #
#   debit/credit Visa card.                      #
# -- We are guessing that "Dd " should be "DCd"  #
#    and will check on that later.               #
# -- Rows with "Dd " have dates in 2020.         #
# Clean BlockD2$CardType.                        #
# * Set "CCD" to "CCd".                          #
# * Set "DCD" to "DCd".                          #
# * Set "UND" to "Und".                          #
# * Set "Dd " and "" to "Und".                   #
# Clean BlockD2$PayMethCat.                      #
# * Set "ewallet" to "eWallet".                  #
##################################################
Scrap1 <- subset(BlockD2,
                 BlockD2$PayMethCat=="EEA debit/credit card");
   unique(Scrap1$CardType)
   unique(Scrap1$PayMeth)
Scrap1 <- subset(BlockD2,BlockD2$CardType=="Dd ");
   unique(Scrap1$PayMethCat)
   unique(Scrap1$PayMeth)
   c(min(Scrap1$SummaryDate),min(Scrap1$ProcessDate))
Scrap1 <- subset(BlockD2,BlockD2$CardType=="PP ");
   unique(Scrap1$PayMethCat)
   unique(Scrap1$PayMeth)
   c(min(Scrap1$SummaryDate),min(Scrap1$ProcessDate))
Scrap1 <- subset(BlockD2,BlockD2$CardType=="");
   unique(Scrap1$PayMethCat)
   unique(Scrap1$PayMeth)
   c(min(Scrap1$SummaryDate),min(Scrap1$ProcessDate))
BlockD2$CardType[BlockD2$CardType=="CCD"] = "CCd";
BlockD2$CardType[BlockD2$CardType=="DCD"] = "DCd";
BlockD2$CardType[BlockD2$CardType=="Dd "] = "DCd";
BlockD2$CardType[BlockD2$CardType=="UND"] = "Und";
BlockD2$CardType[BlockD2$PayMethCat=="ewallet"] = "eWallet";
##################################################
# Look at the different payment types.           #
# * Insert "Und" into some of the blanks in      #
#   BlockD2$CardType.                            #
##################################################
Scrap1 <- unique(BlockD2[,c("PayMeth","PayMethCat","CardType")])
   Scrap1 <- Scrap1[order(Scrap1$PayMeth),]; Scrap1
#BlockD2$PayMethCat[BlockD2$PayMeth=="Abaqoos"] = "eWallet";
BlockD2$CardType[BlockD2$PayMeth=="ApplePay"] = "Und";
BlockD2$CardType[BlockD2$PayMeth=="Banktransfer"] = "Und";
BlockD2$CardType[BlockD2$PayMeth=="CashToCode"] = "Und";
BlockD2$CardType[BlockD2$PayMeth=="DirectPay24"] = "Und";
BlockD2$CardType[BlockD2$PayMeth=="EcoPayz"] = "Und";
BlockD2$CardType[BlockD2$PayMeth=="EPS"] = "Und";
BlockD2$CardType[BlockD2$PayMeth=="Giropay"] = "Und";
BlockD2$CardType[BlockD2$PayMeth=="GIROPAY_SKRILL"] = "Und";
BlockD2$CardType[BlockD2$PayMeth=="InstantBank"] = "Und";
BlockD2$CardType[BlockD2$PayMeth=="InstantBanking"] = "Und";
BlockD2$CardType[BlockD2$PayMeth=="LuxonPay"] = "Und";
Scrap9 <- (1:nD2)[BlockD2$PayMeth=="MASTERCARD"];
   Scrap9 <- Scrap9[BlockD2$CardType[Scrap9]==""];
   BlockD2$CardType[Scrap9] = "Und";
BlockD2$CardType[BlockD2$PayMeth=="MisterCash"] = "Und";
BlockD2$CardType[BlockD2$PayMeth=="Moneta"] = "Und";
BlockD2$CardType[BlockD2$PayMeth=="Moneybookers"] = "Und";
BlockD2$CardType[BlockD2$PayMeth=="MONEYSAFE"] = "Und";
BlockD2$CardType[BlockD2$PayMeth=="MuchBetter"] = "Und";
BlockD2$CardType[BlockD2$PayMeth=="Neteller"] = "Und";
BlockD2$CardType[BlockD2$PayMeth=="PaysafeCard"] = "Und";
BlockD2$CardType[BlockD2$PayMeth=="PayPal"] = "Und";
BlockD2$CardType[BlockD2$PayMeth=="PaysafeCard"] = "Und";
BlockD2$CardType[BlockD2$PayMeth=="Skrill Rapid Transfer"] = "Und";
BlockD2$CardType[BlockD2$PayMeth=="TicketPremium"] = "Und";
Scrap9 <- (1:nD2)[BlockD2$PayMeth=="VISA"];
   Scrap9 <- Scrap9[BlockD2$CardType[Scrap9]==""];
   BlockD2$CardType[Scrap9] = "Und";
##################################################
# Someone could have two rows with the same      #
#  payment processor, with one row having a      #
#  CardType of "Und" and the other having a      #
#  CardType of either "CCd" or "DCd".            #
# * For now, we'll count <PayMeth>/CCd and       #
#   <PayMeth>/Und or <PayMeth>/DCd and           #
#   <PayMeth>/Und as two different methods for   #
#   depositing.                                  #
# * That could only happen with EntroPay,        #
#   Maestro, MASTERCARD, and VISA.               #
# -- EntroPay was either CCd or DCd.             #
# @@@ Each EntroPay user deposited with an       #
#     EntroPay credit card or an EntroPay debit  #
#     card, but not both.                        #
# -- Maestro cards could be CCd, DCd, or Und.    #
# @@@ There are 2 users with                     #
#     both MAESTRO/CCd and MAESTRO/DCd.          #
# @@@ There are 0 users with                     #
#     only MAESTRO/CCd and MAESTRO/Und.          #
# @@@ There are 5 users with                     #
#     both MAESTRO/DCd and MAESTRO/Und.          #
# @@@ There are 0 users with all three.          #
# -- MASTERCARD could be any of the three.       #
# @@@ There are 60 users with both               #
#     MASTERCARD/CCd and MASTERCARD/DCd.         #
# @@@ There are 85 users with both               #
#     MASTERCARD/CCd and MASTERCARD/Und.         #
# @@@ There are 36 users with both               #
#     MASTERCARD/DCd and MASTERCARD/Und.         #
# @@@ There are 23 users in the overlap of the   #
#     three intersections.                       #
# -- VISA could be any of the three.             #
# @@@ There are 173 users with both              #
#     VISA/CCd and VISA/CCd.                     #
# @@@ There are  57 users with both              #
#     VISA/CCd and VISA/Und.                     #
# @@@ There are  71 users with both              #
#     VISA/DCd and VISA/Und.                     #
# @@@ There are  30 users in the overlap of the  #
#     three intersections.                       #  
##################################################
Scrap1 <- subset(BlockD2,BlockD2$PayMeth=="EntroPay");
   Scrap2 <- unique(subset(Scrap1$UserID,
                           "CCd"==Scrap1$CardType));
   Scrap3 <- unique(subset(Scrap1$UserID,
                           "DCd"==Scrap1$CardType));
   intersect(Scrap2,Scrap3)
Scrap1 <- subset(BlockD2,BlockD2$PayMeth=="MAESTRO");
   Scrap2 <- unique(subset(Scrap1$UserID,
                           "CCd"==Scrap1$CardType));
   Scrap3 <- unique(subset(Scrap1$UserID,
                           "DCd"==Scrap1$CardType));
   Scrap4 <- unique(subset(Scrap1$UserID,
                           "Und"==Scrap1$CardType));
   length(intersect(Scrap2,Scrap3))
   intersect(Scrap2,Scrap4)
   length(intersect(Scrap3,Scrap4))
   intersect(Scrap2,intersect(Scrap3,Scrap4))
Scrap1 <- subset(BlockD2,BlockD2$PayMeth=="MASTERCARD");
   Scrap2 <- unique(subset(Scrap1$UserID,
                           "CCd"==Scrap1$CardType));
   Scrap3 <- unique(subset(Scrap1$UserID,
                           "DCd"==Scrap1$CardType));
   Scrap4 <- unique(subset(Scrap1$UserID,
                           "Und"==Scrap1$CardType));
   length(intersect(Scrap2,Scrap3))
   length(intersect(Scrap2,Scrap4))
   length(intersect(Scrap3,Scrap4))
   length(intersect(Scrap2,intersect(Scrap3,Scrap4)))
Scrap1 <- subset(BlockD2,BlockD2$PayMeth=="VISA");
   Scrap2 <- unique(subset(Scrap1$UserID,
                           "CCd"==Scrap1$CardType));
   Scrap3 <- unique(subset(Scrap1$UserID,
                           "DCd"==Scrap1$CardType));
   Scrap4 <- unique(subset(Scrap1$UserID,
                           "Und"==Scrap1$CardType));
   length(intersect(Scrap2,Scrap3))
   length(intersect(Scrap2,Scrap4))
   length(intersect(Scrap3,Scrap4))
   length(intersect(Scrap2,intersect(Scrap3,Scrap4)))
############################################################
# Read in the withdrawal data file                         #
#                         ff_harvard_user_withdrawals.csv. #
# * BlockD1 has 32307 rows and 1827 unique UserID.         #
# * UserID is a number we assigned to the players.         #
# * WithdrawalID is a number we assigned                   #
#  to each withdrawal record.                              #
# * SummaryDate is the date the player entered the         #
#  financial information and attempted the withdrawal.     #
# * ProcessDate is the date that Entain processed the      #
#  withdrawal request.                                     #
# * ProcessTime is the [24-hour] time of the day that      #
#  Entain processed the withdrawal request.                #
# * PayMeth is the brand of the payment method             #
#  (e.g., MasterCard, Visa, PayPal, Neteller).             #
# * PayMethCat is the payment method's category            #
#  (bank transfer, EEA debit/credit card, eWallet,         #
#  prepaid card, voucher, other card, or other).           #
# * CardType is either credit, debit, pre-paid card,       #
#  or undetermined. Deposits from pre-paid cards are all   #
#  dated after the end of the study period.                #
# * Amount is the amount of the withdrawal (in Euros).     #
# * Status is either Completed or Reversed. Only completed #
#  withdrawals result in money exiting the player's        #
#  gambling account (and the player getting paid).         #
############################################################
Raw <- read.csv(paste0(Folder1,"Withdrawals.csv"),
                header=TRUE,stringsAsFactors=FALSE);
BlockW1 <- data.frame(UserID=Raw$UserID,
                      WithdrawalID=Raw$WithdrawalID,
                      SummaryDate=as.Date(Raw$SummaryDate,
                                          format="%Y-%m-%d"),
                      ProcessDate=as.Date(Raw$ProcessDate,
                                          format="%Y-%m-%d"),
                      ProcessTime=Raw$ProcessTime,
                      PayMeth=Raw$PayMeth,
                      PayMethCat=Raw$PayMethCat,
                      CardType=Raw$CardType,
                      Amount=Raw$Amount,
                      Status=Raw$Status,
                      stringsAsFactors=FALSE);
   nW1 <- nrow(BlockW1); nW1
   length(unique(BlockW1$UserID))
##################################################
# Check to see if any two rows have the same     #
#  UserID, SummaryDate, ProcessTime, PayMeth,    #
#  PayMethCat, CardType, Amount, and Status.     #
# * There are 3 redundant rows.                  #
# * BlockW2 is BlockW1 without the duplicates.   #
# -- BlockW1 has 32307 rows.                     #
# -- BlockW2 has 32304 rows.                     #
##################################################
BlockW1 <- BlockW1[order(BlockW1$UserID,BlockW1$SummaryDate,
                         BlockW1$ProcessDate,BlockW1$ProcessTime,
                         BlockW1$PayMeth,BlockW1$PayMethCat,
                         BlockW1$Amount,BlockW1$Status),];
   Scrap1 <- 1:(nW1-1);
   Scrap1 <- Scrap1[BlockW1$UserID[Scrap1]==BlockW1$UserID[Scrap1+1]];
   Scrap1 <- Scrap1[BlockW1$SummaryDate[Scrap1]
                                     ==BlockW1$SummaryDate[Scrap1+1]];
   Scrap1 <- Scrap1[BlockW1$ProcessTime[Scrap1]
                                     ==BlockW1$ProcessTime[Scrap1+1]];
   Scrap1 <- Scrap1[BlockW1$PayMeth[Scrap1]
                                         ==BlockW1$PayMeth[Scrap1+1]];
   Scrap1 <- Scrap1[BlockW1$PayMethCat[Scrap1]
                                      ==BlockW1$PayMethCat[Scrap1+1]];
   Scrap1 <- Scrap1[BlockW1$Amount[Scrap1]==BlockW1$Amount[Scrap1+1]];
   Scrap1 <- Scrap1[BlockW1$Status[Scrap1]==BlockW1$Status[Scrap1+1]];
   length(Scrap1)
   sum(BlockW1$ProcessDate[Scrap1]==BlockW1$ProcessDate[Scrap1+1])
Scrap2 <- setdiff(1:nW1,Scrap1);
   BlockW2 <- BlockW1[Scrap2,];
   nW2 <- nrow(BlockW2); nW1 - nW2
   c(nW1,nW2)
############################################################
# The first time we used this function for calculating the #
#  effect size for a Wilcoxon rank-sum test was Tom et     #
#  al.'s "Understanding the Relation Between Social        #
#  Behaviors..."                                           #
# The first time we used this function for comparing two   #
#  Spearman correlations is Edson et al.'s 2021 paper      #
#  studying online casino play.                            #
############################################################
rFromWilcox <- function(wilcoxModel,N) {
   z <- qnorm(wilcoxModel$p.value/2);
   r <- -z/sqrt(N);
   #cat(wilcoxModel$data.name,", Effect Size, r = ",r,"\n");
   rFromWilcox = r;
}
CompareSpearmans <- function(Corr1,Corr2,n1,n2) {
  Z1 <- 0.5 * log((1+Corr1)/(1-Corr1));
  Z2 <- 0.5 * log((1+Corr2)/(1-Corr2));
  VarZ1 <- 1.06 / (n1-3);
  VarZ2 <- 1.06 / (n2-3);
  StErrDiff <- sqrt(VarZ1+VarZ2); 
  ZDiff <- (Z1-Z2) / StErrDiff;
  pValue <- 2 * pnorm(-abs(ZDiff));
  return(pValue);
}
######################################################################
# Make the structures for Second Session at the Virtual Poker Table. #
# For the Block numbering scheme...                                  #
# * The 1 is for the data in the Transparency Project package.       #
# * The 2 is the data after removing blank rows.                     #
# * The 3 is the data specifically for poker data from during the    #
#   during the study period (2015-02-01 to 2017-01-31).              #
# * The 4 is the data for the analytic roster.                       #
# -- We'll restrict the analytic roster to those in BlockR3 and      #
#   BlockT3.                                                         #
# -- They have to have at least four active poker-playing days       #
#   (cash game, tournament, or both).                                #
# -- They have to have at least one active day before 2017-01-01.    #
######################################################################
# Create the Block*3 files (poker data between             #
#  2015-02-01 and 2017-01-31).                             #
# * For the cash game and tournament data, Date has to be  #
#  inside the study period.                                #
# * For the deposit and withdrawal data, SummaryDate has   #
#  to be on or after 2015-02-01 and ProcessDate has to be  #
#  on or before 2017-01-31.                                #
# * In this part of the code for the original paper, we    #
#  mistakenly used BlockD1 and BlockW1 instead of BlockD2  #
#  and BlockW2. It caused some slight inaccuracies, but    #
#  nothing that materially changed the reported results or #
#  our resulting conclusions.                              #
############################################################
Scrap1 <- 1:nR1;
   Scrap1 <- Scrap1[Date20150201<=BlockR1$Date[Scrap1]];
   Scrap1 <- Scrap1[BlockR1$Date[Scrap1]<=Date20170131];
   BlockR3 <- BlockR1[Scrap1,];
Scrap1 <- 1:nT1;
   Scrap1 <- Scrap1[Date20150201<=BlockT1$Date[Scrap1]];
   Scrap1 <- Scrap1[BlockT1$Date[Scrap1]<=Date20170131];
   BlockT3 <- BlockT1[Scrap1,];
Scrap1 <- 1:nD2;
   Scrap1 <- Scrap1[Date20150201<=BlockD2$SummaryDate[Scrap1]];
   Scrap1 <- Scrap1[BlockD2$ProcessDate[Scrap1]<=Date20170131];
   BlockD3 <- BlockD2[Scrap1,];
Scrap1 <- 1:nW2;
   Scrap1 <- Scrap1[Date20150201<=BlockW2$SummaryDate[Scrap1]];
   Scrap1 <- Scrap1[BlockW2$ProcessDate[Scrap1]<=Date20170131];
   BlockW3 <- BlockW2[Scrap1,];
############################################################
# Construct the roster for the analytic sample.            #
# * We start with the 4667 players between BlockR3 and     #
#   BlockT3.                                               #
# * We keep the 2517 players who had at least four         #
#   active days (removing 2160).                           #
# * We keep the 2504 players who has at least one active   #
#   day before 2017-01-01 (removing 13).                   #
############################################################
# BlockR4 and BlockT4 contain the poker data     #
#   for the players on the analytic roster.      #
##################################################
Scrap1 <- merge(BlockR3,BlockT3,
                all.x=TRUE,all.y=TRUE,by=c("UserID","Date"));
   Scrap1[is.na(Scrap1)] = 0;
Scrap2 <- ddply(Scrap1,~UserID,summarise,
                Day1=min(Date),DayL=max(Date),
                ActiveDays=length(Date));
   Scrap3 <- 1:nrow(Scrap2); length(Scrap3)
      Scrap4 <- c(nrow(Scrap2),0,0);
   Scrap3 <- Scrap3[Scrap2$ActiveDays[Scrap3]>=4];
      Scrap4[2] = length(Scrap3);
   Scrap3 <- Scrap3[Scrap2$Day1[Scrap3]<Date20170101];
      Scrap4[3] = length(Scrap3)
   Scrap5 <- c(Scrap4[1],Scrap4[1]-Scrap4[2],
               Scrap4[2],Scrap4[2]-Scrap4[3],Scrap4[3]); Scrap5
Roster1 <- data.frame(UserID=Scrap2$UserID[Scrap3],
                      Keep=1,
                      Day1=Scrap2$Day1[Scrap3],
                      DayL=Scrap2$DayL[Scrap3],
                      ActiveDays=Scrap2$ActiveDays[Scrap3],
                      stringsAsFactors=FALSE);
   Roster1 <- Roster1[order(Roster1$UserID),];
   nS5 <- nrow(Roster1); nS5
BlockR4 <- merge(BlockR3,Roster1[,c("UserID","Keep")],
                 all.x=FALSE,all.y=FALSE,by=c("UserID"));
BlockT4 <- merge(BlockT3,Roster1[,c("UserID","Keep")],
                 all.x=FALSE,all.y=FALSE,by=c("UserID"));
##################################################
# BlockC5 has the demographic data               #
#  for the analytic sample.                      #
# * Entain did not have a country of residence   #
#   listed for four of the players in the        #
#   analytic sample.                             #
# * Everyone has a listed age and gender.        #
##################################################
BlockC5 <- merge(BlockC1[,c("UserID","SystemAgeAsOfReg",
                            "Gender","CountryID")],
                 Roster1[,c("UserID","Keep")],
                 all.x=FALSE,all.y=FALSE,by=c("UserID"));
   sum(is.na(BlockC5$SystemAgeAsOfReg))
   sum(is.na(BlockC5$Gender))
   sum(is.na(BlockC5$CountryID))
   BlockC5$CountryID[is.na(BlockC5$CountryID)] = 1000;
   BlockC5 <- BlockC5[,c("SystemAgeAsOfReg","Gender",
                         "CountryID","UserID")];
   BlockC5 <- BlockC5[order(BlockC5$UserID),];
##################################################
# BlockD4 and BlockW4 contain the financial      #
#  records for the analytic sample.              #
# * We are merging BlockD4$CardType into         #
#   BlockD4$PayMeth so that it will be easier to #
#   count the number of unique payment types.    #
# * For example, someone might be using a Visa   #
#   credit card from one service, and a Visa     #
#   debit card through another. In that case, we #
#   will count as two separate payment methods.  #
##################################################
BlockD4 <- merge(BlockD3,Roster1[,c("UserID","Keep")],
                 all.x=FALSE,all.y=FALSE,by=c("UserID"));
BlockD4$PayMeth <- paste0(BlockD4$PayMeth,BlockD4$CardType);
BlockD4 <- BlockD4[,c("UserID","DepositID","SummaryDate",
                      "ProcessDate","ProcessTime","PayMeth",
                      "PayMethCat","CardType","Amount","Status")];
BlockW4 <- merge(BlockW3,Roster1[,c("UserID","Keep")],
                 all.x=FALSE,all.y=FALSE,by=c("UserID"));
BlockW4 <- BlockW4[,c("UserID","WithdrawalID","SummaryDate",
                      "ProcessDate","ProcessTime",
                      "PayMeth","PayMethCat","CardType",
                      "Amount","Status")];
############################################################
# Generate the aggregates covering the whole study period. #
############################################################
# BlockR5 contains the ring game aggregates.     #
# * Day1C is the date of the player's first cash #
#   game hand. In the pre-registration, this is  #
#   First Cash Game Date Stamp.                  #
# * DayLC is the date of the player's last cash  #
#   game hand. In the pre-registration, this is  #
#   Last Cash Game Date Stamp.                   #
# * Windows is the number of cash game tables.   #
#   In the pre-registration, this is Number of   #
#   Cash Games.                                  #
# * StakesC is the total amount of cash the      #
#   player has put into pots in cash games       #
#   hands. In the transparent change, this is    #
#   Total Cash Put into Pots.                    #
# * WinningsC is the total amount of cash the    #
#   player won from pots in cash game hands. In  #
#   the transparent chagne, this is Total Cash   #
#   Won from Pots.                               #
# * ProfitC is WinningsC minus StakesC. In the   #
#   pre-registration, this is Total Cash Game    #
#   Plus/Minus.                                  #
##################################################
BlockR5 <- ddply(BlockR4,~UserID,summarise,
                 Day1R=min(Date),DayLR=max(Date),
                 Windows=sum(Windows),StakesC=sum(StakesC),
                 WinningsC=sum(WinningsC));
   BlockR5$ProfitC <- BlockR5$WinningsC - BlockR5$StakesC;
##################################################
# BlockT5 contains the tournament aggregates.    #
# * Day1T is the date of the player's first      #
#   tournament. In the pre-registration, this is #
#   First Tournament Date Stamp.                 #
# * DayLT is the date of the player's last       #
#   Tournament. In the pre-registration, this is #
#   Last Tournament Date Stamp.                  #
# * Trnmnts is in the pre-registration as Number #
#   of Tournaments.                              #
# * StakesT is the total amount of cash the      #
#   player has paid in tournament fees. In the   #
#   pre-registration, this is Total Tournament   #
#   Spend.                                       #
# * WinningsT is the total amount of cash the    #
#   player has won in tournament prizes. In the  #
#   transparent chagne, this is Total Tournament #
#   Prizes.                                      #
# * ProfitT is WinningsT minus StakesT. In the   #
#   pre-registration, this is Total Tournament   #
#   Plus/Minus.                                  #
##################################################
BlockT5 <- ddply(BlockT4,~UserID,summarise,
                 Day1T=min(Date),DayLT=max(Date),
                 Trnmnts=sum(Trnmnts),StakesT=sum(StakesT),
                 WinningsT=sum(WinningsT));
   BlockT5$ProfitT <- BlockT5$WinningsT - BlockT5$StakesT;
##################################################
# BlockS4 contains the poker study aggregates.   #
# * UserID is the player's user ID.              #
# * Day1 is the player's first active day. In    #
#   the pre-registration, this is First Overall  #
#   Date Stamp.                                  #
# * DayL is the player's last active day. In the #
#   pre-registration, this is Last Overall Date  #
#   Stamp.                                       #
# * ActiveDays is the number of active days.     #
# * Sesh is BlockR5$Windows plus                 #
#   BlockT5$Trnmnts. In the pre-registration,    #
#   this is Total Sessions.                      #
# * Duration is the number of days between Last  #
#   Overall Date Stamp and First Overall Date    #
#   Stamp, inclusive. In the pre-registration,   #
#   this is Overall Duration.                    #
# -- It's 1 + DayL - Day1.                       #
# * SeshPD is Sesh divided by Duration. In the   #
#   pre-registration, this is Sessions per Day.  #
# * StakesO is StakesC plus StakesT. In the      #
#   pre-registration, this is Total Overall      #
#   Spend.                                       #
# * StksPS is StakesO divided by Sesh. In the    #
#   pre-registration, this is Average Spend per  #
#   Session.                                     #
# * ProfitO is ProfitC plus ProfitT. In the      #
#   pre-registration, this is Total Overall      #
#   Plus/Minus.                                  #
# * NetLoss is Profit0 times -1.                 #
# * PctProf is 100 times ProfitO divided by      #
#   StakesO. In the pre-registration, this is    #
#   Overall Percent Won.                         #
# * PctLost is PctProf times -1. In the          #
#   pre-registration, this is Percent Lost.      #
##################################################
BlockS4 <- merge(BlockR5,BlockT5,
                by=c("UserID"),all.x=TRUE,all.y=TRUE);
   Scrap2 <- (1:nS5)[is.na(BlockS4$Day1R)];
      BlockS4$Day1R[Scrap2] = as.Date("2999-12-31");
      BlockS4$DayLR[Scrap2] = as.Date("1000-01-01");
      BlockS4$Windows[Scrap2] = 0;
      BlockS4$StakesC[Scrap2] = 0;
      BlockS4$WinningsC[Scrap2] = 0;
      BlockS4$ProfitC[Scrap2] = 0;
   Scrap2 <- (1:nS5)[is.na(BlockS4$Day1T)];
      BlockS4$Day1T[Scrap2] = as.Date("2999-12-31");
      BlockS4$DayLT[Scrap2] = as.Date("1000-01-01");
      BlockS4$Trnmnts[Scrap2] = 0;
      BlockS4$StakesT[Scrap2] = 0;
      BlockS4$WinningsT[Scrap2] = 0;
      BlockS4$ProfitT[Scrap2] = 0;
   BlockS4$Sesh = BlockS4$Windows + BlockS4$Trnmnts;
   BlockS4$StakesO = BlockS4$StakesC + BlockS4$StakesT;
   BlockS4$ProfitO = BlockS4$ProfitC + BlockS4$ProfitT;
   BlockS4$NetLoss = -BlockS4$ProfitO;
   BlockS4$PctProf = 100 * BlockS4$ProfitO / BlockS4$StakesO;
   BlockS4$PctLoss = -BlockS4$PctProf;
   BlockS4 <- BlockS4[order(BlockS4$UserID),];
nS5 - sum(BlockS4$UserID==Roster1$UserID)
   BlockS4$Day1 = Roster1$Day1;
   BlockS4$DayL = Roster1$DayL;
   BlockS4$Duration = 1 + as.numeric(difftime(
                              BlockS4$DayL,BlockS4$Day1,unit="days"));
   BlockS4$SeshPD = BlockS4$Sesh / BlockS4$Duration;
   BlockS4$StksPS = BlockS4$StakesO / BlockS4$Sesh;
   BlockS4 <- BlockS4[order(BlockS4$UserID),];
##################################################
# BlockS5 contains the variables we will use for #
#  the analysis of poker activity.               #
# * That's Duration, Sesh, SeshPD, StakesO,      #
#  StksPS, NetLoss, and PctLoss.                 #
# * The codebook for BlockS5 is the part of      #
#  Table 1 (Measures of poker activity).         #
# * We'll be comparing the distrbutions of men   #
#  and women, so we'll add the variable Gender.  #
# * RankStO ranks the players by StakesO, where  #
#  1 denotes the person with the highest         #
#  StakesO and nS5 = 2504 denotes the            #
#  person with the lowest StakesO.               #
# * BinStO puts players into percentile bins by  #
#  StakesO. Bin 1 has quantiles between 0.00     #
#  and 0.01, Bin 2 has quantiles between 0.01    #
#  and 0.02, etc.                                #
##################################################
sum(BlockS4$UserID==BlockC5$UserID)
BlockS5 <- data.frame(Duration=BlockS4$Duration,
                      Sesh=BlockS4$Sesh,
                      SeshPD=BlockS4$SeshPD,
                      StakesO=BlockS4$StakesO,
                      StksPS=BlockS4$StksPS,
                      NetLoss=BlockS4$NetLoss,
                      PctLoss=BlockS4$PctLoss,
                      UserID=BlockS4$UserID,
                      Gender=BlockC5$Gender,
                      stringsAsFactors=FALSE);
   BlockS5 <- BlockS5[order(-BlockS5$StakesO),];
   BlockS5$RankStO = 1:nS5;
   BlockS5$BinStO <- ceiling(100*(nS5+1-BlockS5$RankStO)/nS5);
   BlockS5 <- BlockS5[order(BlockS5$UserID),];
LabelsS <- c("Duration",
             "Number of Sessions",
             "Sessions Per Day",
             "Total Overall Spend",
             "Average Spend per Session",
             "Net Loss",
             "Percent Loss");
##################################################
# In the transparent change, we said that we'd   #
#  make a BlockF5 with the financial data.       #
# * Instead, we're splitting BlockF5 into a      #
#  BlockD5 for the deposit data and a BlockW5    #
#  for the withdrawal data.                      #
# BlockD5 has the deposit aggregates.            #
# * nAtmpDep is the Number of Attempted          #
#  Deposits.                                     #
# * nCompDep is Number of Completed Deposits.    #
# * nFailDep = nAtmpDep - nCompDep is Number of  #
#  Failed Deposits.                              #
# * PctFailDep is Percentage of Failed Deposits, #
#  the percentage of attempted deposits that     #
#  failed.                                       #
# * nMeth is Number of Unique Payment Methods,   #
#  the number of different PayMeth.              #
# * nCCd is the Number of Unique Credit Cards    #
#  [Used for Deposits], the number of credit     #
#  card types.                                   #
# -- The maximum is 4 (i.e., someone who showed  #
#   an EntroPay, a MAESTRO, a MASTERCARD, and a  #
#   VISA).                                       #
# * nDepDays is Number of Deposit Days, the      #
#  number of unique days with a completed        #
#  deposit.                                      #
# * DepPerDay = nCompDep/nDepDays is Deposits    #
#  per Deposit Day, the number of successful     #
#  deposits per deposit day.                     #
# * AmtDep is Total Deposit Amount, the total    #
#  amount in euros over all successful           #
#  deposits.                                     #
# * AmtPerDay = AmtDep/nDepDays is Average       #
#  Amount Deposited per Deposit Day.             #
# * Out of the 2504 on the analytic roster, 2497 #
#  had least one completed deposit.              #
# -- One user had only failed deposits           #
#   (fifteen of them).                           #
# -- Six users had no deposits whatsoever.       #
# -- Since everyone left has at least one        #
#   completed deposit, nDepDays is always        #
#   greater than 0 and we don't have to worry    #
#   about dividing by 0 when calculating         #
#   DepPerDay and AmtPerDay.                     #
# * The codebook for BlockD5 will be Table 3.    #
#  Measures of deposit activity.                 #
##################################################
BlockD5 <- ddply(BlockD4,~UserID,summarise,
                 nAtmpDep=length(PayMeth),
                 nMeth=length(unique(PayMeth)));
Scrap1 <- subset(BlockD4,BlockD4$Status=="S");
   Scrap2 <- ddply(Scrap1,~UserID,summarise,
                   nCompDep=length(PayMeth),
                   AmtDep=sum(Amount),
                   nDepDays=length(unique(SummaryDate)));
   BlockD5 <- merge(BlockD5,Scrap2,all.x=TRUE,all.y=TRUE,
                    by=c("UserID"));
Scrap1 <- subset(BlockD4,BlockD4$CardType=="CCd");
   Scrap2 <- ddply(Scrap1,~UserID,summarise,
                   nCCd=length(unique(PayMeth)));
   BlockD5 <- merge(BlockD5,Scrap2,all.x=TRUE,all.y=TRUE,
                    by=c("UserID"));
BlockD5[is.na(BlockD5)] = 0;
   BlockD5$nFailDep = BlockD5$nAtmpDep - BlockD5$nCompDep;
   BlockD5$PctFailDep = 100 * BlockD5$nFailDep / BlockD5$nAtmpDep;
   BlockD5$DepPerDay = BlockD5$nCompDep / BlockD5$nDepDays;
   BlockD5$AmtPerDay = BlockD5$AmtDep / BlockD5$nDepDays;
   c(nS5,nrow(BlockD5),nS5-nrow(BlockD5),
     sum(BlockD5$nCompDep>0),sum(BlockD5$nCompDep==0))
   subset(BlockD5,BlockD5$nCompDep==0)[,1:6]
   # UserID nAtmpDep nMeth nCompDep AmtDep nDepDays
   #  42073       15     4        0      0        0
   subset(BlockD5,BlockD5$nCompDep==0)[,c(7:11)]
   # UserID nCCd nFailDep PctFailDep DepPerDay AmtPerDay
   #  42073    0       15        100       NaN       NaN
   BlockD5 <- subset(BlockD5,BlockD5$nCompDep>0);
BlockD5 <- BlockD5[,c("nAtmpDep","nFailDep","nCompDep","PctFailDep",
                      "AmtDep","nDepDays","DepPerDay","AmtPerDay",
                      "nMeth","nCCd","UserID")];
   nD5 <- nrow(BlockD5);
LabelsD <- c("Number of Attempted Deposits",
             "Number of Failed Deposits",
             "Number of Completed Deposits",
             "Percent of Failed Deposits",
             "Total Deposit Amount",
             "Number of Deposit Days",
             "Deposits per Deposit Day",
             "Average Amount Deposited per Deposit Day (euros/day)",
             "Number of Unique Deposit Methods",
             "Number of Unique Credit Cards");
##################################################
# BlockW5 has the withdrawal aggregates.         #
# * nAtmpWth is the Number of Attempted          #
#  Withdrawals.                                  #
# * nCompWth is the Number of Completed          #
#  Withdrawals.                                  #
# * nRvrsWth = nAtmpWth - nCompWth is the Number #
#  of Reversed Withdrawals.                      #
# * PctRvrsWth is Percentage of Reversed         #
#  Withdrawals, the percentage of attempted      #
#  withdrawals that were reversed.               #
# * nWthDays is Number of Withdrawal Days, the   #
#  number of unique days with a completed        #
#  withdrawal.                                   #
# * WthPerDay = nCompWth/nWthDays is Withdrawals #
#  per Withdrawal Day, the number of successful  #
#  deposits per deposit day.                     #
# * AmtWth is Total Withdrawal Amount, the total #
#  amount in euros over all successful           #
#  deposits.                                     #
# * AmtPerDay = AmtDep/nDepDays is Average       #
#  Amount Withdrawn per Withdrawal Day.          #
# * Out of the 2504 on the analytic roster, 833  #
#  had least one completed withdrawal.           #
# -- There were 876 with at least one attempted  #
#   withdrawal.                                  #
# -- There were 43 with only failed withdrawals. #
# * The codebook for BlockW5 is the last block   #
#  of Table 1 (Measures of withdrawal activity). #
##################################################
BlockW5 <- ddply(BlockW4,~UserID,summarise,
                 nAtmpWth=length(PayMeth));
Scrap1 <- subset(BlockW4,BlockW4$Status=="S");
   Scrap2 <- ddply(Scrap1,~UserID,summarise,
                   nCompWth=length(PayMeth),
                   AmtWth=sum(Amount),
                   nWthDays=length(unique(SummaryDate)));
   BlockW5 <- merge(BlockW5,Scrap2,all.x=TRUE,all.y=TRUE,
                    by=c("UserID"));
   BlockW5[is.na(BlockW5)] = 0;
   BlockW5$nRvrsWth = BlockW5$nAtmpWth - BlockW5$nCompWth;
   BlockW5$PctRvrsWth = 100 * BlockW5$nRvrsWth / BlockW5$nAtmpWth;
   BlockW5$WthPerDay = BlockW5$nCompWth / BlockW5$nWthDays;
   BlockW5$AmtPerDay = BlockW5$AmtWth / BlockW5$nWthDays;
   BlockW5[is.na(BlockW5)] = 0;
   c(nS5,nrow(BlockW5),nS5-nrow(BlockW5),
     sum(BlockW5$nCompWth>0),sum(BlockW5$nCompWth==0))
   BlockW5 <- subset(BlockW5,BlockW5$nCompWth>0);
BlockW5 <- BlockW5[,c("nAtmpWth","nRvrsWth","nCompWth","PctRvrsWth",
                      "AmtWth","nWthDays","WthPerDay","AmtPerDay",
                      "UserID")];
   nW5 <- nrow(BlockW5);
LabelsW <- c("Number of Attempted Withdrawals",
             "Number of Reversed Withdrawals",
             "Number of Completed Withdrawals",
             "Percent of Reversed Withdrawals",
             "Total Amount Withdrawn",
             "Number of Withdrawal Days",
             "Withdrawals per Withdrawal Day",
             "Average Amount Withdrawn per Withdrawal Day");
######################################################################
# Create a new set of data tables Block*6, where we make the         #
#  following adjustments based on the second transparent change.     #
# * In order to be in the analytic sample, StakesO has to be greater #
#  than 0. A player has to either put money into a pot in a cash     #
#  game (StakesC > 0) or play in a tournament with an entry fee      #
#  above 0 (StakesT > 0).                                            #
# * In order to be in the analytic sample, a player must have at     #
#  least one completed deposit during the study period.              #
# -- We already applied this criterion to BlockD5. We have to apply  #
#   it to BlockS5 and BlockW5.                                       #
# * If someone has at least one reversed withdrawal but no completed #
#  withdrawals, we include them in the withdrawal data set but hard  #
#  code their values of BlockW5$WthPerDay and BlockW5$AmtPerDay to   #
#  be equal to 0.                                                    #
######################################################################
# Obtain a new Roster2 of players that are in Roster1 and  #
#  satisfy the new criteria.                               #
# * Eight people had BlockS5$StakesO equal to 0.           #
# * Seven of the remaining 2496 had no completed deposits. #
# * That left nS6 = 2489 players.                          #
# Since BlockS6 has fewer players, we have to recalculate  #
#  RankStO, BinStO, and Top.                               #
# * Losing 15 players from the bottom end of the StakesO   #
#  distribution didn't change anything. It's still the     #
#  top 1% versus everyone else.                            #
# * These same 15 players are the difference between       #
#  BlockC5 and BlockC6.                                    #
# * Block 4 of the analyses below will include Table 9:    #
#  Data for the top six centile groups by Total Overall    #
#  Spend.                                                  #
# The difference between BlockD5 and BlockD6 is the eight  #
#  people who only played freeroll tournaments.            #
# * BlockD6 has 2489 players.                              #
# We have to calculate BlockW6 from BlockW4, since we are  #
#  actually adding back people who had only unsuccessful   #
#  withdrawals.                                            #
# * BlockW6 has 876 players, 43 with no completed          #
#  withdrawals.                                            #
############################################################
Scrap1 <- subset(BlockS5$UserID,BlockS5$StakesO>0);
Scrap2 <- subset(BlockD5$UserID,BlockD5$nCompDep>0);
Roster2 <- intersect(Scrap1,Scrap2);
Scrap3 <- data.frame(UserID=Roster2,Keep=1);
BlockS6 <- merge(BlockS5,Scrap3,all.x=FALSE,
                 all.y=FALSE,by=c("UserID"));
   nS6 <- nrow(BlockS6);
   BlockS6 <- BlockS6[order(-BlockS6$StakesO),];
   BlockS6$RankStO <- 1:nS6;
   BlockS6$BinStO <- ceiling(100*(nS6+1-BlockS6$RankStO)/nS6);
   Table05 <- ddply(BlockS6,~BinStO,summarise,
                    nBin=length(StakesO),
                    minStO=min(StakesO),maxStO=max(StakesO),
                    SumStO=sum(StakesO));
      Table05 <- Table05[order(Table05$BinStO),];
   Table05[95:100,]
   # BinStO nBin    minStO     maxStO     SumStO
   #     95   25  15605.97   19825.44   430097.3
   #     96   25  19870.41   27286.54   568337.1
   #     97   25  27329.15   41772.04   847935.7
   #     98   25  41797.70   70113.45  1358184.9
   #     99   25  72802.37  152036.53  2360943.7
   #    100   25 154083.30 3742545.01 15245175.6
   BlockS6$Top <- (BlockS6$BinStO==100);
   plot(x=Table05$BinStO,y=Table05$SumStO,pch=18,
        xlab="Percentile Group",ylab="Group's Total Overall Spend")
   BlockS6 <- BlockS6[,c("Duration","Sesh","SeshPD","StakesO",
                         "StksPS","NetLoss","PctLoss",
                         "UserID","Gender",
                         "RankStO","BinStO","Top")];
   BlockS6 <- BlockS6[order(BlockS6$UserID),];
BlockC6 <- merge(BlockC5,Scrap3,all.x=FALSE,
                 all.y=FALSE,by=c("UserID"));
   nC6 <- nrow(BlockC6); c(nS5,nS5-nC6,nC6)
   BlockC6 <- BlockC6[,c("SystemAgeAsOfReg","Gender",
                         "CountryID","UserID")];
   BlockC6 <- BlockC6[order(BlockC6$UserID),];
BlockD6 <- merge(BlockD5,Scrap3,all.x=FALSE,
                 all.y=FALSE,by=c("UserID"));
   nD6 <- nrow(BlockD6); c(nD5,nD5-nD6,nD6)
   BlockD6 <- BlockD6[,c("nAtmpDep","nFailDep","nCompDep",
                         "PctFailDep","AmtDep","nDepDays",
                         "DepPerDay","AmtPerDay","nMeth",
                         "nCCd","UserID")];
   BlockD6 <- BlockD6[order(BlockD6$UserID),];
BlockW6 <- ddply(BlockW4,~UserID,summarise,
                 nAtmpWth=length(PayMeth));
   Scrap1 <- subset(BlockW4,BlockW4$Status=="S");
      Scrap2 <- ddply(Scrap1,~UserID,summarise,
                      nCompWth=length(PayMeth),
                      AmtWth=sum(Amount),
                      nWthDays=length(unique(SummaryDate)));
      BlockW6 <- merge(BlockW6,Scrap2,all.x=TRUE,all.y=TRUE,
                       by=c("UserID"));
   BlockW6[is.na(BlockW6)] = 0;
   BlockW6$nRvrsWth = BlockW6$nAtmpWth - BlockW6$nCompWth;
   BlockW6$PctRvrsWth = 100 * BlockW6$nRvrsWth / BlockW6$nAtmpWth;
   BlockW6$WthPerDay = BlockW6$nCompWth / BlockW6$nWthDays;
   BlockW6$AmtPerDay = BlockW6$AmtWth / BlockW6$nWthDays;
   BlockW6[is.na(BlockW6)] = 0;
   BlockW6 <- BlockW6[,c("nAtmpWth","nRvrsWth","nCompWth",
                         "PctRvrsWth","AmtWth","nWthDays",
                         "WthPerDay","AmtPerDay","UserID")];
   BlockW6 <- BlockW6[order(BlockW6$UserID),];
   nW6 <- nrow(BlockW6); nW6
sum(BlockS6$UserID==BlockD6$UserID)
sum(BlockS6$UserID==BlockC6$UserID)
############################################################
# Block 1: This block of analyses will contain descriptive #
#  statistics detailing the demographics of the sample.    #
############################################################
# The roster has 2238 men (89.9%) and            #
#  and 251 women (10.1%).                        #
##################################################
Scrap1 <- table(BlockC6$Gender); Scrap1
   round(100*Scrap1/nS6,digits=1)
##################################################
# The players have a mean age (as of             #
#  registration) of 29.6 years and a standard    #
#  deviation of 9.0 years.                       #
# * The five-number summary (in years) is        #
#  18-23-27-34-69.                               #
# Are the distributions of the ages of men and   #
#  women significantly different? Not really.    #
# * The five-number summaries are pretty close:  #
#  18-23-27-34-68 for men,                       #
#  18-24-28-37-69 for women.                     #
# * The p-value for the Mann-Whitney test is     #
#  greater than our alpha.                       #
##################################################
plot(x=BlockC6$SystemAgeAsOfReg,
     y=rnorm(nC6,0,0.10)+ifelse(BlockC6$Gender=="M",2,1),
     col=ifelse(BlockC6$Gender=="M","blue","red"),pch=18,
     xlab="Age as of registration",
     ylab="Blue for men, red for women",
     main="Distribution of Ages",yaxt="n")
Scrap1 <- subset(BlockC6$SystemAgeAsOfReg,BlockC6$Gender=="M");
Scrap2 <- subset(BlockC6$SystemAgeAsOfReg,BlockC6$Gender=="F");
Scrap3 <- BlockC6$SystemAgeAsOfReg;
Scrap4 <- data.frame(row.names=c("Male","Female","All"),
                  n=c(length(Scrap1),length(Scrap2),length(Scrap3)),
                  Mean=c(mean(Scrap1),mean(Scrap2),mean(Scrap3)),
                  SD=c(sd(Scrap1),sd(Scrap2),sd(Scrap3)),
                  P000=0,P025=0,P050=0,P075=0,P100=0);
   Scrap4[1,4:8] = quantile(Scrap1,Every25N);
   Scrap4[2,4:8] = quantile(Scrap2,Every25N);
   Scrap4[3,4:8] = quantile(Scrap3,Every25N);
   round(Scrap4,digits=1)
#           n Mean   SD P000 P025 P050 P075 P100
# Male   2238 29.4  8.8 18.0 23.0 27.0 34.0 68.0
# Female  251 31.3 10.1 18.0 24.0 28.0 37.0 69.0
# All    2489 29.6  9.0 18.0 23.0 27.0 34.0 69.0
   Scrap5 <- wilcox.test(BlockC6$SystemAgeAsOfReg~BlockC6$Gender);
   c(Scrap5$statistic,
     round(Scrap5$p.value,digits=4),
     round(rFromWilcox(Scrap5,nC6),digits=3))
# 309433.5 0.0081 0.0530
##################################################
# Make the frequency distribution for country    #
#  of residence.                                 #
# * We had countries for 2485 out of the 2489.   #
# * The top two are France (691, 27.8%) and      #
#  Germany (662, 26.6%).                         #
# * After that, the drop-off is pretty steep.    #
##################################################
Scrap1 <- table(BlockC6$CountryID)
Scrap2 <- data.frame(Players=as.numeric(Scrap1),
                     Number=as.numeric(rownames(Scrap1)));
Scrap3 <- merge(Scrap2,Nations[,c("Number","NameEN","Char3")],
                all.x=TRUE,all.y=FALSE,by=c("Number"));
   Scrap3$Pct = 100 * Scrap3$Players / nS6;
   nS6 - Scrap3$Players[Scrap3$Number==1000]
   Scrap3 <- Scrap3[,c("Char3","Players","Pct","NameEN")];
   Scrap3 <- Scrap3[order(-Scrap3$Players),];
Scrap4 <- Scrap3;
   Scrap4$Pct = round(Scrap4$Pct,digits=1);
Scrap5 <- subset(Scrap3,Scrap3$Pct>2.0);
   Scrap5 <- rbind(Scrap5,
                   data.frame(Char3=c("Other","Total"),
                              Players=c(nC6-sum(Scrap5$Players),nC6),
                              Pct=c(100*(nC6-sum(Scrap5$Players))/nC6,
                                    100),
                              NameEN=c("Other","Total"),
                              stringsAsFactors=FALSE));
   Scrap5$Pct = round(Scrap5$Pct,digits=1);
   Scrap5[,1:3]
# Char3 Players   Pct
#   FRA     691  27.8
#   DEU     662  26.6
#   BEL     147   5.9
#   ESP     141   5.7
#   AUT     114   4.6
#   CZE     103   4.1
#   GBR      99   4.0
#   HUN      95   3.8
#   SUI      90   3.6
#   NLD      58   2.3
#   SVK      50   2.0
# Other     239   9.6
# Total    2489 100.0
############################################################
# Block 2: Research Questions 1 and 2. This block will     #
#  contain the five number summaries (0th, 25th, 50th,     #
#  75th, and 100th percentiles), means, and standard       #
#  deviations of our seven measures of poker               #
#  participation, as well as a correlation matrix of these #
#  variables.                                              #
############################################################
# Calculate the mean, standard deviation, and    #
#  seven percentiles for each variable.          #
# * Use Kolmogorov-Smirnov tests for normality   #
#  (using ks.test()) to explore which variables  #
#  have skewed distributions.                    #
# -- The function doesn't do well with ties, so  #
#   we add perturbations with orders of          #
#   magnitude 10^-5 or smaller to "break" them.  #
# -- Do the z-scores for a column's data have a  #
#   distribution that looks kinda sorta like     #
#   the standard normal?                         #
# -- Nope. None of the distributions of the      #
#   seven variables were close to normal.        #
# * This is Table 2. Summary statistics of poker #
#  activity for the whole sample (N = 2,489).    #
# The medians for comparison come LaPlante et    #
#  al. and the Sitting package on The            #
#  Transparency Project.                         #
# * Duration: 196.0.                             #
# * Total Sessions: 60, Sessions/Day: 0.6.       #
# * Total Wagered: 808, Euros/Session: 13.       #
# * Net Loss: 111, Percent Lost: 20.             #
##################################################
Table02 <- data.frame(row.names=LabelsS,
                      P000=0,P025=0,P050=0,P075=0,
                      P095=0,P099=0,P100=0,Mean=0,SD=0,
                      KSD=0,pVal=0,Sig=rep("Acc",length(LabelsS)),
                      stringsAsFactors=FALSE);
i1 <- length(LabelsS);
   Scrap6 <- subset(BlockS6$PctLoss,BlockS6$StakesO>0);
   Table02[i1,1:7] = quantile(Scrap6,Skew7N);
   Table02$Mean[i1] = mean(Scrap6);
   Table02$SD[i1] = sd(Scrap6);
   Scrap7 <- Scrap6 + (1:length(Scrap6))*10^-10;
      Scrap7 <- (Scrap7-Table02[i1,6]) / Table02[i1,7];
   Scrap3 <- ks.test(Scrap7,"pnorm");
   Table02$KSD[i1] = Scrap3$statistic;
   Table02$pVal[i1] = Scrap3$p.value;
   if (Scrap3$p.value < alpha) {Table02$Sig[i1] = "Rej";}
   i1 = i1 - 1;
while (i1 > 0) {
   Table02[i1,1:7] = quantile(BlockS6[,i1],Skew7N);
   Table02$Mean[i1] = mean(BlockS6[,i1]);
   Table02$SD[i1] = sd(BlockS6[,i1]);
   Scrap7 <- BlockS6[,i1] + (1:nS6)*10^-10;
      Scrap7 <- (Scrap7-Table02[i1,6]) / Table02[i1,7];
   Scrap3 <- ks.test(Scrap7,"pnorm");
   Table02$KSD[i1] = Scrap3$statistic;
   Table02$pVal[i1] = Scrap3$p.value;
   if (Scrap3$p.value < alpha) {Table02$Sig[i1] = "Rej";}
   i1 = i1 - 1;
}
Scrap9 <- Table02;
   Scrap9[,1:9] = round(Scrap9[,1:9],digits=1);
   Scrap9[,10] = round(Scrap9[,10],digits=2);
   Scrap9[,11] = round(Scrap9[,11],digits=3);
   Scrap9[,1:5]
#                               P000  P025  P050   P075    P095
# Duration                       4.0  21.0  79.0  348.0   688.6
# Number of Sessions             4.0  20.0  43.0  133.0   830.4
# Sessions Per Day               0.0   0.2   0.8    2.3     7.3
# Total Overall Spend            0.9 114.1 439.7 1936.6 19852.4
# Average Spend per Session      0.0   3.2   7.9   23.1   126.6
# Net Loss                  -50775.9  14.4  52.3  186.0  1260.5
# Percent Loss                -854.7   6.2  13.7   25.9    62.9
   Scrap9[,6:11]
#                             P099      P100   Mean       SD  KSD pVal Sig
# Duration                   719.1     731.0  202.9    232.6 0.49    0 Rej
# Number of Sessions        2705.3   10529.0  197.1    575.4 0.52    0 Rej
# Sessions Per Day            16.8     176.5    2.0      5.1 0.49    0 Rej
# Total Overall Spend     152282.1 3742545.0 9728.0 108348.5 0.49    0 Rej
# Avg. Spend per Session     526.3    3270.9   36.4    136.9 0.51    0 Rej
# Net Loss                  4570.7   20378.6  249.9   1707.7 0.52    0 Rej
# Percent Loss                95.6     100.0   17.3     34.7 0.59    0 Rej
##################################################
# Because we expect to see skewed distributions, #
#  we will calculate Spearman correlations       #
#  between the variables (using                  #
#  corr(data,method=вЂќspearmanвЂќ)).                #
# * This will be Table C1. Spearman correlation  #
#  matrix for the measures of poker activity     #
#  (N = 2,489).                                  #
# * StksPS is not significantly correlated with  #
#  Duration, Sesh, or SeshPD.                    #
# * Scrap3 has the p-values, just for            #
#  diagnostics.                                  #
##################################################
TableC1 <- cor(BlockS6[,1:length(LabelsS)],method="spearman");
round(TableC1,digits=2)
#          Duration  Sesh SeshPD StakesO StksPS NetLoss PctLoss
# Duration     1.00
# Sesh         0.47  1.00
# SeshPD      -0.58  0.38   1.00
# StakesO      0.34  0.68   0.26    1.00
# StksPS       0.02  0.03   0.00    0.72   1.00
# NetLoss      0.28  0.48   0.12    0.70   0.48    1.00
# PctLoss     -0.09 -0.27  -0.15   -0.43  -0.35    0.19    1.00
Scrap2 <- TableC1; Scrap2[1:length(LabelsS),1:length(LabelsS)] = "";
Scrap3 <- TableC1;
i1 <- length(LabelsS); while (i1 > 0) {
   i2 = i1 - 1; while (i2 > 0) {
      Scrap4 <- cor.test(BlockS6[,i1],BlockS6[,i2],
                         method="spearman");
      Scrap2[i1,i2] = ifelse(Scrap4$p.value<alpha,"Rej","Acc");
      Scrap3[i1,i2] = Scrap4$p.value;
      Scrap3[i2,i1] = 0;
      i2 = i2 - 1;
   }
   Scrap3[i1,i1] = 0;
   i1 = i1 - 1;
}
Scrap2
#          Duration Sesh  SeshPD StakesO StksPS NetLoss PctLoss
# Duration ""       ""    ""     ""      ""     ""      ""     
# Sesh     "Rej"    ""    ""     ""      ""     ""      ""     
# SeshPD   "Rej"    "Rej" ""     ""      ""     ""      ""     
# StakesO  "Rej"    "Rej" "Rej"  ""      ""     ""      ""     
# StksPS   "Acc"    "Acc" "Acc"  "Rej"   ""     ""      ""     
# NetLoss  "Rej"    "Rej" "Rej"  "Rej"   "Rej"  ""      ""     
# PctLoss  "Rej"    "Rej" "Rej"  "Rej"   "Rej"  "Rej"   ""
round(Scrap3,digits=3)
#          Duration  Sesh SeshPD StakesO StksPS NetLoss PctLoss
# Duration
# Sesh        0.000
# SeshPD      0.000 0.000
# StakesO     0.000 0.000  0.000
# StksPS      0.447 0.101  0.865   0.000
# NetLoss     0.000 0.000  0.000   0.000  0.000
# PctLoss     0.000 0.000  0.000   0.000  0.000   0.000
##################################################
# Get the summary statistics for the BlockS6     #
#  variables for men, for women, and for         #
#  everyone.                                     #
# * This will be Table D1. Medians of the        #
#  measures of poker activity for males and      #
#  for females.                                  #
# Perform the Mann-Whitney tests comparing the   #
#  distributions of the men and women.           #
# * The two significant differences were in      #
#  Sessions per Day and Average Spend per        #
#  Session.                                      #
##################################################
TableD1 <- data.frame(row.names=LabelsS,
                      MedM=0,MedF=0,MedA=0,
                      W=0,pVal=0,r=0,Sig=rep("Acc",length(LabelsS)),
                      stringsAsFactors=FALSE);
Scrap1 <- subset(BlockS6,BlockS6$Gender=="M");
Scrap2 <- subset(BlockS6,BlockS6$Gender=="F");
i1 <- length(LabelsS); while (i1 > 0) {
   TableD1$MedM[i1] = median(Scrap1[,i1]);
   TableD1$MedF[i1] = median(Scrap2[,i1]);
   TableD1$MedA[i1] = median(BlockS6[,i1]);
   Scrap3 <- wilcox.test(BlockS6[,i1]~BlockS6$Gender);
   TableD1$W[i1] = Scrap3$statistic;
   TableD1$r[i1] = rFromWilcox(Scrap3,nS6);
   TableD1$pVal[i1] = Scrap3$p.value;
   if (Scrap3$p.value < alpha) {TableD1$Sig[i1] = "Rej";}
   i1 = i1 - 1;
}
TableD1 <- rbind(data.frame(row.names=c("n"),
                            MedM=nrow(Scrap1),MedF=nrow(Scrap2),
                            MedA=nS6,W=0,pVal=0,r=0,Sig="",
                            stringsAsFactors=FALSE),
                 TableD1);
Scrap9 <- TableD1;
   Scrap9[,1:4] = round(Scrap9[,1:4],digits=1);
   Scrap9[,5] = round(Scrap9[,5],digits=3);
   Scrap9[,6] = round(Scrap9[,6],digits=3); Scrap9
#                         MedM  MedF   MedA        W  pVal     r Sig
# n                     2238.0 251.0 2489.0
# Duration                82.0  64.0   79.0 266450.0 0.182 0.027 Acc
# Number of Sessions      43.0  46.0   43.0 296559.5 0.146 0.029 Acc
# Sessions Per Day         0.8   1.1    0.8 316665.5 0.001 0.066 Rej
# Total Overall Spend    454.5 327.9  439.7 262144.0 0.083 0.035 Acc
# Average Spend per Sesh   8.2   5.6    7.9 232970.0 0.000 0.089 Rej
# Net Loss                52.3  53.7   52.3 270553.0 0.339 0.019 Acc
# Percent Loss            13.5  15.4   13.7 292278.5 0.291 0.021 Acc
########################################
# Make a dot plot comparing the        #
#  distributions of men and women.     #
########################################
i1 <- 5;
Scrap1 <- data.frame(Data=BlockS6[,i1],
                     Height=ifelse(BlockS6$Gender=="M",2,1));
plot(x=Scrap1$Data,y=Scrap1$Height+rnorm(nS6,0,0.1),
     pch=18,cex=1,col=c("red","blue")[Scrap1$Height],
     main=LabelsS[i1],xlab=LabelsS[i1],ylim=c(0,3),
     ylab="Men in blue, women in red",yaxt="n");
############################################################
# Block 3: We will look at the distributions of the        #
#  aggregates derived from the [financial transaction      #
#  files].                                                 #
############################################################
# First comes Table 3. Summary statistics for    #
#  the measures of deposit activity for the full #
#  sample (N = 2,489).                           #
# * Of the 2489 depositors, 1449 (58.2%) only    #
#  used one method for depositing funds.         #
# * Over half (1570 out of 2489, 63.1%) did not  #
#  use a credit card to deposit funds.           #
##################################################
Table03 <- data.frame(row.names=LabelsD,
                      P000=0,P025=0,P050=0,P075=0,
                      P095=0,P099=0,P100=0,
                      Mean=0,SD=rep(0,length(LabelsD)),
                      stringsAsFactors=FALSE);
i1 <- length(LabelsD); while (i1 > 0) {
   Table03[i1,1:7] = quantile(BlockD6[,i1],Skew7N);
   Table03$Mean[i1] = mean(BlockD6[,i1]);
   Table03$SD[i1] = sd(BlockD6[,i1]);
   i1 = i1 - 1;
}
round(Table03[,1:5],digits=1)
#                                   P000 P025  P050  P075   P095
# Number of Attempted Deposits       1.0    3  11.0  35.0  155.0
# Number of Failed Deposits          0.0    0   2.0   8.0   42.0
# Number of Completed Deposits       1.0    3   7.0  25.0  111.6
# Percent of Failed Deposits         0.0    0  16.7  36.0   66.7
# Total Deposit Amount               5.0   51 174.6 745.0 4497.3
# Number of Deposit Days             1.0    2   6.0  18.0   77.0
# Deposits per Deposit Day           1.0    1   1.1   1.3    2.3
# Average Deposit Amount             2.8   15  24.5  51.0  203.6
# Number of Unique Deposit Methods   1.0    1   1.0   2.0    4.0
# Number of Unique Credit Cards      0.0    0   0.0   1.0    1.0
round(Table03[,6:9],digits=1)
#                                      P099     P100   Mean     SD
# Number of Attempted Deposits        380.7   1211.0   36.9   83.8
# Number of Failed Deposits           138.2    850.0   10.1   31.9
# Number of Completed Deposits        281.2   1168.0   26.7   62.4
# Percent of Failed Deposits           80.8     96.4   22.1   22.5
# Total Deposit Amount              15800.3 112337.9 1148.3 4376.9
# Number of Deposit Days              160.0    416.0   17.7   31.9
# Deposits per Deposit Day              3.8     11.8    1.3    0.6
# Average Deposit Amount              691.6   3114.8   62.7  160.5
# Number of Unique Deposit Methods      6.0     10.0    1.7    1.1
# Number of Unique Credit Cards         2.0      2.0    0.4    0.6
c(sum(BlockD6$nMeth==1),
  round(100*sum(BlockD6$nMeth==1)/nD6,digits=1))
# 1449 58.2
c(sum(BlockD6$nCCd==0),
  round(100*sum(BlockD6$nCCd==0)/nS6,digits=1))
# 1570 63.1
##################################################
# Next comes Table C3. Spearman correlation      #
#  matrix for the measures of deposit activity   #
#  (N = 2,489).                                  #
# * The only correlations that are NOT           #
#  significant involve nCCd (i.e., versus        #
#  nCompDep, nDepDays, and DepPerDay).           #
##################################################
TableC3 <- cor(BlockD6[,1:length(LabelsD)],method="spearman");
round(TableC3,digits=2)
#          nAtmp nFail nComp PctFail  Amt nDep DepPer AmtPer n    nCCd
#          Dep   Dep   Dep   Dep      Dep Days Day    Day    Meth
# nAtmpDep  1.00
# nFailDep  0.84  1.00
# nCompDep  0.97  0.71  1.00
# PctFail   0.41  0.77  0.21    1.00
# AmtDep    0.85  0.62  0.88    0.19 1.00
# nDepDays  0.95  0.69  0.98    0.20 0.85 1.00
# DepPerDay 0.68  0.51  0.69    0.17 0.64 0.57   1.00
# AmtPerDay 0.24  0.19  0.25    0.07 0.64 0.18   0.41   1.00
# nMeth     0.53  0.59  0.46    0.43 0.42 0.45   0.34   0.16 1.00
# nCCd      0.07  0.14  0.04    0.13 0.10 0.04   0.03   0.15 0.21 1.00
Scrap2 <- TableC3; Scrap2[1:length(LabelsD),1:length(LabelsD)] = "";
Scrap3 <- TableC3;
i1 <- length(LabelsD); while (i1 > 0) {
   i2 = i1 - 1; while (i2 > 0) {
      Scrap4 <- cor.test(BlockD6[,i1],BlockD6[,i2],
                         method="spearman");
      Scrap2[i1,i2] = ifelse(Scrap4$p.value<alpha,"Rej","Acc");
      Scrap3[i1,i2] = Scrap4$p.value;
      Scrap3[i2,i1] = 0;
      i2 = i2 - 1;
   }
   Scrap3[i1,i1] = 0;
   i1 = i1 - 1;
}
Scrap2[,1:8]
#            nAtmp nFail nComp PctFail Amt   nDep  DepPer AmtPer
#            Dep   Dep   Dep   Dep     Dep   Days  Day    Day
# nAtmpDep   ""    ""    ""    ""      ""    ""    ""     ""
# nFailDep   "Rej" ""    ""    ""      ""    ""    ""     ""
# nCompDep   "Rej" "Rej" ""    ""      ""    ""    ""     ""
# PctFailDep "Rej" "Rej" "Rej" ""      ""    ""    ""     ""
# AmtDep     "Rej" "Rej" "Rej" "Rej"   ""    ""    ""     ""
# nDepDays   "Rej" "Rej" "Rej" "Rej"   "Rej" ""    ""     ""
# DepPerDay  "Rej" "Rej" "Rej" "Rej"   "Rej" "Rej" ""     ""
# AmtPerDay  "Rej" "Rej" "Rej" "Rej"   "Rej" "Rej" "Rej"  ""
# nMeth      "Rej" "Rej" "Rej" "Rej"   "Rej" "Rej" "Rej"  "Rej"
# nCCd       "Rej" "Rej" "Acc" "Rej"   "Rej" "Acc" "Acc"  "Rej"
Scrap2[,9:10]
#            nMeth nCCd
# nAtmpDep   ""    ""
# nFailDep   ""    ""
# nCompDep   ""    ""
# PctFailDep ""    ""
# AmtDep     ""    ""
# nDepDays   ""    ""
# DepPerDay  ""    ""
# AmtPerDay  ""    ""
# nMeth      ""    ""
# nCCd       "Rej" ""
round(Scrap3[,1:8],digits=3)
#            nAtmp nFail nComp PctFail   Amt  nDep  DepPer AmtPer
#            Dep   Dep   Dep   Dep       Dep  Days  Day    Day
# nAtmpDep
# nFailDep   0.000
# nCompDep   0.000 0.000
# PctFailDep 0.000 0.000 0.000
# AmtDep     0.000 0.000 0.000   0.000
# nDepDays   0.000 0.000 0.000   0.000 0.000
# DepPerDay  0.000 0.000 0.000   0.000 0.000 0.000
# AmtPerDay  0.000 0.000 0.000   0.001 0.000 0.000   0.000
# nMeth      0.000 0.000 0.000   0.000 0.000 0.000   0.000  0.000
# nCCd       0.000 0.000 0.032   0.000 0.000 0.038   0.157  0.000
round(Scrap3[,9:10],digits=3)
#            nMeth nCCd
# nAtmpDep
# nFailDep
# nCompDep
# PctFailDep
# AmtDep
# nDepDays
# DepPerDay
# AmtPerDay
# nMeth
# nCCd       0.000
##################################################
# Next comes Table 4. Summary statistics for the #
#  measures of withdrawal activity (n = 876).    #
# * Five variables (nCompWth, AmtWth, nWthDays,  #
#  WthPerDay, AmtPerDay) were not in the         #
#  original pre-registration or the first        #
#  transparent change.                           #
# * Out of the 876, 591 (67.4%) had no           #
#  reversed withdrawals.                         #
# * On the other hand, 43 (4.9%) had only        #
#  reversed withdrawals.                         #
# * The remaining 242 (27.6%) had at least one   #
#  completed withdrawal and at least one         #
#  reversed withdrawal.                          #
# * One of the means is slightly off in the      #
#  paper (Total Amount Withdrawn, 1510.2 in the  #
#  paper, 1015.0 here).                          #
##################################################
c(nW6,sum(BlockW6$nRvrsWth==0),
  round(100*sum(BlockW6$nRvrsWth==0)/nW6,digits=1),
  sum(BlockW6$nCompWth==0),
  round(100*sum(BlockW6$nCompWth==0)/nW6,digits=1),
  sum(BlockW6$nCompWth*BlockW6$nRvrsWth>0),
  round(100*sum(BlockW6$nCompWth*BlockW6$nRvrsWth>0)/nW6,digits=1))
Table04 <- data.frame(row.names=LabelsW,
                      P000=0,P025=0,P050=0,P075=0,
                      P095=0,P099=0,P100=0,
                      Mean=0,SD=rep(0,length(LabelsW)),
                      stringsAsFactors=FALSE);
i1 <- length(LabelsW); while (i1 > 0) {
   Table04[i1,1:7] = quantile(BlockW6[,i1],Skew7N,na.rm=TRUE);
   Table04$Mean[i1] = mean(BlockW6[,i1]);
   Table04$SD[i1] = sd(BlockW6[,i1]);
   i1 = i1 - 1;
}
round(Table04[,1:5],digits=1)
#                                     P000 P025  P050  P075   P095
# Number of Attempted Withdrawals      1.0  1.0   3.0   8.0   31.0
# Number of Reversed Withdrawals       0.0  0.0   0.0   1.0   13.0
# Number of Completed Withdrawals      0.0  1.0   2.0   6.0   21.0
# Percent of Reversed Withdrawals      0.0  0.0   0.0  17.1   97.1
# Total Amount Withdrawn               0.0 80.3 290.3 951.4 5824.3
# Number of Withdrawal Days            0.0  1.0   2.0   6.0   19.0
# Withdrawals per Withdrawal Day       0.0  1.0   1.0   1.0    1.5
# Average Withdrawn Amount per Day     0.0 47.7 100.0 233.5  901.1
round(Table04[,6:9],digits=1)
#                                     P099     P100   Mean     SD
# Number of Attempted Withdrawals     83.5    299.0    8.7   22.2
# Number of Reversed Withdrawals      31.2    298.0    2.4   12.3
# Number of Completed Withdrawals     56.8    213.0    6.3   15.3
# Percent of Reversed Withdrawals    100.0    100.0   15.4   28.5
# Total Amount Withdrawn           20671.1 120446.6 1510.0 5826.9
# Number of Withdrawal Days           52.2    175.0    5.5   12.0
# Withdrawals per Withdrawal Day       2.0      3.0    1.0    0.3
# Average Withdrawn Amount per Day  2793.8  10000.0  259.9  603.6
##################################################
# Next is Table C4. Spearman correlation matrix  #
#  for the measures of withdrawal activity       #
#  (n = 846).                                    #
# * Make the table showing which ones are        #
#  statistically significant.                    #
# * There are several correlations that are NOT  #
#  significant.                                  #
# * One correlation that was listed as           #
#  significant in the paper was actually not     #
#  significant (WthPerDay versus nRvrsWth).      #
##################################################
TableC4 <- cor(BlockW6[,1:length(LabelsW)],method="spearman");
round(TableC4,digits=2)
#            nAtmp nRvrs nComp PctRvrs  Amt nWth WthPer AmtPer
#            Wth   Wth   Wth   Wth      Wth Days Day    Day
# nAtmpWth    1.00
# nRvrsWth    0.54  1.00
# nCompWth    0.90  0.24  1.00
# PctRvrsWth  0.42  0.96  0.08    1.00
# AmtWth      0.67  0.19  0.75    0.05 1.00
# nWthDays    0.89  0.24  0.99    0.07 0.75 1.00
# WthPerDay   0.51  0.11  0.62   -0.04 0.46 0.55   1.00
# AmtPerDay   0.21  0.00  0.28   -0.08 0.81 0.27   0.25   1.00
Scrap2 <- TableC4; Scrap2[1:length(LabelsW),1:length(LabelsW)] = "";
Scrap3 <- TableC4;
i1 <- length(LabelsW); while (i1 > 0) {
   i2 = i1 - 1; while (i2 > 0) {
      Scrap4 <- cor.test(BlockW6[,i1],BlockW6[,i2],
                         method="spearman");
      Scrap2[i1,i2] = ifelse(Scrap4$p.value<alpha,"Rej","Acc");
      Scrap3[i1,i2] = Scrap4$p.value;
      Scrap3[i2,i1] = 0;
      i2 = i2 - 1;
   }
   Scrap3[i1,i1] = 0;
   i1 = i1 - 1;
}
Scrap2[,1:8]
#            nAtmp nRvrs nComp PctRvrs Amt   nWth  WthPer AmtPer
#            Wth   Wth   Wth   Wth     Wth   Days  Day    Day
# nAtmpWth   ""    ""    ""    ""      ""    ""    ""     ""
# nRvrsWth   "Rej" ""    ""    ""      ""    ""    ""     ""
# nCompWth   "Rej" "Rej" ""    ""      ""    ""    ""     ""
# PctRvrsWth "Rej" "Rej" "Acc" ""      ""    ""    ""     ""
# AmtWth     "Rej" "Rej" "Rej" "Acc"   ""    ""    ""     ""
# nWthDays   "Rej" "Rej" "Rej" "Acc"   "Rej" ""    ""     ""
# WthPerDay  "Rej" "Acc" "Rej" "Acc"   "Rej" "Rej" ""     ""
# AmtPerDay  "Rej" "Acc" "Rej" "Acc"   "Rej" "Rej" "Rej"  ""
round(Scrap3,digits=3)
#            nAtmp nRvrs nComp PctRvrs Amt   nWth  WthPer AmtPer
#            Wth   Wth   Wth   Wth     Wth   Days  Day    Day
# nAtmpWth
# nRvrsWth   0.000
# nCompWth   0.000 0.000
# PctRvrsWth 0.000 0.000 0.022
# AmtWth     0.000 0.000 0.000 0.126
# nWthDays   0.000 0.000 0.000 0.033   0.000
# WthPerDay  0.000 0.001 0.000 0.215   0.000 0.000
# AmtPerDay  0.000 0.933 0.000 0.022   0.000 0.000 0.000
############################################################
# Block 4: Hypotheses 3 and 4. We will identify the most   #
#  involved players based on Total Overall Spend.          #
############################################################
# We've already made the quantile plot and       #
#  separated the top 1% from the rest of the     #
#  analytic sample.                              #
# * We have Table 5. Data for the top six        #
#   centile groups by Total Overall Spend.       #
# For each of the seven measures in the second   #
#  block, we will calculate the mean, standard   #
#  deviation, and median for the majority and    #
#  the most involved.                            #
# * This is Table 6. Summary statistics for      #
#  measures of poker activity for the most       #
#  involved 1% and the remaining 99%.            #
# * The numbering scheme is pushed forward       #
#  because we are inserting the data from the    #
#  top six centile groups.                       #
# * We will also use Mann-Whitney U-tests to     #
#  find the variables where the majority and     #
#  the most involved differ significantly.       #
# * The only one that wasn't significant was     #
#  NetLoss.                                      #
##################################################
Table05[95:100,]
   # BinStO nBin    minStO     maxStO     SumStO
   #     95   25  15605.97   19825.44   430097.3
   #     96   25  19870.41   27286.54   568337.1
   #     97   25  27329.15   41772.04   847935.7
   #     98   25  41797.70   70113.45  1358184.9
   #     99   25  72802.37  152036.53  2360943.7
   #    100   25 154083.30 3742545.01 15245175.6
Scrap9 <- split(1:nS6,BlockS6$BinStO==100);
   Scrap1 <- BlockS6[Scrap9$'FALSE',];
   Scrap2 <- BlockS6[Scrap9$'TRUE',];
Table06 <- data.frame(row.names=LabelsS,
                      MeanB=0,SDB=0,MedB=0,
                      MeanT=0,SDT=0,MedT=0,
                      W=0,pVal=0,r=0,Sig=rep("Acc",length(LabelsS)),
                      stringsAsFactors=FALSE);
i1 <- length(LabelsS); while (i1 > 0) {
   Table06$MeanB[i1] = mean(Scrap1[,i1]);
   Table06$SDB[i1] = sd(Scrap1[,i1]);
   Table06$MedB[i1] = median(Scrap1[,i1]);
   Table06$MeanT[i1] = mean(Scrap2[,i1]);
   Table06$SDT[i1] = sd(Scrap2[,i1]);
   Table06$MedT[i1] = median(Scrap2[,i1]);
   Scrap3 <- wilcox.test(BlockS6[,i1]~BlockS6$Top);
   Table06$W[i1] = Scrap3$statistic;
   Table06$pVal[i1] = Scrap3$p.value;
   Table06$r[i1] = rFromWilcox(Scrap3,nS6);
   if (Scrap3$p.value<alpha) {Table06$Sig[i1] = "Rej";}
   i1 = i1 - 1;
}
Table06 <- rbind(data.frame(row.names=c("n"),
                            MeanB=nrow(Scrap1),SDB=0,MedB=0,
                            MeanT=nrow(Scrap2),SDT=0,MedT=0,
                            W=0,pVal=0,r=0,Sig="",
                            stringsAsFactors=FALSE),
                 Table06);
Scrap9 <- Table06;
   Scrap9[,1:7] = round(Scrap9[,1:7],digits=1);
   Scrap9[,8] = round(Scrap9[,8],digits=3);
   Scrap9[,9] = round(Scrap9[,9],digits=3);
   Scrap9[,1:3]
#                            MeanB     SDB  MedB
# n                         2464.0
# Duration                   200.9   231.7  77.0
# Number of Sessions         180.7   522.6  42.5
# Sessions Per Day             2.0     5.1   0.8
# Total Overall Spend       3639.5 11906.7 427.2
# Average Spend per Session   31.2   108.5   7.7
# Net Loss                   265.9   874.9  52.4
# Percent Loss                17.5    34.8  13.9
   Scrap9[,4:6]
#                              MeanT      SDT     MedT
# n                             25.0
# Duration                     397.6    247.4    354.0
# Number of Sessions          1806.7   1889.1   1149.0
# Sessions Per Day               6.1      7.3      3.5
# Total Overall Spend       609807.0 907485.4 272581.4
# Average Spend per Session    540.9    683.1    318.2
# Net Loss                   -1326.0  14870.9    -46.0
# Percent Loss                   1.1      4.1      0.0
   Scrap9[,7:10]
#                                 W  pVal     r Sig
# n
# Duration                  14773.0 0.000 0.090 Rej
# Number of Sessions         2917.0 0.000 0.156 Rej
# Sessions Per Day          11756.5 0.000 0.107 Rej
# Total Overall Spend           0.0 0.000 0.173 Rej
# Average Spend per Session  1674.0 0.000 0.163 Rej
# Net Loss                  32382.0 0.658 0.009 Acc
# Percent Loss              53816.0 0.000 0.129 Rej
c(min(Scrap1$NetLoss),max(Scrap1$NetLoss),
  min(Scrap2$NetLoss),max(Scrap2$NetLoss))
# -5665.254 12505.61 -50775.95 20378.57
sort(Scrap2$NetLoss)
# [ 1] -50775.9500 -25828.0887 -13835.3321 -12082.4722  -9244.1416
# [ 6]  -7696.3438  -4575.4500  -4461.2000  -4316.9511  -2631.1600
# [11]  -2512.5678   -863.4647    -46.0000    878.0131   1369.3200
# [16]   2033.5100   2784.8334   3725.5933   4424.7714   7538.0700
# [21]   9483.9541  17115.8955  17930.7800  18056.6592  20378.5714
############################################################
# Reviewers requested additional [unplanned] analyses.     #
############################################################
# Test whether the correlations for the poker    #
#  measures are significantly different from the #
#  ones in LaPlante's "Sitting..." (2009).       #
# * The Sitting study had 3445 players. The n1   #
#  for comparing the Spearmans is always 3445.   #
# * Our correlation matrix has n2 = nS6 = 2489.  #
# * For 11 out of the 21 variable pairs, the     #
#  Spearman correlations in this study were      #
#  significantly different from the Spearman     #
#  correlations in the Sitting paper.            #
# -- For those 11, the average absolute          #
#   difference was 0.168.                        #
# -- The maximum difference was 0.280 (for       #
#   Percent Loss versus Stakes per Session).     #
# This is Table C2 for the unplanned analyses.   #
##################################################
Rho05 <- TableC1;
   Rho05[1,] = c( 0.00, 0.00, 0.00, 0.00, 0.00,0.00,0.00);
   Rho05[2,] = c( 0.62, 0.00, 0.00, 0.00, 0.00,0.00,0.00);
   Rho05[3,] = c(-0.38, 0.37, 0.00, 0.00, 0.00,0.00,0.00);
   Rho05[4,] = c( 0.56, 0.85, 0.29, 0.00, 0.00,0.00,0.00);
   Rho05[5,] = c( 0.13, 0.09,-0.03, 0.57, 0.00,0.00,0.00);
   Rho05[6,] = c( 0.31, 0.44, 0.10, 0.59, 0.42,0.00,0.00);
   Rho05[7,] = c(-0.23,-0.41,-0.18,-0.36,-0.07,0.36,0.00);
   rownames(Rho05) = colnames(Rho05); Rho05
#          Duration  Sesh SeshPD StakesO StksPS NetLoss PctLoss
# Duration     0.00  0.00   0.00    0.00   0.00    0.00    0.00
# Sesh         0.62  0.00   0.00    0.00   0.00    0.00    0.00
# SeshPD      -0.38  0.37   0.00    0.00   0.00    0.00    0.00
# StakesO      0.56  0.85   0.29    0.00   0.00    0.00    0.00
# StksPS       0.13  0.09  -0.03    0.57   0.00    0.00    0.00
# NetLoss      0.31  0.44   0.10    0.59   0.42    0.00    0.00
# PctLoss     -0.23 -0.41  -0.18   -0.36  -0.07    0.36    0.00
N05 <- Rho05; N05[1:7,1:7] = 3445; N05
#          Duration Sessions SeshPD Wagered EurosPS NetLoss PctLost
# Duration     3445     3445   3445    3445    3445    3445    3445
# Sessions     3445     3445   3445    3445    3445    3445    3445
# SeshPD       3445     3445   3445    3445    3445    3445    3445
# Wagered      3445     3445   3445    3445    3445    3445    3445
# EurosPS      3445     3445   3445    3445    3445    3445    3445
# NetLoss      3445     3445   3445    3445    3445    3445    3445
# PctLost      3445     3445   3445    3445    3445    3445    3445
Rho15 <- TableC1; round(Rho15,digits=3)
#          Duration   Sesh SeshPD StakesO StksPS NetLoss PctLoss
# Duration    1.000  0.469 -0.583   0.338  0.015   0.281  -0.092
# Sesh        0.469  1.000  0.380   0.681  0.033   0.485  -0.266
# SeshPD     -0.583  0.380  1.000   0.262  0.003   0.123  -0.153
# StakesO     0.338  0.681  0.262   1.000  0.717   0.696  -0.428
# StksPS      0.015  0.033  0.003   0.717  1.000   0.485  -0.351
# NetLoss     0.281  0.485  0.123   0.696  0.485   1.000   0.193
# PctLoss    -0.092 -0.266 -0.153  -0.428 -0.351   0.193   1.000
N15 <- Rho15; N15[1:6,1:6] <- nS6;
   N15[,7] = sum(BlockS6$StakesO>0);
   N15[7,] = sum(BlockS6$StakesO>0); N15
#          Duration Sesh SeshPD StakesO StksPS NetLoss PctLoss
# Duration     2489 2489   2489    2489   2489    2489    2489
# Sesh         2489 2489   2489    2489   2489    2489    2489
# SeshPD       2489 2489   2489    2489   2489    2489    2489
# StakesO      2489 2489   2489    2489   2489    2489    2489
# StksPS       2489 2489   2489    2489   2489    2489    2489
# NetLoss      2489 2489   2489    2489   2489    2489    2489
# PctLoss      2489 2489   2489    2489   2489    2489    2489
Comp1 <- N15; Comp1[1:7,1:7] = 0;
Comp2 <- N15; Comp2[1:7,1:7] = "Acc";
TableC2 <- data.frame(Var1=rep("a",21),Var2=rep("a",21),
                      Rho05=0,Rho15=0,Diff=0,pVal=0,Sig="Acc",
                      i1=0,i2=0,
                      stringsAsFactors=FALSE);
j1 <- 21; i1 <- 7; while (i1 > 0) {
   i2 <- i1 - 1; while (i2 > 0) {
      TableC2$i1[j1] = i1; TableC2$Rho05[j1] = Rho05[i1,i2];
      TableC2$i2[j1] = i2; TableC2$Rho15[j1] = Rho15[i1,i2];
      Comp1[i1,i2] = CompareSpearmans(Corr1=Rho05[i1,i2],
                                      Corr2=Rho15[i1,i2],
                                      n1=N05[i1,i2],n2=N15[i1,i2]);
      TableC2$pVal[j1] = Comp1[i1,i2];
      if (Comp1[i1,i2] < alpha) {
         Comp2[i1,i2] = "Rej"; TableC2$Sig[j1]= "Rej";
      }
      i2 = i2 - 1; j1 = j1 - 1;
   }
   i1 = i1 - 1;
}
TableC2$Diff = abs(TableC2$Rho05-TableC2$Rho15);
   TableC2 <- TableC2[order(-TableC2$Diff),]
   TableC2$Var1 = colnames(Rho05)[TableC2$i1];
   TableC2$Var2 = colnames(Rho15)[TableC2$i2];
round(Comp1,digits=3)
#          Duration  Sesh SeshPD StakesO StksPS NetLoss PctLoss
# Duration
# Sesh        0.000
# SeshPD      0.000 0.660
# StakesO     0.000 0.000  0.260
# StksPS      0.000 0.034  0.217   0.000
# NetLoss     0.236 0.037  0.391   0.000  0.003
# PctLoss     0.000 0.000  0.308   0.003  0.000   0.000
Comp2
#          Duration Sesh  SeshPD StakesO StksPS NetLoss PctLoss
# Duration  
# Sesh     "Rej"  
# SeshPD   "Rej"    "Acc"  
# StakesO  "Rej"    "Rej" "Acc"  
# StksPS   "Rej"    "Acc" "Acc"  "Rej"  
# NetLoss  "Acc"    "Acc" "Acc"  "Rej"   "Acc"  
# PctLoss  "Rej"    "Rej" "Acc"  "Acc"   "Rej"  "Rej"
TableC2[1,]
#    Var1   Var2 Rho05      Rho15      Diff         pVal Sig
# PctLoss StksPS -0.07 -0.3511644 0.2811644 6.853178e-28 Rej
sum(TableC2$Sig=="Rej")
# 11
mean(subset(TableC2$Diff,TableC2$Sig=="Rej"))
# 0.1674865
sd(subset(TableC2$Diff,TableC2$Sig=="Rej"))
# 0.05087278
Scrap9 <- TableC2;
   Scrap9[,3:6] = round(Scrap9[,3:6],digits=3); Scrap9
#    Var1     Var2 Rho05  Rho15  Diff  pVal Sig i1 i2
# PctLoss   StksPS -0.07 -0.351 0.281 0.000 Rej  7  5
# StakesO Duration  0.56  0.338 0.222 0.000 Rej  4  1
#  SeshPD Duration -0.38 -0.583 0.203 0.000 Rej  3  1
# StakesO     Sesh  0.85  0.681 0.169 0.000 Rej  4  2
# PctLoss  NetLoss  0.36  0.193 0.167 0.000 Rej  7  6
#    Sesh Duration  0.62  0.469 0.151 0.000 Rej  2  1
#  StksPS  StakesO  0.57  0.717 0.147 0.000 Rej  5  4
# PctLoss     Sesh -0.41 -0.266 0.144 0.000 Rej  7  2
# PctLoss Duration -0.23 -0.092 0.138 0.000 Rej  7  1
#  StksPS Duration  0.13  0.015 0.115 0.000 Rej  5  1
# NetLoss  StakesO  0.59  0.696 0.106 0.000 Rej  6  4
# PctLoss  StakesO -0.36 -0.428 0.068 0.003 Acc  7  4
# NetLoss   StksPS  0.42  0.485 0.065 0.003 Acc  6  5
#  StksPS     Sesh  0.09  0.033 0.057 0.034 Acc  5  2
# NetLoss     Sesh  0.44  0.485 0.045 0.037 Acc  6  2
#  StksPS   SeshPD -0.03  0.003 0.033 0.217 Acc  5  3
# NetLoss Duration  0.31  0.281 0.029 0.236 Acc  6  1
# StakesO   SeshPD  0.29  0.262 0.028 0.260 Acc  4  3
# PctLoss   SeshPD -0.18 -0.153 0.027 0.308 Acc  7  3
# NetLoss   SeshPD  0.10  0.123 0.023 0.391 Acc  6  3
#  SeshPD     Sesh  0.37  0.380 0.010 0.660 Acc  3  2
##################################################
# There are 43 people (roughly 5%) with only     #
#  reversed withdrawals.                         #
# * Make a TableNL so that we can see how the    #
#  the univariate statistics change if we        #
#  remove these 43 people.                       #
# * A lot of the minimums changed, as expected   #
#  when you remove people who had no completed   #
#  withdrawals and therefore withdrew nothing.   #
# * Make a TableC5 so that we can see how the    #
#  correlations change if we remove these 43     #
#  people.                                       #
# * A few of the correlations involving          #
#  WthPerDay listed in the paper were 0.01 off   #
#  of the values listed here.                    #
# * There were definite changes in some of the   #
#  correlations involving nRvrsWth and           #
#  PctRvrsWth.                                   #
# -- The correlation between nRvrsWth and        #
#   AmtPerDay changed from not significant to    #
#   significant.                                 #
# -- The correlations between PctRvrsWth and     #
#   four other variables (nCompWth, AmtWth,      #
#   nWthDays, WthPerDay) changed from not        #
#   significant to significant.                  #
# -- The correlation between WthPerDay and       #
#   AmtPerDay changed from significant to not    #
#   significant.                                 #
##################################################
Scrap5 <- subset(BlockW6,BlockW6$nCompWth>0);
TableNL <- data.frame(row.names=LabelsW,
                      P000=0,P025=0,P050=0,P075=0,P100=0,
                      Mean=0,SD=rep(0,length(LabelsW)),
                      stringsAsFactors=FALSE);
i1 <- length(LabelsW); while (i1 > 0) {
   TableNL[i1,1:5] = quantile(Scrap5[,i1],Every25N,na.rm=TRUE);
   TableNL[i1,6] = mean(Scrap5[,i1]);
   TableNL[i1,7] = sd(Scrap5[,i1]);
   i1 = i1 - 1;
}
round(TableNL[,1:5],digits=1)
#                                     P000  P025  P050   P075     P100
# Number of Attempted Withdrawals      1.0   1.0   3.0    8.0    299.0
# Number of Reversed Withdrawals       0.0   0.0   0.0    1.0    298.0
# Number of Completed Withdrawals      1.0   1.0   3.0    6.0    213.0
# Percent of Reversed Withdrawals      0.0   0.0   0.0   10.0     99.7
# Total Amount Withdrawn               7.0 100.0 340.4 1000.0 120446.6
# Number of Withdrawal Days            1.0   1.0   2.0    6.0    175.0
# Withdrawals per Withdrawal Day       1.0   1.0   1.0    1.0      3.0
# Average Withdrawn per Withdrawal Day 7.0  52.2 105.0  246.2  10000.0
round(TableNL[,6:7],digits=1)
#                                               Mean     SD
# Number of Attempted Withdrawals                9.0   22.7
# Number of Reversed Withdrawals                 2.4   12.6
# Number of Completed Withdrawals                6.6   15.6
# Percent of Reversed Withdrawals               11.0   21.6
# Total Amount Withdrawn                      1588.2 5965.2
# Number of Withdrawal Days                      5.8   12.2
# Withdrawals per Withdrawal Day                 1.1    0.2
# Average Amount Withdrawn per Withdrawal Day  273.3  616.0
TableC5 <- cor(Scrap5[,1:length(LabelsW)],method="spearman");
round(TableC5,digits=2)
#            nAtmp nRvrs nComp PctRvrs  Amt nWth WthPer AmtPer
#            Wth   Wth   Wth   Wth      Wth Days Day    Day
# nAtmpWth    1.00
# nRvrsWth    0.62  1.00
# nCompWth    0.93  0.39  1.00
# PctRvrsWth  0.55  0.98  0.30    1.00
# AmtWth      0.68  0.33  0.71    0.27 1.00
# nWthDays    0.92  0.39  0.99    0.30 0.71 1.00
# WthPerDay   0.52  0.30  0.54    0.23 0.34 0.44   1.00
# AmtPerDay   0.17  0.12  0.16    0.11 0.78 0.15   0.08  1.00
Scrap6 <- TableC5; Scrap6[1:length(LabelsW),1:length(LabelsW)] = "";
Scrap7 <- TableC5;
i1 <- length(LabelsW); while (i1 > 0) {
   i2 = i1 - 1; while (i2 > 0) {
      Scrap8 <- cor.test(Scrap5[,i1],Scrap5[,i2],
                         method="spearman");
      Scrap6[i1,i2] = ifelse(Scrap8$p.value<alpha,"Rej","Acc");
      Scrap7[i1,i2] = Scrap8$p.value;
      Scrap7[i2,i1] = 0;
      i2 = i2 - 1;
   }
   Scrap7[i1,i1] = 0;
   i1 = i1 - 1;
}
Scrap6[,1:8]
#            nAtmp nRvrs nComp PctRvrs   Amt  nWth WthPer AmtPer
#            Wth   Wth   Wth   Wth       Wth  Days Day    Day
# nAtmpWth   ""    ""    ""    ""      ""    ""    ""     ""       
# nRvrsWth   "Rej" ""    ""    ""      ""    ""    ""     ""       
# nCompWth   "Rej" "Rej" ""    ""      ""    ""    ""     ""       
# PctRvrsWth "Rej" "Rej" "Rej" ""      ""    ""    ""     ""       
# AmtWth     "Rej" "Rej" "Rej" "Rej"   ""    ""    ""     ""       
# nWthDays   "Rej" "Rej" "Rej" "Rej"   "Rej" ""    ""     ""       
# WthPerDay  "Rej" "Rej" "Rej" "Rej"   "Rej" "Rej" ""     ""       
# AmtPerDay  "Rej" "Rej" "Rej" "Acc"   "Rej" "Rej" "Acc"  ""
round(Scrap7,digits=3)
#            nAtmp nRvrs nComp PctRvrs Amt   nWth  WthPer AmtPer
#            Wth   Wth   Wth   Wth     Wth   Days  Day    Day
# nAtmpWth
# nRvrsWth   0.000
# nCompWth   0.000 0.000
# PctRvrsWth 0.000 0.000 0.000
# AmtWth     0.000 0.000 0.000 0.000
# nWthDays   0.000 0.000 0.000 0.000   0.000
# WthPerDay  0.000 0.000 0.000 0.000   0.000 0.000
# AmtPerDay  0.000 0.001 0.000 0.002   0.000 0.000 0.029
Scrap7[8,2]
# 0.0008695221


