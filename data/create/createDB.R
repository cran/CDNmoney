# After any changes be sure to compare calculations below with tests and demo.

#Get data from ets and create a file with the data set.

#     real M1 (using CPI)  *100/p100000  (p20 wfs)
#     per capita M1       /pop converted by spline to monthly

library("padi")
library("tframe")

################ MB money dataset (continuity adjusted money components)

B2001   <- tframed(getpadi( "B2001", server="ets"), names="currency")
MB486   <- tframed(getpadi( "MB486", server="ets"), names="PCA")
MB487   <- tframed(getpadi( "MB487", server="ets"), names="CA other demand")
MB487p  <- tframed(getpadi( "MB487PLUS", server="ets"), names="CA other demand - specially adjusted for M1+")
MB472   <- tframed(getpadi( "MB472", server="ets"), names="N-P cheq notice")
MB473   <- tframed(getpadi( "MB473", server="ets"), names="N-P non-cheq")
MB473pp <- tframed(getpadi( "MB473PLUSPLUS", server="ets"), names="N-P non-cheq - specially adjusted for M1++")
MB452   <- tframed(getpadi( "MB452", server="ets"), names="pers cheq")
MB452p  <- tframed(getpadi( "MB452PLUS", server="ets"), names="pers cheq - specially adjusted for M1+")
MB453   <- tframed(getpadi( "MB453", server="ets"), names="pers n-cheq notice")
MB454   <- tframed(getpadi( "MB454", server="ets"), names="pers term")
MB475   <- tframed(getpadi( "MB475", server="ets"), names="N-P term dep")
MB482   <- tframed(getpadi( "MB482", server="ets"), names="Fgn curr dep")
float   <- tframed(getpadi( "B476",  server="ets"), names="float")
B2046   <- tframed(getpadi( "B2046", server="ets"), names="life insur")
B2047   <- tframed(getpadi( "B2047", server="ets"), names="dep at gov inst")
B2048   <- tframed(getpadi( "B2048", server="ets"), names="mmmf")
B2057   <- tframed(getpadi( "B2057", server="ets"), names="CSB")
B2058   <- tframed(getpadi( "B2058", server="ets"), names="non-mmmf")


NonbankCheq    <- tframed(getpadi("TMLCHEQPLUS", server="ets") + 
                    getpadi("LCUCHEQPLUS", server="ets"), names="NonbankCheq")
NonbankNonCheq <- tframed(getpadi("TMLNCPLUSPLUS", server="ets")  +
                    getpadi("LCUNCPLUSPLUS", server="ets"), names="NonbankNonCheq")
NonbankTotal <- tframed(getpadi("MB2038M2P", server="ets") + 
                    getpadi("MB2042", server="ets"), names="NonbankTotal")
# This is personal and non-personal. From 1989? there may be a breakdown.
NonbankTerm <- NonbankTotal - (NonbankCheq + NonbankNonCheq )


TMLinterbank <- MB487 - MB487p # should be Fame mave(plus16, 2) + mave(plus18, 2)
 
MB473adj <- MB473pp - MB473
MB452adj <- MB452p  - MB452

M1p     <-  tframed(B2001 + MB486 + MB487p + MB452 + MB452adj + MB472 + NonbankCheq, names="M1+ (B2060)")

# As of Aug 2002, (but hopefully a better estimate will eventually be available)
# CUadj  is really calculated by Fame code:
#   ratio = lcuncheq[apr1996]/(lcuncheq[apr1996]+lcuterm[apr1996]) 
#   date jan1968 to mar1996
#   CUadj = (lcuncheq-(lcuncheq+lcuterm)*ratio) 
#  but next is easier from R.

CUadj   <-  tframed(getpadi("B2061", server="ets") - 
                (M1p + MB453 + MB473 + MB473adj + NonbankNonCheq),
		names="CU estimate fix")



################ Calculations to get monetary aggregates
##########  (these should work with the saved dataset alone)
####### copy/move to demo/aggregates.R (and add cpi and popm calc)

M1gross <-  tframed(B2001 + MB486 + MB487p + TMLinterbank, names="gross M1 (B2054)")
M1total <-  tframed(M1gross - float, names="M1total (net) (B2033)")
M1p     <-  tframed(B2001 + MB486 + MB487p + MB452 + MB452adj + MB472 + NonbankCheq, names="M1+ (B2060)")
M1pp    <-  tframed(CUadj + M1p   + MB453 + MB473 + MB473adj + NonbankNonCheq, names="M1++ (B2061)")
M2      <-  tframed(M1total + MB472 + MB473 + MB452 + MB453 + MB454, names="M2 (B2031)")
M2p     <-  tframed(M2    + NonbankCheq + NonbankNonCheq + NonbankTerm +
                          + B2046 + B2047 + B2048, names="M2+ (B2037)")
M2pp    <-  tframed(M2p   + B2057 + B2058, names="M2++ (B2059)")
M3      <-  tframed(M2    + MB475 + MB482, names="M3 (B2030)")


tfplot(M1gross )
tfplot(M1total )
tfplot(M1p     )
tfplot(M1pp    )
tfplot(M2      )
tfplot(M2p     )
tfplot(M2pp    )
tfplot(M3      )

################ check against monetary aggregates

max(abs(M1gross - getpadi( "B2054", server="ets")))
max(abs(M1total   - getpadi( "B2033", server="ets")))
max(abs(M1p     - getpadi( "B2060", server="ets")))
max(abs(M1pp    - getpadi( "B2061", server="ets")))
max(abs(M2      - getpadi( "B2031", server="ets")))
max(abs(M2p     - getpadi( "B2037", server="ets")))
max(abs(M2pp    - getpadi( "B2059", server="ets")))
max(abs(M3      - getpadi( "B2030", server="ets")))


tfplot(M1gross , getpadi( "B2054", server="ets"))
tfplot(M1total , getpadi( "B2033", server="ets"))
tfplot(M1p     , getpadi( "B2060", server="ets"))
tfplot(M1pp    , getpadi( "B2061", server="ets"))
tfplot(M2      , getpadi( "B2031", server="ets"))
tfplot(M2p     , getpadi( "B2037", server="ets"))
tfplot(M2pp    , getpadi( "B2059", server="ets"))
tfplot(M3      , getpadi( "B2030", server="ets"))

tfplot(M1gross - getpadi( "B2054", server="ets"))
tfplot(M1total - getpadi( "B2033", server="ets"))
tfplot(M1p     - getpadi( "B2060", server="ets"))
tfplot(M1pp    - getpadi( "B2061", server="ets"))
tfplot(M2      - getpadi( "B2031", server="ets"))
tfplot(M2p     - getpadi( "B2037", server="ets"))
tfplot(M2pp    - getpadi( "B2059", server="ets"))
tfplot(M3      - getpadi( "B2030", server="ets"))



################ save data set

cpi <- getpadi("P100000", server="ets") # June 1992=100
pop <- getpadi("D1", server="ets")

popm <- ts(spline(seq(length(pop)), pop, 
                  n=3*length(pop) - 2)$y, start=c(1946,3), freq=12)

M1real <- M1total * 100 / cpi
seriesNames(M1real) <- "real M1"

M1PerCapita <- M1total / popm
seriesNames(M1PerCapita) <- "per capita M1"

MBseries <- c("B2001", "MB486", "MB487p", "MB452", "MB453", "MB454", "MB472", 
   "NonbankCheq", "MB473", "NonbankNonCheq", "NonbankTerm", "B2046",
   "B2047","B2048","B2057", "B2058", , "MB475", "MB482",
   "M1total", "M1real", "M1PerCapita",
   "float", "TMLinterbank", "MB452adj", "MB473adj", "CUadj") 
  
#dump(MBseries, file="CanadianMoneyData.asof.26Aug2002.R")
#source("CanadianMoneyData.asof.xxAug2002.R")
