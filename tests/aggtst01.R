
require("CDNmoney")

data("CanadianMoneyData.asof.26Aug2002", package="CDNmoney")

if(!require("tframe")) stop("This test requires the tframe package.")
  # in S series must be the same length to add
if (!is.R()) {
  cat("################ Calculations of truncated versions for S\n")
  MB2001 <- tfwindow(MB2001, start=c(1981, 11), end=c(2002,6)) 
  MB486 <- tfwindow(MB486, start=c(1981, 11), end=c(2002,6)) 
  MB487p <- tfwindow(MB487p, start=c(1981, 11), end=c(2002,6)) 
  TMLinterbank <- tfwindow(TMLinterbank, start=c(1981, 11), end=c(2002,6)) 
  float    <- tfwindow(float,    start=c(1981, 11), end=c(2002,6)) 
  MB452    <- tfwindow(MB452,    start=c(1981, 11), end=c(2002,6)) 
  MB452adj <- tfwindow(MB452adj, start=c(1981, 11), end=c(2002,6)) 
  MB472 <- tfwindow(MB472, start=c(1981, 11), end=c(2002,6)) 
  NonbankCheq <- tfwindow(NonbankCheq, start=c(1981, 11), end=c(2002,6)) 
  CUadj <- tfwindow(CUadj, start=c(1981, 11), end=c(2002,6)) 
  MB453 <- tfwindow(MB453, start=c(1981, 11), end=c(2002,6)) 
  MB454 <- tfwindow(MB454, start=c(1981, 11), end=c(2002,6)) 
  MB473 <- tfwindow(MB473, start=c(1981, 11), end=c(2002,6)) 
  MB473adj <- tfwindow(MB473adj, start=c(1981, 11), end=c(2002,6)) 
  NonbankNonCheq <- tfwindow(NonbankNonCheq, start=c(1981, 11), end=c(2002,6)) 
  NonbankTerm <- tfwindow(NonbankTerm, start=c(1981, 11), end=c(2002,6)) 
  MB2046 <- tfwindow(MB2046, start=c(1981, 11), end=c(2002,6)) 
  MB2047 <- tfwindow(MB2047, start=c(1981, 11), end=c(2002,6)) 
  MB2048 <- tfwindow(MB2048, start=c(1981, 11), end=c(2002,6)) 
  MB2057 <- tfwindow(MB2057, start=c(1981, 11), end=c(2002,6)) 
  MB2058 <- tfwindow(MB2058, start=c(1981, 11), end=c(2002,6)) 
  MB475 <- tfwindow(MB475, start=c(1981, 11), end=c(2002,6)) 
  MB482 <- tfwindow(MB482, start=c(1981, 11), end=c(2002,6))
  M1real <- tfwindow(M1real, start=c(1981, 11), end=c(2002,6))
  M1total <- tfwindow(M1total, start=c(1981, 11), end=c(2002,6))
  M1PerCapita<- tfwindow(M1PerCapita, start=c(1981, 11), end=c(2002,6))
  }
  
cat("################ Calculations to get monetary aggregates\n")

M1gross <-  tframed(MB2001 + MB486 + MB487p + TMLinterbank, names="gross M1 (B2054)")
M1p     <-  tframed(MB2001 + MB486 + MB487p + MB452 + MB452adj + MB472 + NonbankCheq, names="M1+ (B2060)")
M1pp    <-  tframed(CUadj + M1p   + MB453 + MB473 + MB473adj + NonbankNonCheq, names="M1++ (B2061)")
M2      <-  tframed(M1total + MB472 + MB473 + MB452 + MB453 + MB454, names="M2 (B2031)")
M2p     <-  tframed(M2    + NonbankCheq + NonbankNonCheq + NonbankTerm  
                          + MB2046 + MB2047 + MB2048, names="M2+ (B2037)")
M2pp    <-  tframed(M2p   + MB2057 + MB2058, names="M2++ (B2059)")
M3      <-  tframed(M2    + MB475  + MB482,  names="M3 (B2030)")

fuzz <- 1e-4
if (max(abs(M1total + float - M1gross)) > fuzz) 
   stop("M1total + float = M1gross does not check out.")
# tfwindow below is so that same test can be used in S and R
if (max(abs(14142795.699713 - sum(tfwindow(M1gross, start=c(1981, 11), end=c(2002,6)))))
    > fuzz)  stop("M1gross does not check out.")
if (max(abs(34660366.422202 - sum(tfwindow(M1p, start=c(1981, 11), end=c(2002,6)))))
    > fuzz)  stop("M1p does not check out.")
if (max(abs(51830587 - sum(tfwindow(M1pp, start=c(1981, 11), end=c(2002,6)))))
    > fuzz)  stop("M1pp does not check out.")
if (max(abs(88222821.575018 - sum(tfwindow(M2, start=c(1981, 11), end=c(2002,6)))))
    > fuzz)  stop("M2 does not check out.")
if (max(abs(121535893.35587 - sum(tfwindow(M2p, start=c(1981, 11), end=c(2002,6)))))
    > fuzz)  stop("M2p does not check out.")
if (max(abs(154718166.35587 - sum(tfwindow(M2pp, start=c(1981, 11), end=c(2002,6)))))
    > fuzz)  stop("M2pp does not check out.")
if (max(abs(109253304.67148 - sum(tfwindow(M3, start=c(1981, 11), end=c(2002,6)))))
    > fuzz)  stop("M3 does not check out.")


cat("################ Calculations of cpi and pop\n")

#  M1real = M1total * 100/p100000  (CPI - p20 Bank of Canada Weekly Financial
#       Statistics, June 1992=100)
#  M1PerCapita = M1total * 100 /(pop * p100000) # quarterly pop series converted
#       to monthly using spline.

cpi <- 100 * M1total / M1real
seriesNames(cpi) <- "CPI"

popm <- M1total / M1PerCapita 
seriesNames(popm) <- "Population of Canada"

cat("################ Plot aggregates\n")

tfplot(tbind(M1total, M1gross, M1p, M1pp))
tfplot(tbind(M1PerCapita, M1real))
tfplot(tbind(M2, M2p, M2pp))
tfplot(M3)
