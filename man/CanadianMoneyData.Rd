\name{CanadianMoneyData}
\docType{data}
\alias{CanadianMoneyData}
\alias{CanadianMoneyData.asof.3Mar2006}
\alias{CanadianMoneyData.asof.28Jan2005}
\alias{CanadianMoneyData.asof.6Feb2004}
\alias{CanadianMoneyData.asof.26Aug2002}
\alias{MB2001}
\alias{MB486}
\alias{MB487p}
\alias{MB452}
\alias{MB453}
\alias{MB454}
\alias{MB472}
\alias{NonbankCheq}
\alias{MB473}
\alias{NonbankNonCheq}
\alias{NonbankTerm}
\alias{MB2046}
\alias{MB2047}
\alias{MB2048}
\alias{MB2057}
\alias{MB2058}
\alias{MB475}
\alias{MB482}
\alias{M1total}
\alias{M1real}
\alias{M1PerCapita}
\alias{float}
\alias{TMLinterbank}
\alias{MB452adj}
\alias{MB473adj}
\alias{CUadj}

\title{Continuity Adjusted Component Data for Canadian Monetary Aggregates}
\description{Components of the Canadian Monetary aggregates.}

\usage{
	data(CanadianMoneyData)
	data(CanadianMoneyData.asof.3Mar2006)
	data(CanadianMoneyData.asof.28Jan2005)
	data(CanadianMoneyData.asof.6Feb2004)
	data(CanadianMoneyData.asof.26Aug2002)
}

\format{
	The objects are time series.
}

\details{
	Several data objects are loaded, as listed in the table below. The 
	\code{CanadianMoneyData} usage loads the most recently version and 
	other usages loads data as of a given date.
	
	These data are the components of the Canadian Monetary aggregates.
	They have been "continuity adjusted" so that take-overs and mergers do
	not result in breaks in the series. This involves re-arranging
	historical data so that it reflects the current structure of the
	industry. Trust company deposits are not included in some Canadian 
	monetary aggregates so, if a bank takes over a trust company, the
	historical data for the trust company must be added to the bank's 
	historical data to eliminate a break in the series. The series start 
	at various  dates but continuity adjustments prior to 1981 are not 
	complete. More details about the adjustment are provided 
	in Kottaras (2003).

	The originally released data is usually very accurate because of
	bank reporting requirements. Beware that continuity adjustments
	described above are the main reason for revisions to the data. For this
	reason the "as of" data may not be especially useful for some kinds of
	data revision studies.

\tabular{lll}{
 variable (ID)   \tab short description    \tab longer description \cr 
   \tab \tab \cr 
 MB2001  	 \tab currency  	   \tab currency outside banks\cr
 MB486  	 \tab PCA		   \tab personal chequing accounts at banks\cr
 MB487p 	 \tab CA other demand	   \tab current accounts at banks\cr
 MB452  	 \tab pers cheq 	   \tab personal notice - chequable at banks\cr
 MB453  	 \tab pers n-cheq notice   \tab personal notice - non-chequable at banks\cr
 MB454  	 \tab pers term 	   \tab personal term at banks\cr
 NonbankCheq	 \tab NonbankCheq	   \tab chequing accounts at non-banks\cr
 MB472  	 \tab N-P cheq notice	   \tab non-personal chequable notice\cr
 MB473  	 \tab N-P non-cheq	   \tab non-personal - non-chequable\cr
 NonbankNonCheq  \tab NonbankNonCheq	   \tab non-chequing accounts at non-banks\cr
 MB451  	 \tab pers savings	   \tab personal savings accounts at banks\cr
 NonbankTerm	 \tab NonbankTerm	   \tab term deposits at non-banks\cr
 MB2046  	 \tab life insur	   \tab individual annuities at life insurance co.\cr
 MB2047  	 \tab dep at gov inst	   \tab deposits at government institutions\cr
 MB2048  	 \tab mmmf		   \tab money market mutual funds\cr
 MB2057  	 \tab CSB		   \tab Canadian saving bonds\cr
 MB2058  	 \tab non-mmmf  	   \tab non-money market mutual funds\cr
 MB475  	 \tab N-P term dep	   \tab non-personal term at banks\cr
 MB482  	 \tab Fgn curr dep	   \tab Foreign currency deposits of residents\cr


 MB451  	 \tab Pers savings	   \tab MB452 + MB453 + MB454\cr
 MB478  	 \tab Demand deposits	   \tab Chartered bank net demand deposits\cr
 MB2038  	 \tab NonbankTotal	   \tab Total Deposits at Trust & Mortgage Loan Companies\cr


 float  	 \tab B476                 \tab checks and deposits "in transit"\cr 
 TMLinterbank	 \tab TML depostis at banks \tab Deposits of trust mortgage and loan companies at banks\cr
 MB452adj	 \tab pre-1982 problem      \tab \cr
 MB473adj	 \tab pre-1982 problem      \tab \cr
 CUadj  	 \tab CU estimate fix	    \tab fix for poor Statcan estimate of NonbankNonCheq/NonbankTerm split for one Credit Union prior to April 1996\cr
}

The following table indicates how these components are added for the Bank of
Canada monetary aggregates. The column on the left indicates how they are
combined to form components for factor estimates as in Gilbert and Pichette (2003).
\newpage
\tabular{lccccccccc}{
 \tab  \tab ----- \tab ---- \tab ---- \tab Monetary \tab Aggregate \tab ---- \tab ---- \tab ---- \cr
    \tab  	      \tab	  \tab netM1= \tab     \tab	 \tab	    \tab      \tab	\tab \cr
    \tab  	      \tab grossM1\tab totalM1\tab M1+ \tab M1++ \tab  M2   \tab M2+  \tab M2++  \tab M3 \cr
comp. \tab variable (ID) \tab B2054  \tab B2033 \tab B2060\tab B2061\tab B2031 \tab B2037 \tab B2059 \tab B2030 \cr 
 \tab  \tab \tab \tab \tab \tab \tab \tab \tab \cr
 1 \tab MB2001        \tab X \tab   X   \tab  X  \tab   X \tab    X  \tab  X  \tab   X  \tab  X \cr
 2 \tab MB486	      \tab X \tab   X	\tab  X  \tab	X \tab    X  \tab  X  \tab   X  \tab  X \cr
 4 \tab MB487PLUS     \tab X \tab   X	\tab  X  \tab	X \tab    X  \tab  X  \tab   X  \tab  X \cr
 2 \tab MB452	      \tab   \tab	\tab  X  \tab	X \tab    X  \tab  X  \tab   X  \tab  X \cr
 6 \tab MB453	      \tab   \tab	\tab	 \tab	X \tab    X  \tab  X  \tab   X  \tab  X \cr
 3 \tab NonbankCheq   \tab   \tab	\tab  X  \tab	X \tab       \tab  X  \tab   X  \tab	\cr
 4 \tab MB472	      \tab   \tab	\tab  X  \tab	X \tab    X  \tab  X  \tab   X  \tab  X \cr
 4 \tab MB473	      \tab   \tab	\tab	 \tab	X \tab    X  \tab  X  \tab   X  \tab  X \cr
 6 \tab NonbankNonCheq \tab   \tab	 \tab	  \tab   X \tab       \tab  X  \tab   X  \tab	 \cr
 6 \tab MB454	      \tab   \tab	\tab	 \tab	  \tab    X  \tab  X  \tab   X  \tab  X \cr
 6 \tab NonbankTerm   \tab   \tab	\tab	 \tab	  \tab       \tab  X  \tab   X  \tab	\cr
 6 \tab MB2046        \tab   \tab	\tab	 \tab	  \tab       \tab  X  \tab   X  \tab	\cr
 6 \tab MB2047        \tab   \tab	\tab	 \tab	  \tab       \tab  X  \tab   X  \tab	\cr
 6 \tab MB2048        \tab   \tab	\tab	 \tab	  \tab       \tab  X  \tab   X  \tab	\cr
 6 \tab MB2057        \tab   \tab	\tab	 \tab	  \tab       \tab     \tab   X  \tab	\cr
 6 \tab MB2058        \tab   \tab	\tab	 \tab	  \tab       \tab     \tab   X  \tab	\cr
 5 \tab MB475	      \tab   \tab	\tab	 \tab	  \tab       \tab     \tab	\tab  X \cr
 6 \tab MB482	      \tab   \tab	\tab	 \tab	  \tab       \tab     \tab	\tab  X \cr
 \tab \tab \tab \tab \tab \tab \tab \tab \tab \cr
   \tab float MB476    \tab + \tab   -   \tab  +  \tab   + \tab    -  \tab  -  \tab   -  \tab  - \cr
   \tab TMLinterbank   \tab X \tab   X   \tab	  \tab     \tab       \tab     \tab	 \tab	 \cr
   \tab MB452adj       \tab   \tab	 \tab  X  \tab     \tab       \tab     \tab	 \tab	 \cr
   \tab MB473adj       \tab   \tab	 \tab	  \tab   X \tab       \tab     \tab	 \tab	 \cr
   \tab CUadj	       \tab   \tab	 \tab	  \tab   X \tab       \tab     \tab	 \tab	 \cr
}

 X- included\cr  
 + float is in\cr
 - float is out\cr

NonbankCheq	 = TMLCHEQPLUS + LCUCHEQPLUS\cr
NonbankNonCheq  = TMLNCPLUSPLUS + LCUNCPLUSPLUS\cr
NonbankTotal = NonbankTerm + NonbankCheq + NonbankNonCheq = MB2038M2P + MB2042\cr
[actually NonbankTerm= NonbankTotal - (NonbankCheq + NonbankNonCheq ) ]\cr

MB473adj ( = MB473PLUSPLUS - MB473 )adjustment is only for data prior 
to the sample used in Gilbert and Pichette.

CUadj  changes an estimate of NonbankNonCheq/NonbankTerm split prior 
to April 1996. This only affects M1++.

Many of the MB numbers are related to B numbers from Statistics Canada's Cansim series 
identification system. The correspondence between these related B numbers 
and V number
identifiers from the newer Statistics Canada Cansim II system is as follows:

\tabular{ccc}{
Cansim ID \tab Cansim II ID \tab \cr
B451 \tab V36814  \tab personal savings deposits\cr
B452 \tab V36815  \tab personal chequing deposits\cr
B453 \tab V36818  \tab personal non-chequing notice deposits\cr
B454 \tab V36823  \tab personal term deposits\cr
B472 \tab V36827  \tab non-personal chequing notice deposits\cr
B473 \tab V36828  \tab non-personal non-chequing deposits\cr
B475 \tab V36830  \tab non-personal term deposits\cr
B476 \tab V36809  \tab float \cr
B478 \tab V36831  \tab demand deposits \cr
B482 \tab V36876  \tab foreign currency deposits \cr
B486 \tab V36844  \tab personal chequing accounts (PCA)\cr
B487 \tab V36845  \tab current accounts deposits\cr
B2001 \tab V37173 \tab currency \cr
B2038 \tab V37235 \tab non-bank total deposits \cr
B2042 \tab V37239 \tab credit unions and caisses populaires total deposits\cr
B2046 \tab V37243 \tab life insurance \cr
B2047 \tab V37244 \tab deposits at government owned institutions\cr
B2048 \tab V37245 \tab money market mutual funds \cr
B2057 \tab V37255 \tab Canadian savings bonds (CSB)\cr
B2058 \tab V37256 \tab non-money market mutual funds \cr
}


The components in this database are not seasonally adjusted (SA), but the
corresponding Bank of Canada / Statistics Canada numbers for the seasonally 
adjusted aggregates are as follows:

\tabular{ccccc}{
         \tab           \tab  Monetary Aggregates \tab \tab \cr
         \tab         \tab             \tab   SA   \tab   SA     \cr
  	  \tab Cansim \tab Cansim II   \tab Cansim \tab Cansim II\cr
M1 total  \tab B2033  \tab V37200      \tab B1627  \tab V37124   \cr
M1 gross  \tab B2054  \tab V37252      \tab B1642  \tab V37141   \cr
M1+	  \tab B2060  \tab V37258      \tab B1651  \tab V37151   \cr
M1++	  \tab B2061  \tab V37259      \tab B1652  \tab V37152   \cr
M2	  \tab B2031  \tab V37198      \tab B1630  \tab V37128   \cr
M2+	  \tab B2037  \tab V37216      \tab B1633  \tab V37131   \cr
M2++	  \tab B2059  \tab V37257      \tab B1650  \tab V37150   \cr
M3	  \tab B2030  \tab V37197      \tab B1628  \tab V37125   \cr
}
}


\source{
	Components are from the \emph{Bank of Canada.}
	Population and consumer price index (CPI) data from 
	\emph{Statistics Canada} are used to calculate \code{realM1} 
	and  \code{percapitaM1}. 
}

\references{
	Gilbert, P.D. and L. Pichette (2003) Dynamic Factor Analysis for 
	   Measuring Money. Bank of Canada
	   Working Paper 2003-21. \url{http://www.bank-banque-canada.ca/pgilbert}.

	Kottaras, J. (2003) The Construction of Continuity-Adjusted Monetary
	   Aggregate Components. Bank of Canada Working Paper 2003-22.
	   \url{http://www.bank-banque-canada.ca}
}

\examples{
 require("tframe")
 data("CanadianMoneyData", package="CDNmoney")
########### Calculations to get monetary aggregates ######

 M1gross <-  tframed(MB2001 + MB486 + MB487p + TMLinterbank, names="gross M1 (B2054)")
 M1p     <-  tframed(MB2001 + MB486 + MB487p + MB452 + MB452adj + MB472
   + NonbankCheq, names="M1+ (B2060)")
 M1pp    <-  tframed(CUadj + M1p   + MB453 + MB473 + MB473adj + NonbankNonCheq,
    names="M1++ (B2061)")
 M2      <-  tframed(M1total + MB472 + MB473 + MB452 + MB453 + MB454, names="M2 (B2031)")
 M2p     <-  tframed(M2    + NonbankCheq + NonbankNonCheq + NonbankTerm  
                           + MB2046 + MB2047 + MB2048, names="M2+ (B2037)")
 M2pp    <-  tframed(M2p   + MB2057 + MB2058, names="M2++ (B2059)")
 M3      <-  tframed(M2    + MB475  + MB482, names="M3 (B2030)")

######### Calculations of cpi and pop ######

 #  M1real = M1total * 100/p100000  (CPI - p20 Bank of Canada Weekly Financial
 #	 Statistics, June 1992=100)
 #  M1PerCapita = M1total * 100 /(pop * p100000) # using a quarterly population
 #       series converted to monthly using spline.
 
 cpi <- 100 * M1total / M1real
 seriesNames(cpi) <- "CPI"
 
 popm <- M1total / M1PerCapita
 seriesNames(popm) <- "Population of Canada"

######### Plot aggregates #####

 tfplot(tbind(M1total, M1gross, M1p, M1pp))
 tfplot(tbind(M1PerCapita, M1real))
 tfplot(tbind(M2, M2p, M2pp))
 tfplot(M3)

#### Calculations to get components as used in Gilbert and Pichette ####

z <-tfwindow(tframed(tbind(
     MB2001,
     MB486 + MB452 ,
     NonbankCheq,
     MB472 + MB473 + MB487p,
     MB475,
     NonbankNonCheq + MB454 + NonbankTerm + MB2046 + MB2047 + MB2048 + 
            MB2057 + MB2058 + MB482 + MB453),
     names=c("currency", "personal cheq.", "NonbankCheq", 
             "N-P demand & notice", "N-P term", "Investment")
   ), start=c(1986,1), end=c(2002,4))

MBcomponents <- 1e8 * z /matrix(tfwindow(popm * cpi, start=c(1986,1),
                               end=c(2002,4)),196, 6)
# 1e8 * gives real $ per person
#(MB numbers in millions, CPI in fraction*100, popm in persons.)

tfplot(MBcomponents, graphs.per.page=3)
}

\seealso{
	\code{\link{CanadianCreditData}},
	\code{\link[tframe]{tframe}in the dse bundle of packages}
}
\keyword{datasets}
