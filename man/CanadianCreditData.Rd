\name{CanadianCreditData}
\docType{data}
\alias{CanadianCreditData}
\alias{CanadianCreditData.asof.3Mar2006}
\alias{CanadianCreditData.asof.28Jan2005}
\alias{TotalCredit}
\alias{ConsumerCredit}
\alias{ResidentialMortgage}
\alias{ShortTermBusinessCredit}
\alias{OtherBusinessCredit}

\title{Canadian Credit Aggregates}
\description{Canadian credit aggregates.}

\usage{
	data(CanadianCreditData)
	data(CanadianCreditData.asof.3Mar2006)
	data(CanadianCreditData.asof.28Jan2005)
}

\format{
	The objects are time series.
}

\details{
	Several data objects are loaded, as listed in the table below. The 
	\code{CanadianCreditData} usage loads the most recently version and 
	other usages loads data as of a given date.
	
	These data are the Canadian Credit aggregates in millions of Canadian
	dollars.
	

\tabular{lll}{
 variable (ID)   \tab short description    \tab longer description \cr 
   \tab \tab \cr 
 TotalCredit  	         \tab total credit	         \tab total household & business credit\cr
 ConsumerCredit  	 \tab consumer credit  	         \tab total consumer credit\cr
 ResidentialMortgage  	 \tab residential mortgage  	 \tab total residential mortgage  credit\cr
 ShortTermBusinessCredit \tab short term-business credit \tab total short-term business credit\cr
 OtherBusinessCredit  	 \tab other business credit  	 \tab total other business credit\cr
}


The components in this database are not seasonally adjusted (SA), but the
corresponding Bank of Canada / Statistics Canada Cansim II numbers for 
the seasonally adjusted aggregates are as follows:

\tabular{ccc}{
         \tab	unadjusted  \tab   SA	\cr
 TotalCredit  	         \tab v122644	 \tab v122648  \cr
 ConsumerCredit  	 \tab v122698	 \tab v122707  \cr
 ResidentialMortgage  	 \tab v122736	 \tab v122746  \cr
 ShortTermBusinessCredit \tab v122639	 \tab v122646  \cr
 OtherBusinessCredit  	 \tab v36412	 \tab          \cr
}
}


\source{
	Aggregates are from the \emph{Bank of Canada}
	and also available from  \emph{Statistics Canada}. 
}

\references{
	\emph{Bank of Canada Banking and Financial Statistics}. Table E2
	  \url{http://www.bank-banque-canada.ca}.
}

\examples{
 require("tframe")
 data("CanadianCreditData", package="CDNmoney")

 tfplot(TotalCredit, ConsumerCredit, ResidentialMortgage,
    ShortTermBusinessCredit, OtherBusinessCredit) 
 tfplot(tbind(TotalCredit, ConsumerCredit, ResidentialMortgage,
    ShortTermBusinessCredit, OtherBusinessCredit), graphs.per.page=3 ) 

 tfplot(diff(tbind(TotalCredit, ConsumerCredit, ResidentialMortgage,
    ShortTermBusinessCredit, OtherBusinessCredit)), graphs.per.page=3 ) 

 tfplot(tbind(TotalCredit, ConsumerCredit, ResidentialMortgage,
    ShortTermBusinessCredit, OtherBusinessCredit), graphs.per.page=3, 
    start=c(1990,6), end=c(1991,6)) 
}

\seealso{
	\code{\link{CanadianMoneyData}},
	\code{\link[tframe]{tframe}in the dse bundle of packages}
}
\keyword{datasets}
