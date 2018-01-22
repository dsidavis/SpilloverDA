\name{mkCSV}
\alias{mkCSV}
\title{
  Create a CSV or XLSX file from the extracted results from xml2eco
}
\description{
  Takes the results from extracting keywords from the text, and writes
  then into a CSV or XLSX file to be reviewed. 
}
\usage{
mkCSV(f, obs = grep(gsub("rds", "", basename(f)), species$PDF, fixed = TRUE), species = sp, eco = readRDS(f), spVar = c("Country", "State", "City", "Location", "Region"), spFixed = c("reference_ID", "PDF"), jvar = getLocation, write = TRUE, jdata = jvar(eco), xlsxFile = outFilename(f))
}
\arguments{
  \item{f}{
	the file path to the extracted and saved RDS file from xml2eco
  }
  \item{obs}{
	The rows in the Species.csv which correspond to the document in f
  }
  \item{species}{
	the \code{data.frame} of Species.csv
  }
  \item{eco}{
	the extracted terms in a list.
  }
  \item{spVar}{
	the variables in Species.csv which are of interest in the comparison
	CSV or XLSX.
  }
  \item{spFixed}{
	the fixed variables, i.e, the reference variables which identify the
	rows of Species.csv. 
  }
  \item{jvar}{
	the function to call to extract the items from the ecoextract results
  }
  \item{write}{
	logical, whether to write the extracted terms to a file
  }
  \item{jdata}{
	the extracted data items from the ecoextract results
}
  \item{xlsxFile}{
	the file name of the saved XLSX
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
Duncan Temple Lang
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (f, obs = grep(gsub("rds", "", basename(f)), species$PDF, 
    fixed = TRUE), species = sp, eco = readRDS(f), spVar = c("Country", 
    "State", "City", "Location", "Region"), spFixed = c("reference_ID", 
    "PDF"), jvar = getLocation, write = TRUE, jdata = jvar(eco), 
    xlsxFile = outFilename(f)) 
{
    fx = sp[obs, c(spFixed, spVar)]
    fx = fx[!duplicated(fx), ]
    ans = jdata
    n = nrow(ans)
    fx = as.data.frame(lapply(fx, function(x) c(x, rep("", n - 
        nrow(fx)))), stringsAsFactors = FALSE)
    fx$PDF = gsub("internal-pdf://", "", fx$PDF)
    ans = cbind(fx, section = rep("", n), correct = rep(FALSE, 
        n), ans)
    w = sapply(ans, is.character)
    ans[w] = lapply(ans[w], function(x) gsub("\031", "'", x))
    if (write) {
        createXLSX(ans, xlsxFile)
    }
    ans
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
