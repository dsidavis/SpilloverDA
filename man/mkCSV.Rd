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
mkCSV(eco, xlsxFile = character(),
      obs = grep(gsub("rds", "", basename(f)), species$PDF, fixed = TRUE),
      species = NULL, # the data.frame from Species.csv
      spVar = c("Country", "State", "City", "Location", "Region"),
      spFixed = c("reference_ID", "PDF"),
      jvar = getLocation,
      write = TRUE,
      jdata = jvar(eco))
     
}
\arguments{
  \item{eco}{
	a data frame with each row corresponding to a term extracted
	from the PDF document.
      }
  \item{xlsxFile}{
	the file name to which the results are written as an XLSX file.
  }      
  \item{obs}{
	The rows in the Species.csv which correspond to the document in 
  }
  \item{species}{
	the \code{data.frame} of Species.csv
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
}
\value{}
\author{
Duncan Temple Lang  
}

\seealso{
}
\examples{
}
\keyword{IO}
