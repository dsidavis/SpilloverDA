\name{xml2eco}
\alias{xml2eco}
\title{XML to EcoJSON}
\description{
Takes an XML file from a PDF created using pdftohtml, and runs the
augmented keyword extractor on it. The intermediate text, broken into
sections, and the results can be cached to disk.
  %%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
xml2eco(XML, ecoextractLoc = getEcoExtractPyScript(), results_dir = "ecoJSON", cache.dir = "sections")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{XML}{XML as created by \code{convertPDF2XML}
}
  \item{ecoextractLoc}{the file path to the ecoextract.py
	script. Defaults to the included script in python/ecoextract.py.
}
  \item{results_dir}{
	optional directory to save the results into. Will be created if it
	does not exist.
  }
  \item{cache.dir}{
	optional directory to save the intermediate text, broken into
	sections.
}
}
\details{
  }
\value{
  A list representation of the JSON object returned by ecoextract.py
  \item{location}{}
  \item{date}{}
  \item{resolved_keyword}{}
  \item{txt}{original text}
}


\references{
}
\author{
Matt Espe
}
\note{
}


\seealso{
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (XML, ecoextractLoc = getEcoExtractPyScript(), results_dir = "ecoJSON", 
    cache.dir = "sections") 
{
    if (length(cache.dir) && !is.na(cache.dir)) {
        if (!dir.exists(cache.dir)) 
            dir.create(cache.dir)
        cache.file = file.path(cache.dir, gsub("xml$", "rds", 
            basename(XML)))
        if (exists(cache.file)) 
            ll = readRDS(cache.file)
        else {
            ll = readXMLSections(XML)
            saveRDS(ll, file = cache.file)
        }
    }
    else {
        ll = readXMLSections(XML)
    }
    ll = lapply(ll, function(x) gsub("Â|\"", "", x))
    i = grep("references?|cited|acknowledgements?|funding|interests|disclosure", 
        names(ll), ignore.case = TRUE, invert = TRUE)
    ll = ll[i]
    ans = lapply(ll, function(x, eco) try(sect2eco(x, ecoextract = eco)), 
        eco = ecoextractLoc)
    if (length(results_dir) && !is.na(results_dir)) {
        if (!dir.exists(results_dir)) 
            dir.create(results_dir)
        saveRDS(ans, file.path(results_dir, gsub("xml$", "rds", 
            basename(XML))))
    }
    ans
  }
}
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
