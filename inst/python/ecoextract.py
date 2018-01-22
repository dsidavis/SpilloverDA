#!/usr/bin/env python3

import sys

from epitator.annotator  import AnnoDoc
from epitator.geoname_annotator import GeonameAnnotator
from epitator.date_annotator import DateAnnotator
from epitator.resolved_keyword_annotator import ResolvedKeywordAnnotator

import json

def main(in_file, out_file):
    f = open(in_file)
    txt = f.readlines()
    txt = ' '.join(txt)
    adoc = AnnoDoc(txt)
    adoc.add_tiers(GeonameAnnotator())
    adoc.add_tiers(DateAnnotator())
    adoc.add_tiers(ResolvedKeywordAnnotator())
    # print(vars(adoc.tiers['geonames']))
    # print(vars(adoc.tiers['dates']))
    # print(vars(adoc.tiers['resolved_keywords']))
    
    # Location
    geo = [x.to_dict() for x in adoc.tiers['geonames'].spans]
    # Date
    dates = [str(x) for x in adoc.tiers['dates'].spans]
    # Other
    other = [x.to_dict() for x in adoc.tiers['resolved_keywords'].spans]

    d = {'location': geo, 'date': dates, 'resolved_keyword': other}

    with open(out_file, 'w') as dest:
        json.dump(d, dest)


    
if __name__ == "__main__":
   main(sys.argv[1], sys.argv[2])

