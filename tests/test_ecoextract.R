# Test that the ecoextract mechanism is recovering known terms
# from a short piece of text

library(SpilloverDA)

sample_text = list(title = "My PCR experiment in China",
                   abstract = "This is just some text which will be used as filler. There is a diagnostic test nested-PCR mentioned here, as well as a species Rattus rattus. There is no mention of country names or the virus. If I add antibodies, I should also recover this.")

ans = doc2eco(section.text = sample_text)

getDiagTest(ans)
getCountry(ans)
getVirus(ans)
getSpecies(ans)
