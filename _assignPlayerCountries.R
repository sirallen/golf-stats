setwd('C:/Users/sirallen/Dropbox/projects/golfStats/data/')
library(data.table)
library(stringr)

sortFreq = function(x) {
  names(sort(table(x), decr=TRUE))
}

# Learn the country <--> abbreviation mapping and assign consistent country
# label to each player
country = fread('OWGR/all_weeks/owgr_COUNTRY.csv', na='')

# The format of a player's country/nationality is not consistent
# over time (sometimes ISO, sometimes full name, or other variants);
# Take the ISO to be the most common 3-letter string (if there is
# one) in a player's history and likewise the full country name
# the most common >3-letter string. Careful: Sometimes these do not
# correspond -- e.g., T&T paired with Canada (Stephen Ames?)
country[, ISO:= list( apply(country[, -1], 1, function(r) {
  c(sortFreq(toupper(r[!is.na(r) & nchar(r)==3])), NA)[1] }))]

country[, full:= list( apply(country[, -1], 1, function(r) {
  c(sortFreq(str_to_title(r[!is.na(r) & nchar(r) > 3])), NA)[1] })) ]

# Create the mapping using the most common full name associated with
# each ISO, across players. Careful: Ireland is both IRE and IRL
mapping = country[!is.na(ISO), .(full = sortFreq(full)[1]), by='ISO']

# First modify full country names if appropriate
mapping[ISO=='TWN', full:= 'Chinese Taipei']
mapping[ISO=='VEN', full:= 'Venezuela']
mapping[ISO=='HOL', full:= 'Netherlands']

# Now fill in country name for players which had ISO but missing country name,
# or one that wasn't the most common for that ISO
country[mapping, on='ISO', full:= i.full]

# Fill in ISO for players which have country name but missing ISO
country[mapping, on='full', ISO:= ifelse(is.na(ISO), i.ISO, ISO)]

# Save and fill in any missing entries manually
fwrite(country[, .(Name, ISO, Country=full)], 'OWGR/PlayerCountries.csv',
       quote=TRUE)

# To do -- did any players switch countries/ (e.g., Graeme McDowell)

rm(country, mapping, sortFreq)
