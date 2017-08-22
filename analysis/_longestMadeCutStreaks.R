setwd('C:/Users/sirallen/Dropbox/projects/golfStats/data/')
library(data.table)
library(ggplot2)

source('../_getOWGRhistorical.R')
source('../analysis/_functions.R')
source('../analysis/_setggtheme.R')


### Longest streaks of cuts made at majors
majors = readEvents(tour='MAJ', select=c('Pos','Name','playerId'))

data = rbindlist(majors, idcol='majorIdx')[
  !grepl('(M|m)issed', Name)]

# Check -- all playerIds have exactly one corresponding player name
# (and vice versa). So it doesn't matter whether I use 'Name' or 'playerId'
# for the by-groupings below
table(data[, uniqueN(Name), by='playerId']$V1)
table(data[, uniqueN(playerId), by='Name']$V1)

# MC indicator. What about WD or DQ after making the cut?
data[, MC:= ifelse(Pos %in% c('MC','WD','DQ'), 1, 0)]

# counter for the kth major that a player competed in
data[, playerMajorIdx:= 1:.N, keyby='Name']

# Each player's longest streak of cuts made at majors, since 1986. 'Start'
# is the playerMajorIdx where the streak started and 'end' the first
# time after 'start' that player missed the cut; NA if streak still going!
longestStreak = data[, rle(MC), by='Name'][
  , `:=`(start = shift(cumsum(lengths), fill=0) + 1,
         end   = c(cumsum(lengths)[-.N] + 1, NA)), by='Name'][
           values==0, .SD[which.max(lengths)], by='Name'][
             lengths > 5][
               order(-lengths)]


longestStreak = data[, .(Name, playerId, majorIdx, start=playerMajorIdx)][
  longestStreak, on=.(Name, start)][
    , values:= NULL]


