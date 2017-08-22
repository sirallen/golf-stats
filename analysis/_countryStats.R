setwd('C:/Users/sirallen/Dropbox/projects/golfStats/data/')
library(data.table)
library(ggplot2)

source('../_getOWGRhistorical.R')
source('../analysis/_functions.R')
source('../analysis/_setggtheme.R')


load('OWGR/owgr_MASTER.RData')

countryStats = master[RANK <= 100, .(
  totalPoints   = sum(TOTPTS),
  totalAvgPoints = sum(AVGPTS),
  highestRank    = min(RANK),
  highestRankPlayer = Name[which.min(RANK)],
  nPlayers      = .N),
  by=.(week, COUNTRY)][
    order(week, -nPlayers, highestRank)]

stopifnot(
  # Will make this assumption below
  all(countryStats[, COUNTRY[1], by='week']$V1=='United States')
)

intlPres = countryStats[, .(
  nCountries   = .N,
  nIntlPlayers = sum(nPlayers[-1]),
  # the 3rd order() variable (highestRank) is the tiebreaker
  country2    = COUNTRY[2],
  nPlayers2   = nPlayers[2],
  nTies2      = sum(nPlayers==nPlayers[2])),
  by='week']

intlPres[week=='2010_04', (2:5):= intlPres[week=='2010_03', 2:5]]

intlPres[, `:=`(
  date = week_to_date(week),
  stretch = cumsum(country2!=shift(country2, fill=''))
)]

stretches = intlPres[, .(country2=country2[1], start=date[1], length=.N),
                     by='stretch']
stretches[, `:=`(
  country2 = factor(country2, unique(country2)),
  end = shift(start, type='lead')
)]

stretches[nrow(stretches), end:= intlPres[, tail(date, 1) + 7]]

setcolorder(stretches, c(1,2,4,3,5))

ggplot(intlPres, aes(x=date, y=100-nIntlPlayers)) +
  geom_step() +
  geom_smooth(span=.5, se=FALSE, lty=2, lwd=2, col='darkmagenta') +
  #geom_area(aes(y=nPlayers2), alpha=.4) +
  # geom_rect(data=stretches, inherit.aes=FALSE,
  #           aes(xmin=start, xmax=end, ymin=40, ymax=70,
  #               fill=country2), alpha=.4) +
  labs(x='Data: www.owgr.com', y='',
       title='A Rankings Resurgence for the United States?',
       subtitle='American players in the OWGR Top 100') +
  scale_x_date(date_breaks='4 years', date_minor_breaks='1 year',
               date_labels='%Y') +
  ggsave('../figure/intlPlayers100.pdf', dev='pdf', width=12, height=6) +
  ggsave('../figure/intlPlayers100.png', dev='png', width=12, height=6,
         dpi=150)

