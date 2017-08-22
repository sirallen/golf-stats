setwd('C:/Users/sirallen/Dropbox/projects/golfStats/data/')
library(data.table)
library(ggplot2)

source('../analysis/_functions.R')
source('../analysis/_setggtheme.R')


load('OWGR/owgr_MASTER.RData')
# Ranking is determined by AVGPTS, so use this; but could also do another
# version with TOTPTS; should suffice to change this line and some of the plot
# settings below
setnames(master, 'AVGPTS', 'Points')

master = master[order(week, -Points)][week > '2007_45']

pointsGap = master[, .(
  No1 = Name[1],
  No2 = Name[2],
  No1Points = Points[1],
  AbsGap = Points[1] - Points[2],
  RelGap = Points[1]/Points[2]),
  by='week']

pointsGap[, `:=`(
  date = week_to_date(week),
  newWNO = as.numeric(No1!=shift(No1, fill='')),
  stretch = cumsum(No1!=shift(No1, fill='')),
  No1 = factor(No1, unique(No1))
)]

stretches = pointsGap[, .(Player=No1[1], start=date[1], maxGap=max(AbsGap),
                          maxPoints=max(No1Points), length=.N),
                      by='stretch']

# A stretch ends when a new one begins
stretches[, end:= shift(start, type='lead')]

# Set the 'end' of the current WNO's stretch to next week
stretches[nrow(stretches), end:= pointsGap[, tail(date, 1) + 7]]

setcolorder(stretches, c(1,2,6,3,7,5,4))

# Add labels with player initials and duration of stretch (number
# of weeks before ascension of new WNO)
stretches[length >= 10, label:= paste0(
  abbreviate(Player, 2), ' (', ifelse(.I==1, '>', ''), length,
  ifelse(.I==1, ' weeks', ''), ')')]



ggplot(pointsGap, aes(x=date, y=AbsGap)) +
  geom_line() +
  geom_rect(data=stretches, inherit.aes=FALSE,
            aes(xmin=start, xmax=end, ymin=0, ymax=maxGap, #ymax=maxPoints,
                fill=Player), alpha=.4) +
  geom_text(data=stretches,
            aes(x=start, y=maxGap, #ymax=maxPoints,
                label=label), hjust=0, vjust=-.5, size=2.5) +
  labs(x='Data: www.owgr.com', y='',
       title='How Dominant Is the World\'s Number One Golfer?',
       #subtitle='OWGR Average Points') +
       subtitle='Points Gap (difference between No. 1 and No. 2, OWGR average points)') +
  scale_y_continuous(breaks=seq(0,24,4)) +
  scale_x_date(date_minor_breaks='1 year') +
  ggsave('../figure/WNO_Dominance.pdf', dev='pdf', width=12, height=6) +
  ggsave('../figure/WNO_Dominance.png', dev='png', width=12, height=6, dpi=150)

