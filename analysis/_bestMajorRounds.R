setwd('C:/Users/sirallen/Dropbox/projects/golfStats/data/')
library(data.table)
library(ggplot2)

source('../_getOWGRhistorical.R')
source('../analysis/_functions.R')
source('../analysis/_setggtheme.R')


loadEventsInfo()
# How many events per year, by Tour? (some events have multiple
# tour affiliations)
eventsInfo[, table(unlist(Tour), unlist(mapply(rep, Year, lengths(Tour))))]

# Scores available since 2007 (2001 for majors)
majors = readEvents(tour='MAJ', year=2001:2017)

majorStats = rbindlist(lapply(majors, function(d) {
  d[!Pos %in% c('WD','MC','DQ'), .(
    Year = attr(d, 'eventYear'),
    Event = attr(d, 'eventName'),
    Winner = Name[1],
    Score = Agg[1],
    RelToField.mean = Agg[1] - mean(Agg[-1]),
    RelToField.median = Agg[1] - median(Agg[-1]),
    Margin = Agg[2] - Agg[1],
    # Most impressive individual rounds (RelToField stats computed only for
    # players who made the cut). Next step is to get scores rel to par
    R1.LowRound.Score  = min(R1),
    R1.LowRound.Players = list(Name[R1==min(R1)]),
    R1.LowRound.RelToField.mean = min(R1) - mean(R1[-which.min(R1)]),
    R1.LowRound.RelToField.median = min(R1) - median(R1[-which.min(R1)]),
    R1.LowRound.Margin   = min(R1) - min(R1[-which.min(R1)]),
    R2.LowRound.Score  = min(R2),
    R2.LowRound.Players = list(Name[R2==min(R2)]),
    R2.LowRound.RelToField.mean = min(R2) - mean(R2[-which.min(R2)]),
    R2.LowRound.RelToField.median = min(R2) - median(R2[-which.min(R2)]),
    R2.LowRound.Margin   = min(R2) - min(R2[-which.min(R2)]),
    R3.LowRound.Score  = min(R3),
    R3.LowRound.Players = list(Name[R3==min(R3)]),
    R3.LowRound.RelToField.mean = min(R3) - mean(R3[-which.min(R3)]),
    R3.LowRound.RelToField.median = min(R3) - median(R3[-which.min(R3)]),
    R3.LowRound.Margin   = min(R3) - min(R3[-which.min(R3)]),
    R4.LowRound.Score  = min(R4),
    R4.LowRound.Players = list(Name[R4==min(R4)]),
    R4.LowRound.RelToField.mean = min(R4) - mean(R4[-which.min(R4)]),
    R4.LowRound.RelToField.median = min(R4) - median(R4[-which.min(R4)]),
    R4.LowRound.Margin   = min(R4) - min(R4[-which.min(R4)])
  )]
}))

majorStats.LowRounds = melt.data.table(
  majorStats, id.vars=c('Year','Event'),
  measure.vars=patterns('\\.Score','\\.Players','\\.RelToField\\.mean',
                        '\\.RelToField\\.median','\\.Margin'),
  variable.name='Round',
  value.name=paste0('LowRound.', c('Score','Players','RelToField.mean',
                                   'RelToField.median','Margin'))
)

majorStats.LowRounds[, Event:= factor(Event, levels=c('Masters Tournament',
                                                      'U.S. Open',
                                                      'The Open Championship',
                                                      'PGA Championship'))]

# Shorten the player names
majorStats.LowRounds[, Players.label:=
                       sapply(LowRound.Players,
                              function(x) {
                                paste(abbreviateNames(x), collapse=', ') })]

majorStats.LowRounds[, Combined.label:=
                       paste0(Players.label, '\n', Year, ' ',
                              sub('The ', '', Event), '\nRound ', Round,
                              ' (', LowRound.Score, ')')]

majorStats.LowRounds[, Facet.label:=
                       paste0(Players.label, '\n', Year, '\nRound ', Round,
                              ' (', LowRound.Score, ')')]

majorStats = majorStats[, 1:7]

ggplot(majorStats, aes(x=RelToField.median, y=Margin)) +
  geom_jitter() +
  geom_jitter(aes(x=RelToField.mean), col='red')

# Best major rounds (relative to players who made the cut)
# Note: May be a "bias" toward rounds 3 and 4 since there is no limit to
# weekend scores; conversely, someone can play badly in rounds 1 and 2, but
# not *too* badly, otherwise he will miss the cut. How to correct for this?

# (a) All majors together
ggplot(
  majorStats.LowRounds[tail(order(-LowRound.RelToField.mean), 12)],
  aes(x=factor(Combined.label, levels=Combined.label),
      y=-LowRound.RelToField.mean)) +
  geom_col(fill='firebrick', width=.6) +
  scale_y_continuous(breaks=seq(0,10,2)) +
  coord_flip() +
  labs(x='', y='',
       title='Best Rounds at Major Championships (since 2001)',
       subtitle='Strokes better than the field average') +
  ggsave('../figure/BestRounds.pdf', dev='pdf', width=8, height=12) +
  ggsave('../figure/BestRounds.png', dev='png', width=8, height=12, dpi=150)

# (b) Facet by major
ggplot(
  majorStats.LowRounds[order(-LowRound.RelToField.mean)][
    , tail(.SD, 6), by='Event'] %>%
    setnames(sub('LowRound.RelToField.', '', names(.))) %>%
    melt(id.vars=c('Year','Event','Round','Facet.label'),
         measure.vars=c('mean','median'), variable.name='Measure'),
  aes(x=factor(Facet.label, levels=unique(Facet.label)),
      y=-value, group=factor(Measure, levels=c('median','mean')),
      alpha=Measure)) +
  geom_col(aes(fill=Event), position='dodge', width=.6) +
  scale_y_continuous(breaks=seq(0,10,2)) +
  scale_fill_manual(values=c('#076652', 'navyblue', 'goldenrod', 'firebrick')) +
  scale_alpha_manual(values=c(.9, .5)) +
  coord_flip() +
  facet_wrap(~ Event, scales='free_y', nrow=1) +
  labs(x='', y='',
       title='Best Rounds at Major Championships (since 2001)',
       subtitle='Strokes better than field mean (top) and median (bottom)') +
  theme(legend.position='none',
        strip.background = element_blank()) +
  ggsave('../figure/BestRounds_Facet.pdf', dev='pdf', width=12, height=6) +
  ggsave('../figure/BestRounds_Facet.png', dev='png', width=12, height=6,
         dpi=150)



