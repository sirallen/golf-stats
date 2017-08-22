setwd('C:/Users/sirallen/Dropbox/projects/golfStats/data/')
library(data.table)
library(rvest)
library(stringr)
library(httr)

getRedirectTarget = function(u) {
  base_url = 'https://en.wikipedia.org/w/api.php'
  page_title = sub('/wiki/', '', URLdecode(u))
  
  x = GET(base_url, query = list(action='query', titles=page_title,
                                 redirects=NA, format='json'))
  
  target = content(x)$query$redirects[[1]]$to
  
  if (length(target) > 0) {
    target = target %>%
      str_replace_all(' ', '_') %>%
      URLencode(reserved=TRUE)
    
    return(paste0('/wiki/', target))
    
  } else {
    return(u)
  }
}


sortFreq = function(x) {
  names(sort(table(x), decr=TRUE))
}

firstSunday = function(yr) {
  firstWeek = lapply(as.Date(paste0(yr, '-01-01')), '+', 0:6)
  sapply(firstWeek, function(w) which(weekdays(w)=='Sunday'))
}

formatPar = function(x) {
  ifelse(x > 0, paste0('+', x),
         ifelse(x==0, 'E', as.character(x)))
}

scrapePgaTourEvents = function(years=1960:2017) {
  base_url = 'https://en.wikipedia.org/wiki/'
  result = list()
  
  for (yr in as.character(years)) {
    url = paste0(base_url, yr, '_PGA_Tour')
    
    tryCatch({
      html = read_html(url)
      
      invisible({
        xml_remove(html_nodes(html, xpath='//table[@role="presentation"]'))
        xml_remove(html_nodes(html, xpath='//table[1]/tr/td[4]//
                              span[@class="flagicon"]'))
        xml_remove(html_nodes(html, xpath='//sup[@class="reference"]'))
      })
      
      eventTable = html %>%
        html_nodes(xpath='//table[1]') %>%
        .[[1]] %>%
        html_table()
      
      # Note: the tournament url might redirect to another page (the most
      # recent tournament name)
      eventTable$TournamentURL = html %>%
        html_nodes(xpath='//table[1]/tr/td[2]//a') %>%
        html_attr('href')
      
      # column of type 'list' (team events have multiple winners)
      eventTable$PlayerURL = html %>%
        html_nodes(xpath='//table[1]/tr/td[4]') %>%
        lapply(html_nodes, xpath='.//a') %>%
        lapply(html_attr, 'href')
      
      eventTable$Season = as.numeric(yr)
      
      tryCatch({
        setnames(eventTable, 'Winner\'s\nshare ($)', '1st prize ($)')
        setnames(eventTable, grep('OWGR', names(eventTable)), 'OWGR points')
        setnames(eventTable, 'Week', 'Date')
      },
      error = function(e) NULL)
      
      result[[yr]] = eventTable
    },
    error = function(e) message(e)
    )
  }
  
  pgaEvents = rbindlist(result, fill=TRUE)
  
  # convert en- and em-dash to hyphen
  pgaEvents[, Score:= gsub('â..', '-', iconv(Score))]
  
  # get the calendar year (PGA Tour seasons start in previous calendar year's
  # Fall since 2014 season) and calendar week
  pgaEvents[, Week:= week(as.Date(paste(Season, Date), format='%Y %b %d'))]
  pgaEvents[, Year:= Season - (cummin(Week) > 3), by='Season']
  # 'Week' is measured from the first Sunday of the Year *after* New Year's
  # (except in 2000)
  pgaEvents[, Week:= round((yday(as.Date(paste(Year, Date), format='%Y %b %d')) -
                            firstSunday(Year))/7 +
                             ifelse(firstSunday(Year)==1, 0,
                                    ifelse(Year==2000, 0, 1)))]
  
  pgaEvents[grepl('(C|c)ancelled', Winner), Notes:= 'Cancelled']
  pgaEvents[Notes=='Cancelled', PlayerURL:= as.list(rep(NA, .N))]
  pgaEvents[Notes=='Cancelled', c('Winner','Score','1st prize ($)','Purse ($)',
                                  'OWGR points'):= NA_character_]
  
  pgaEvents[grepl('Masters|U.S._Open|(Open|PGA)_Championship', TournamentURL),
            Notes:= 'Major championship']
  pgaEvents[grepl('Open_Championship', TournamentURL),
            Tournament:= 'The Open Championship']
  
  pgaEvents[is.na(Notes) & grepl('WGC', TournamentURL), Notes:=
              'World Golf Championships']
  
  pgaEvents[, Tour:= 'PGA']
  pgaEvents[Notes=='Major championship', Tour:= 'MAJ']
  pgaEvents[Notes=='World Golf Championships', Tour:= 'WGC']
  
  cat('Retrieving TournamentURL redirects...\n')
  for (u in pgaEvents[, unique(TournamentURL)]) {
    pgaEvents[TournamentURL==u, TournamentRedirectURL:= getRedirectTarget(u)]
  }
  
  setcolorder(pgaEvents, c(9, 14, 15, 13, 1:6, 10:12, 7, 16, 8))
  
  suppressWarnings(
    pgaEvents[, `OWGR points`:= as.numeric(`OWGR points`)]
  )
  
  fwrite(pgaEvents, 'wikipedia/pgaEvents.csv', quote=T)
}


loadPgaEvents = function() {
  # Load pgaEvents.csv into .GlobalEnv. Function is for convenience
  # since columns of type 'list' need converting via strsplit
  pgaEvents = fread('wikipedia/pgaEvents.csv', na='')
  
  pgaEvents[, PlayerURL:=  strsplit(gsub('\"', '', PlayerURL), '\\|')]
  
  assign('pgaEvents', pgaEvents, pos=1)
}


scrapeMajors = function() {
  urls = c('https://en.wikipedia.org/wiki/Masters_Tournament',
           'https://en.wikipedia.org/wiki/U.S._Open_(golf)',
           'https://en.wikipedia.org/wiki/The_Open_Championship',
           'https://en.wikipedia.org/wiki/PGA_Championship')
  
  result = list()
  
  for (url in urls) {
    tourn = tail(strsplit(url, '/')[[1]], 1) %>%
      str_replace_all('_', ' ') %>%
      str_replace(' \\(golf\\)', '')
    
    wikitable = url %>% read_html() %>%
      html_nodes(xpath='//h2/span[@id="Winners" or @id="Champions"]') %>%
      xml_parent() %>%
      html_node(xpath='./following-sibling::table[1]') %>%
      .[[1]]
    
    table = wikitable %>%
      html_table() %>%
      setDT()
    
    table = table[!grepl('(c|C)ancelled|No Championship', Champion)]
    table[, Year:= as.integer(Year)]
    table[, Tournament:= tourn]
    table[, `Runner(s)-up`:= NULL]
    
    setnames(table, names(table), gsub('\n', ' ', names(table)))
    setnames(table, names(table), gsub('\\[.*\\]', '', names(table)))
    
    if ('Location of venue' %in% names(table)) {
      setnames(table, 'Location of venue', 'Location') }
    if ('Winning score' %in% names(table)) {
      setnames(table, 'Winning score', 'Score') }
    if ('Margin of victory' %in% names(table)) {
      setnames(table, 'Margin of victory', 'Margin') }
    if ('Winning margin' %in% names(table)) {
      setnames(table, 'Winning margin', 'Margin') }
    if ('Dates' %in% names(table)) table[, Dates:= NULL]
    
    if (tourn=='Masters Tournament') {
      table[, `:=`(Venue = 'Augusta National Golf Club',
                   Location = 'Augusta, Georgia',
                   CourseURL = '/wiki/Augusta_National_Golf_Club',
                   `To par` = iconv(`To par`) %>%
                     str_replace('â..', '-') %>%
                     str_replace(' !E', '') %>%
                     as.numeric())]
      
      table[, Strokes:= 288 + `To par`]
      
    } else {
      table[, `:=`(`To par` = Score %>% str_extract('\\(.*\\)') %>%
                     str_replace_all('\\(|\\)', '') %>%
                     iconv() %>%
                     str_replace('â..', '-') %>%
                     str_replace('E', '0') %>%
                     as.numeric(),
                   
                   Strokes = Score %>% str_extract('\\d{3}') %>%
                     as.numeric())]
      
      venueCol = wikitable %>%
        html_nodes(xpath='tr/th') %>%
        html_text() %>%
        match(x='Venue')
      
      # Some cells have two <a>'s (Bethpage State Park, Bethpage Black Course);
      # only select a[1]
      table$CourseURL = wikitable %>%
        html_nodes(xpath=paste0('./tr/td[', venueCol, ']/a[1]')) %>%
        html_attr('href')
    }
    
    table[table==''|table=='-'] = NA
    
    result[[tourn]] = table
  }
  
  result = rbindlist(result, fill=TRUE)
  
  result[, Location:= gsub('\\[.*\\]', '', Location)]
  
  result[, Score:= ifelse(is.na(`To par`), as.character(Strokes),
                         paste0(Strokes, ' (', formatPar(`To par`), ')'))]
  
  for (u in result[, unique(CourseURL)]) {
    result[CourseURL==u, CourseRedirectURL:= getRedirectTarget(u)]
  }
  
  result[, CourseName:= CourseRedirectURL %>%
           str_replace('/wiki/', '') %>%
           str_replace_all('_', ' ')]
  
  setcolorder(result, c('Year','Tournament','Champion','Country','Score',
                       'Strokes','To par','Margin','Venue','Location',
                       grep('Winner\'s share', names(result), v=T),
                       'CourseURL','CourseRedirectURL','CourseName'))
  
  setkey(result, Year, Tournament)
  
  fwrite(result, 'wikipedia/majors.csv', quote=T)
}

scrapeCourseInfo = function() {
  # Now scrape each (unique) TournamentURL
  # Also get list of tournament name variants
  if (!exists('pgaEvents', 1)) loadPgaEvents()
  
  pgaEvents[, TournamentRedirectURLNoYear:=
              sub('\\d{4}_', '', TournamentRedirectURL)]
  
  tournamentURLs = pgaEvents[is.na(CourseName) & Tour!='MAJ',
                             unique(TournamentRedirectURL)]
  
  i = 0
  for (u in rev(tournamentURLs)) {
    if (i %% 25 == 0) {
      cat('Scraped', i, 'of', length(tournamentURLs), 'urls...\n')
    }
    url = paste0('https://en.wikipedia.org', u)
    
    if (nrow(pgaEvents[is.na(CourseName) & TournamentURL==u]) == 0) {
      i = i + 1
      next
    }
    
    tryCatch({
      html = read_html(url)
      
      # Get the side panel with course information (Note: Don't use this;
      # usually only records info for the most recent vintage of the tournament)
      infobox = html %>%
        html_node(xpath='//table[@class="infobox vcard" or 
                  @class="infobox vevent"]') %>%
        html_nodes(xpath='.//th[@scope="row"]') %>%
        xml_parent()
      
      winners = html %>%
        html_nodes(xpath='//h2/span[@id="Winners" or @id="Champions"]') %>%
        xml_parent() %>%
        html_node(xpath='./following-sibling::table[1]') %>%
        .[[1]] %>%
        html_table()
      
      setDT(winners)
      
      winners = winners[grepl('\\d{4}$', Year)]
      winners[, Year:= as.integer(Year)]
      
      if ('Venue' %in% names(winners)) setnames(winners, 'Venue', 'Course')
      if ('Host club' %in% names(winners)) setnames(winners,
                                                    'Host club', 'Course')
      # Tournaments with multiple names
      redirects = getRedirects(u)
      
      for (redirect in redirects) {
        winners[, TournamentURL:= redirect]
        pgaEvents[winners, on=.(Year, TournamentURL), CourseName:= Course]
      }
      
    },
    error=function(e) message(e)
    )
    
    i = i + 1
  }
  
  pgaEvents[, CourseName:= gsub('\n', ' ', CourseName)]
  
  # CourseURL is more consistent than CourseName; so assign the most
  # common name
  #pgaEvents[, CourseName:= sortFreq(CourseName)[1], by='CourseURL']
  
}

#source('C:/Users/sirallen/Dropbox/projects/golfStats/_pgaEventsAddCourses.R')

# Merging with OWGR eventsInfo
loadPgaEvents()
loadEventsInfo()

eventsInfo = eventsInfo[
  sapply(Tour, function(x) any(c('USA','MAJ','WGC') %in% x))][
    , Tour:= sapply(Tour, function(x) {
      if ('MAJ' %in% x) 'MAJ' else if ('WGC' %in% x) 'WGC' else 'PGA'})]

# within-week index (alphabetical) for events with same year, week, tour
pgaEvents[, w_i:= order(Tournament), by=.(Year,Week,Tour)]
eventsInfo[, w_i:= order(EventName), by=.(Year,Week,Tour)]

setkeyv(pgaEvents, c('Year','Week','Tour','w_i'))
setkeyv(eventsInfo, c('Year','Week','Tour','w_i'))

merged = merge(pgaEvents[Year>=1986 & (is.na(Notes) | Notes!='Cancelled'),
                         .(Year, Week, Tour, w_i, Tournament)],
               eventsInfo[, .(Year, Week, Tour, w_i, EventName)],
               all=TRUE)
# count overlapping words
merged[, overlap:= Map(function(a,b) length(intersect(tolower(a), tolower(b))),
                       strsplit(Tournament, ' |-'), strsplit(EventName, ' |-'))]

pgaEvents[eventsInfo, eventId:= eventId]

majors = fread('wikipedia/majors.csv')
majors[, table(CourseName, Tournament)]

stopifnot(
  pgaEvents[majors, on=.(Year, Tournament), sum(Score!=i.Score, na.rm=T)] == 0
)

# Fill in missing 'Score' information in pgaEvents (majors only)
pgaEvents[majors, on=.(Year, Tournament), Score:= i.Score]


