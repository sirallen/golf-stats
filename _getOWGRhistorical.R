setwd('C:/Users/sirallen/Dropbox/projects/golfStats/data/')
library(data.table)
library(rvest)
library(stringr)

allWeeks = function(after=2007) {
  # return a vector of year/wk : yyyy_ww from 'after' to present
  all_wks = expand.grid(str_pad(1:52, 2, 'left', '0'), after:2017)
  all_wks = mapply(paste, all_wks$Var2, all_wks$Var1, sep='_')
  
  all_wks[all_wks < paste0('2017_', week(Sys.Date())-1)]
}


scrapePdfs = function(years) {
  # Scrape historical rankings pdfs from
  # http://www.owgr.com/about?tabID={BBE32113-EBCB-4AD1-82AA-E3FE9741E2D9}
  stopifnot( typeof(years) %in% c('double','integer') )
  
  base_url = 'http://dps.endavadigital.net/owgr/doc/content/archive/'
  
  for (yr in years) {
    max_wk = if (yr < 2017) 52 else week(Sys.Date()) - 1
    
    for (wk in 1:max_wk) {
      wk_pad = str_pad(wk, 2, 'left', '0')
      url = paste0(base_url, yr, '/owgr', wk_pad, 'f',
                   if (yr==2015 & wk_pad=='21') 'f',
                   if (!yr %in% 2001:2002) yr, '.pdf')
      
      tryCatch({
        save_name = paste0('OWGR/pdf/', if (yr < 1996) 'scans_86-95/',
                           'owgr', yr, '_', wk_pad, '.pdf')
        if (!file.exists(save_name)) {
          download.file(url, save_name, mode='wb', quiet=TRUE)
        }
        
      },
      error = function(e) message(e)
      )
    }
    
  }
}



checkMissing = function(after=1996) {
  # Check which weeks are missing (from Historical Rankings
  # pdf downloads)
  have_wks = str_extract(dir('OWGR/pdf/', pattern='pdf$'), '\\d{4}_\\d{2}')
  
  missing = setdiff(allWeeks(after), have_wks)
  
  cat('Missing weeks:\n\n')
  for (yr in after:2017) {
    cat(yr, ': ', paste(grep(yr, missing, val=TRUE), collapse=', '), '\n')
  }
  
}

checkMissing()


scrapeEventResults = function(eventids=NULL) {
  # Download event results. Results include (i) position, (ii) player,
  # (iii) player id, (iv) country, (v-viii) R1-R4 scores, (ix) aggregate score,
  # (x) OWGR points earned
  stopifnot( all(grepl('^\\d+', eventids)) )
  
  base_url = 'http://www.owgr.com/en/Events/EventResult.aspx?eventId='
  table_xpath = c('//section[@id="event_result_table"]/
                  div[@class="table_container"]/table')
  
  for (eventid in as.character(eventids)) {
    url = paste0(base_url, eventid)
    save_name = paste0('OWGR/event_results/eventId_', eventid, '.txt')
    
    tryCatch({
      html = read_html(url)
      # <thead> has an extra <tr> element with all blanks; screws things up
      # so remove it and add header manually
      xml_remove(html_nodes(html, xpath=paste0(table_xpath, '/thead')))
      
      eventTable = html %>%
        html_nodes(xpath=table_xpath) %>%
        `[[`(1) %>%
        html_table(header=FALSE, fill=TRUE) %>%
        setDT()
      
      numeric_cols = c('R1','R2','R3','R4','Agg','RankingPoints')
      setnames(eventTable, c('Pos','Ctry','Name', numeric_cols))
      
      eventTable$Ctry = html %>%
        html_nodes(xpath=paste0(table_xpath, '/tr/td[@class="ctry"]/img')) %>%
        html_attr(name='title')
      
      eventTable$playerId = html %>%
        html_nodes(xpath=paste0(table_xpath, '/tr/td[@class="name"]/a')) %>%
        html_attr(name='href') %>%
        str_extract('\\d+') %>%
        as.numeric()
      
      suppressWarnings(
        eventTable[, (numeric_cols):= lapply(.SD, as.numeric),
                   .SDcols=numeric_cols]
      )
      
      # fix/standardize ties formatting
      eventTable[, Pos:= sub('T', '', Pos)]
      eventTable[eventTable[grepl('\\d', Pos), .I[.N>1], by='Pos']$V1,
                 Pos:= paste0('T', Pos)]
      
      ## save
      fwrite(eventTable, save_name, quote=T)
      
    }, error=function(e) message('event id ', eventid, ': ', e, '\n'))
    
  }
}

scrapeEventsInfo = function() {
  # Different (easier!) way to get event info. Doesn't have country but
  # can use playerId. Assumes that all events in a given year fit on one
  # page (up to 400?)
  base_url = 'http://www.owgr.com/events?pageNo=1&pageSize=ALL&tour=&year='
  table_xpath = '//table[@id="ctl1"]'
  results = list()
  
  for (yr in as.character(1986:2017)) {
    url = paste0(base_url, yr)
    html = read_html(url)
    
    eventTable = html %>%
      html_nodes(xpath=table_xpath) %>%
      `[[`(1) %>%
      html_table(header=TRUE, fill=TRUE)
    
    eventTable$eventId = html %>%
      html_nodes(xpath=paste0(table_xpath, '/tr/td[@id="ctl5"]/a')) %>%
      html_attr(name='href') %>%
      str_extract('\\d+') %>%
      as.integer()
    
    eventTable$playerId = html %>%
      html_nodes(xpath=paste0(table_xpath, '/tr/td[@id="ctl6"]/a')) %>%
      html_attr(name='href') %>%
      str_extract('\\d+') %>%
      as.integer()
    
    results[[yr]] = eventTable
  }
  
  eventsInfo = rbindlist(results)
  setnames(eventsInfo, head(names(eventsInfo), -2),
           c('Week','Year','Tour','EventName','Winner','WinnerPts',
             'WorldRating','HomeRating','FieldStrength'))
  setcolorder(eventsInfo, c(10, 1:6, 11, 7:9))
  
  eventsInfo[eventsInfo=='-'] = NA
  
  integer_cols = c('WinnerPts','WorldRating','HomeRating','FieldStrength')
  eventsInfo[, (integer_cols):= lapply(.SD, as.integer), .SDcols=integer_cols]
  
  # Nationwide Tour (and equivalent) coded as USA from 2000-2004 but I want to
  # distinguish these from PGA Tour events. Got the event ids by comparing
  # EventName to the events listed on the Wikipedia pages
  # https://en.wikipedia.org/wiki/2003_Nationwide_Tour etc.
  eventsInfo[grepl('Buy.(c|C)om', EventName), Tour:= 'BUY']
  eventsInfo[eventId %in% c(2450,2464,2466,2483,2485,2497,2503,2509,2556,2570,
                            2574,2580,2699,2798,2857,2864,2873,2876,2879,2894,
                            2896,2906,2910,2912,2917,2927,2933,2938,2940,2945,
                            2954,2958,2959,2965,2973,2979,2984,2985,2991,2997,
                            3002,3007,3011,3042,3069,3079,3082,3089,3095,3107,
                            3111,3116,3119,3124,3130,3135,3142,3147,3153,3157,
                            3162,3165,3170,3177,3186,3189,3194,3201,3206,3211,
                            3217,3222), Tour:= 'BUY']
  
  # Small fixes to improve the match rate with the Wikipedia data
  eventsInfo[, EventName:= sub('Greensbor(o?)', 'Greensboro', EventName)]
  eventsInfo[eventId==139, EventName:= 'Seiko-Tuscon Match-Pl.']
  # Olympics
  eventsInfo[eventId==6319, Tour:= 'OGC']
  
  eventsInfo[, Tour:= lapply(strsplit(Tour, ', '), unique)]
  
  # Standardize the names of major championships
  eventsInfo[Tour==list('MAJ'), EventName:=
               dplyr::case_when(
                 grepl('Masters', EventName, ignore=T) ~ 'Masters Tournament',
                 grepl('U\\.? ?S\\.? Open', EventName, ignore=T) ~ 'U.S. Open',
                 grepl('Open', EventName, ignore=T) ~ 'The Open Championship',
                 TRUE ~ 'PGA Championship'
               )]
  
  fwrite(eventsInfo[order(eventId)], 'OWGR/eventsInfo2.csv', quote=T)
}


loadEventsInfo = function() {
  # Load EventsInfo.csv into .GlobalEnv. Function is for convenience
  # since columns of type 'list' need converting via strsplit
  eventsInfo = fread('OWGR/eventsInfo2.csv')
  #eventsInfo[, eventDate:= as.Date(eventDate, '%m/%d/%Y')]
  
  eventsInfo[, Tour:=  strsplit(gsub('\"', '', Tour), '\\|')]
  
  assign('eventsInfo', eventsInfo, pos=1)
}


readEvents = function(ids=NULL, name=NULL, year=NULL, tour=NULL, ...) {
  # Example: want to load event results for all PGATOUR events in 2013-14:
  # > readEvents(year=2013:2014, tour='USA')
  # See E:/OWGR/eventTourLabel.csv for possible tour filters
  # ... - arguments to pass to fread(), e.g., select
  
  # If eventsInfo doesn't exist in .GlobalEnv, load it
  if (!exists('eventsInfo', where=1)) loadEventsInfo()
  
  # Sort by year & week since eventId isn't always chronological
  setkey(eventsInfo, Year, Week)
  
  if (!is.null(ids)) {
    # Use match() to ensure that the order of ids is preserved
    eventsInfo = eventsInfo[match(ids, eventId)]
    
  } else {
    # Note: subsetting here does not modify eventsInfo in .GlobalEnv
    if (!is.null(name)) eventsInfo = eventsInfo[grepl(name, EventName, ignore=T)]
    if (!is.null(year)) eventsInfo = eventsInfo[Year %in% year]
    if (!is.null(tour)) eventsInfo = eventsInfo[
      sapply(Tour, function(s) any(tour %in% s))]
    
    if (nrow(eventsInfo)==0) {
      stop('No events match filters. n00b!')
    }
  }
    
  files = paste0('OWGR/event_results/eventid_',
                 eventsInfo[, eventId], '.txt')
  
  result = lapply(files, fread, ...)
  
  # Add event info to attributes -- better way to do this?
  # Use str(object) to view attributes.
  Map(function(d,a1,a2,a3,a4) {
    setattr(d, 'eventName', a1)
    setattr(d, 'eventYear', a2)
    setattr(d, 'fieldStrength', a3)
    setattr(d, 'eventTour', a4)
    },
    result,
    eventsInfo[, as.list(EventName)],
    eventsInfo[, as.list(Year)],
    eventsInfo[, as.list(FieldStrength)],
    eventsInfo[, Tour])
}



### parsing the pdfs --> txt files

pdf2txt = function(file_name) {
  # Install xpdf from http://www.foolabs.com/xpdf/download.html
  # and add to system PATH
  system2('pdftotext', args=c('-layout','-nopgbrk',file_name,'-'), stdout=T)
  
}


parsePdfs = function() {
  # Parse scraped Historical Rankings pdfs (see eventScrape()), save as
  # txt files. Pdfs have different formats in different time periods!
  pdfs = dir('OWGR/pdf/', pattern='pdf$')
  pdfs_form0 = pdfs[pdfs <= 'owgr2007_46.pdf']
  pdfs_form1 = pdfs[pdfs %between% c('owgr2007_47.pdf', 'owgr2011_15.pdf')]
  pdfs_form2 = pdfs[pdfs > 'owgr2011_15.pdf']
  
  files_to_parse = setdiff(pdfs, sub('txt', 'pdf',
                                     dir('OWGR/txt/', pattern='txt$')))
  
  # before 2007 wk 46 -- only top 200 players listed
  # 2010 wk 04 only has 25 rows
  # 2016 wk 47 only has 7 columns
  # 2008 wk 34 -- edit Stephen Ames (rank 27); change country to Canada
  
  for (file in files_to_parse) {
    tryCatch({
      file_ = paste0('OWGR/pdf/', file)
      dat = data.table(V1=pdf2txt(file_))
      dat[, V1:= str_trim(V1)]
      dat = dat[grepl('^\\d+=?\\s+\\(', V1)]
      
      # Add "~" delimiters, split
      dat[, V1:= V1 %>% str_replace('\\([^A-Z]*', ' ') %>%
            # Kim Do-Hoon#752(Daegu) Korea
            str_replace('#[^ ]*', ' ') %>%
            # (rank)= used to indicate ties (before 2007?)
            str_replace_all('=|\\+', '') %>%
            str_replace_all(' {2,}', '~') %>%
            str_replace_all('(\\d) (\\d)', '\\1~\\2') %>%
            str_replace_all('(\\.) (\\d)', '\\1\\2')]
      
      dat = dat[, tstrsplit(V1, '~', type.convert=T)]
      
      if (file != 'owgr2016_47.pdf') {
        setnames(dat, c('ThisWeek','Name','Country','AveragePoints',
                        'TotalPoints','EventsPlayed','PointsLostYr',
                        'PointsGainedYr',
                        if (file %in% pdfs_form2) 'EventsPlayedActual'))
      } else {
        setnames(dat, c('ThisWeek','Name','Country','AveragePoints',
                        'TotalPoints','EventsPlayed'))
      }
      
      # remove parenthetical notes
      dat[, Name:= str_trim(gsub('\\(.*\\)', '', Name))]
      # remove extra spaces in country abbreviations (e.g., 'A us')
      dat[nchar(Country) < 5, Country:= gsub(' ', '', Country)]
      
      fwrite(dat, gsub('pdf', 'txt', file_), quote=T)
    },
    error=function(e) {cat(file, '\n'); message(e)}
    )
    
  }
}



combineWeeks = function(after=2007, var='ThisWeek', suffix='RANK') {
  # To do: Modify this so that it can *update* files
  files = dir('OWGR/txt/', pattern='txt$', full.names=T)
  files = files[files > paste0('OWGR/txt/owgr', after)]
  
  dat = setNames(lapply(files, fread, select=c(var, 'Name'), na=''),
                 files)
  
  invisible( lapply(files, function(h) {
    setnames(dat[[h]], var, str_extract(h, '\\d{4}_\\d{2}')) })
  )
  
  cat('Merging weekly data...')
  dat = Reduce(function(x,y) merge(x, y, all=TRUE), dat) # merge on Name
  cat('done!\n')
  
  # Add NA columns for missing weeks
  all_wks  = allWeeks(after)
  all_wks  = all_wks[all_wks >= names(dat)[2]]
  missing_wks = setdiff(all_wks, names(dat))
  
  if (length(missing_wks) > 0) {
    if (var=='ThisWeek') dat[, (missing_wks):= NA_integer_]
    if (var=='Country')  dat[, (missing_wks):= NA_character_]
    if (grepl('Points', var)) dat[, (missing_wks):= NA_real_]
  }
  
  setcolorder(dat, c('Name', sort(setdiff(names(dat), 'Name'))))
  
  # Correct name variants
  nameEdits = fread('OWGR/NameCorrections_edit.csv', na='')[!is.na(Edit)]
  dat[nameEdits, on='Name', Name:= Edit]
  
  # Now combine rows with same player name
  dupNames = dat[duplicated(Name), unique(Name)]
  for (name in dupNames) {
    name.i = dat[, which(Name==name)]
    statVec  = apply(dat[name.i, -1], 2, function(y) c(y[!is.na(y)], NA)[1])
    dat[name.i[1], (names(dat)[-1]):= as.list(statVec)]
    dat = dat[-name.i[-1]]
  }
  
  fwrite(dat, paste0('OWGR/all_weeks/owgr_', suffix, '.csv'), quote=T)
}



makeMaster = function() {
  # Make master file (long format, attributes as columns)
  files = dir('OWGR/all_weeks/', pattern='owgr_', full.names=T)
  
  cat('Reading files in OWGR/all_weeks/...')
  master = setNames(lapply(files, fread, na=''),
                    str_extract(files, '[A-Z]+(?=\\.csv)'))
  cat('done!\n')
  
  # Throws a warning; missing weeks with all NA entries are type 'logical'
  suppressWarnings({
    master = lapply(master, melt.data.table, id='Name',
                    variable.name='week', variable.factor=F, na.rm=T)
  })
  
  for (z in names(master)) {
    setnames(master[[z]], 'value', z)
  }
  
  master = Reduce(merge, master)
  
  # Assign consistent country names (see _assignPlayerCountries.R)
  countryMapping = fread('OWGR/PlayerCountries_edit.csv', na='')
  master[countryMapping, on='Name', COUNTRY:= Country]
  
  setkey(master, week, RANK)
  
  save(master, file='OWGR/owgr_MASTER.RData')
  
}


updateOWGR = function() {
  scrapePdfs(2017)
  parsePdfs()
  scrapeEventResults(eventids = 6682:6690)
  combineWeeks(after=1996, var='ThisWeek', suffix='RANK')
  combineWeeks(after=1996, var='AveragePoints', suffix='AVGPTS')
  combineWeeks(after=1996, var='TotalPoints', suffix='TOTPTS')
  # # Also run _assignPlayerCountries.R and update spreadsheet after running
  # # this one
  combineWeeks(after=1996, var='Country', suffix='COUNTRY')
  #makeMaster()
}

