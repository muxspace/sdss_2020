# source("coffee.R") # Needed for normalize_coffee
library(xml2)
library(httr)
library(futile.logger)
library(lambda.tools)

flog.threshold(DEBUG)

read_cspi <- function(path='private/coffee.html') {
  df <- htmltab(doc=path, which=1)[1:28,]
  colnames(df) <- c('coffee','size','caffeine')

  df
}

# Exercise. Add futile.logger modulo to improve transparency of progress
# This works fine for a few records, but the complete download has a parsing
# error. Discuss this in troubleshooting and debugging.
read_oc <- function(limit=NULL) {
  base <- 'http://www.overcaffeinated.org'
  toc.uri <- sprintf('%s/database/coffee/',base)
  rsp <- content(GET(toc.uri))
  xpath <- '//div[@class="field-content views-align-center"]/a'
  paths <- unique(xml_attr(xml_find_all(rsp, xpath), 'href'))
  if (!is.null(limit)) paths <- paths[1:limit]

  flog.info("Downloading %s drinks", length(paths))

  do.call(rbind, lapply(paths, function(p) {
    uri <- sprintf('%s/%s', base, p)
    flog.debug("Get %s", uri)
    rsp <- content(GET(uri))

    name <- xml_text(xml_find_all(rsp, '//h1[@class="page-title"]'))
    divs <- xml_text(xml_find_all(rsp, '//div[@class="field-item even"]'))
    brand <- divs[4]
    blurb <- divs[2]
    size <- xml_text(xml_find_all(rsp,
      '//td[@class="views-field views-field-field-size"]'))
    caffeine <- xml_text(xml_find_all(rsp,
      '//td[@class="views-field views-field-field-caffeine"]'))

    data.frame(name=name, brand=brand, blurb=blurb, 
      size=size, caffeine=caffeine, stringsAsFactors=FALSE)
  }))
}

# Unit test only checks download. A functional test is more effective since
# it inspects output and values after the normalization.
read_coffee <- function(limit=10) {
  df.cspi <- read_cspi()
  df.oc <- read_oc(limit=limit)

  # TODO: Discuss checking units
  colnames(df.cspi)[1] <- 'name'
  rbind(df.cspi, df.oc[,colnames(df.cspi)])
}


#' Verify read and normalize coffee work as expected
#
# TODO: In troubleshooting chapter, discuss removing warning messages
test_read_coffee <- function() {
  df <- normalize(read_coffee())
}

# Yields
# trim(df$size)
# [1] "12oz Cup"      "16oz Cup"      "20oz Cup"      "8oz Cup"      
# [5] "13.7oz Bottle" "9.5oz Bottle"  "13.7oz Bottle" "13.7oz Bottle"
# [9] "9.5oz Bottle"
trim <- function(x) {
  regex <- c('^\\s+', '\\s+$')
  fold(regex, function(r,a) gsub(r,'',a,perl=TRUE), x)
}

parse_caffeine <- function(x) {
  x <- trim(x)
  scalar <- as.numeric(x)
  range <- sapply(strsplit(x,'-', fixed=TRUE), 
    function(p) mean(as.numeric(p)))
  ifelse(is.na(scalar), range, scalar)
}

# This hints at another version of trim that can take additional replacements
# Yields
#
trim <- function(x, pre=NULL) {
  regex <- c(pre, '^\\s+', '\\s+$')
  fold(regex, function(r,a) gsub(r,'',a,perl=TRUE), x)
}

parse_caffeine <- function(x) {
  x <- trim(x)
  scalar <- as.numeric(x)
  range <- sapply(strsplit(x,'-', fixed=TRUE), 
    function(p) mean(as.numeric(p)))
  ifelse(is.na(scalar), range, scalar)
}

# TODO: Add unit test for this (incorrect parse)
# > sub('^.*?([0-9]+)\\s?oz.*', '\\1', x, perl=TRUE)
# [1] "12" "16" "20" "8"  "7"  "5"  "7"  "7"  "5"
parse_volume <- function(x) {
  x <- trim(x, c('Cup','Bottle'))
  as.numeric(sub('^.*?([0-9]+)\\s?oz.*', '\\1', x, perl=TRUE))
}

parse_volume <- function(x) {
  x <- trim(x, c('Cup','Bottle'))
  as.numeric(sub('^.*?([0-9.]+)\\s?oz.*', '\\1', x, perl=TRUE))
}


