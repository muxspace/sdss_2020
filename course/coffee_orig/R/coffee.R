library(htmltab)

read_coffee <- function() {
  uri <- 'https://cspinet.org/eating-healthy/ingredients-of-concern/caffeine-chart'
  df <- htmltab(doc=uri, which=1)[1:28,]
  colnames(df) <- c('coffee','size','caffeine')

  df
}


normalize_coffee <- function(df) {
  df$caffeine <- parse_caffeine(df$caffeine)
  df$volume <- parse_volume(df$size)
  df
}



parse_volume <- function(x) {
  as.numeric(sub('^.*?([0-9]+) oz.*', '\\1', x, perl=TRUE))
}

parse_caffeine <- function(x) {
  scalar <- as.numeric(x)
  range <- sapply(strsplit(x,'-', fixed=TRUE), 
    function(p) mean(as.numeric(p)))
  ifelse(is.na(scalar), range, scalar)
}


train_coffee <- function(df) {
  lm(caffeine ~ volume, df)
}




parse_caffeine <- function(x) {
  sapply(strsplit(x,'-', fixed=TRUE), function(p) mean(as.numeric(p)))
}


train_coffee <- function(df, ...) {
  lm(caffeine ~ volume + roast + brand, df, ...)
}


read_coffee <- function(path='private/coffee.html') {
  df <- htmltab(doc=path, which=1)[1:28,]
  colnames(df) <- c('coffee','size','caffeine')

  df
}

