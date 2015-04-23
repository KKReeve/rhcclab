library(data.table)

f.proposedFilename = function(prefix, settings) 
  paste(c(prefix, names(settings)[unlist(settings)]), collapse="_")

f.year = function(date) as.integer(gsub("(.+)-.+", "\\1", date))
f.month = function(date) as.integer(gsub(".+-(.+)", "\\1", date))
f.nextdate = function(sd) 
{
  sd.year = f.year(sd)
  sd.month = f.month(sd)
  dd.month = (sd.month %% 12) + 1
  dd.year = (sd.month %/% 12) + sd.year
  sprintf("%d-%02d", dd.year, dd.month)
}
f.quarter = function(date) paste( f.year(date), (f.month(date)-1) %/% 3 + 1, sep='-Q' )
f.monthsSince = function(date, start) (f.year(date)-f.year(start)) * 12 + (f.month(date)-f.month(start))

#Example: f.summary(c(1,2,3,6))
f.summary = function(v)
{
  f = fivenum(v)
  c(min=f[[1]],
    q1=f[[2]],
    median=median(v),
    mean=mean(v),
    sd=sd(v),
    q3=f[[4]],
    max=f[[5]])
}

# Example: f.mode(c('A','B','B','C','C','D'))
f.mode = function(x)
{
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Example: f.weightedMode(c('A','B','B','C','C','D'), c(10,20,30,40,50,60))
f.weightedMode = function(x,w = 1, ignore = NULL)
{
  x = x[!(x %in% ignore)]
  tx <- table(x)
  modeX = names(tx[tx == max(tx)])
  modeXWeighted = data.table(x,w)[x %in% modeX, sum(w), by="x"][order(-V1)]
  modeXWeighted[1,]$x
}

f.weightedMode2 = function(x,w = 1, ignore = NULL, w2 = 1)
{
  tx = data.table(x,w2)[!(x %in% ignore), max(w2) * length(w2), by='x']
  modeX = tx[V1 == max(tx$V1)]$x
  modeXWeighted = data.table(x,w)[x %in% modeX, sum(w), by="x"][order(-V1)]
  modeXWeighted[1,]$x
}

f.gaussianMode = function(x, w = c(0.3, 0.4, 0.3))
{
  tw = data.table(x,w)
  tw[,sum(w),by="x"][order(-V1)][1,]$x
}

library(RcppRoll)
f.rollingGaussMode <- rollit_raw("return X(0) == X(2) ? X(0) : X(1);")

f.str_elide = function(s, length = 20, elideText = "...") {
  el = str_length(elideText)
  l = (length %/% 2) - (el %/% 2)
  s1 = str_sub(s, 1, l)
  s2 = str_sub(s, str_length(s)-(length-el-l)+1, str_length(s))
  s12 = paste0(s1, elideText, s2)
  ifelse(str_length(s) > length, s12, s)
}
