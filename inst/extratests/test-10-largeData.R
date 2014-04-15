context('largeData')

test_that('large1dim', {
  require(plyr)
  mirtCluster(3)
  
  data <- dataComplete[, grep('^i[0-9]', names(dataComplete))]
  itemTypes = ifelse(
    sapply(data, max) > 1,
    'graded', 
    '2PL'
  )
  model = mirt(data, 1, itemtype=itemTypes, se=T)
  
  mplusParams = paramComplete # are part of data/dataComplete.RData
  mirtParams = mod2values(model)[, c('item', 'name', 'value')]
  mirtParams$item = as.character(mirtParams$item)
  mirtParams$name = as.character(mirtParams$name)
  mirtParams$name = sub('^d$', 'd1', mirtParams$name)
  params = join(mplusParams, mirtParams, by=c('item', 'name'), type='left', match='first')
  for(n in c('^a1$', '^d')){
    filter = grepl(n, params$name)
    comp = abs(cor(params[filter, 3], params[filter, 5]))
    plot(params[filter, 3], params[filter, 5], main=paste0(n, ': cor(', round(comp, 3), ')'))
    expect_equal(comp, 1, tolerance = 0.05)
  }

  mirtScores = fscores(model, full.scores=T, method='EAP')
  comp = cor(mirtScores[, 1], dataComplete$theta)
  expect_equal(comp, 1, tolerance = 0.01)  
  plot(mirtScores[, 1], dataComplete$theta)
})

