context('largeData')

test_that('large1dim', {
  require(plyr)
  mirtCluster(3)
  
  data <- dataComplete[, grep('^i[0-9]', names(dataComplete))]

  # no error
  itemTypes = ifelse(
    sapply(data, max) > 1,
    'gpcm', 
    '2PL'
  )
  model = mirt(data, 1, itemtype=itemTypes, se=T)
    
  # error
  itemTypes = ifelse(
    sapply(data, max) > 1,
    'grsm', 
    '2PL'
  )
  model = mirt(data, 1, itemtype=itemTypes, se=T)
  
  mplusParams = paramComplete # are part of data/dataComplete.RData
  mirtParams = mod2values(model)[, c('item', 'name', 'value')]
  mirtParams$item = as.character(mirtParams$item)
  mirtParams$name = as.character(mirtParams$name)
  mirtParams$name = sub('^d$', 'd1', mirtParams$name)
  params = join(mplusParams, mirtParams, by=c('item', 'name'), type='left', match='first')
  filter = params$name == 'a1'
  comp = cor(params[filter, 3], params[filter, 5])
  expect_equal(comp, 1, tolerance = 0.2)
  
  mirtScores = fscores(model, full.scores=T, method='EAP')
})

