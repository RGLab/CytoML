chnls <- c("FSC-H","SSC-H")
data.range <- data.frame(min = c(-1, -1), max = c(4e3, 4e3), row.names = chnls)
sqrcut <- matrix(c(300, 50
                   ,300, 300
                   , 600, 300
                   , 600, 50)
                 , byrow = TRUE, nrow=4
                 , dimnames = list(NULL, chnls))
pg <- polygonGate(filterId="nonDebris", sqrcut)

test_that("extend.polygonGate: y axis",{

  bound <- matrix(c(100,3e3
                    ,100,3e3)
                  , byrow = TRUE, nrow = 2
                  , dimnames = list(chnls, c("min", "max")))
  pg.extened <- extend(pg, bound)
  # paste(as.vector(pg.extened@boundaries), collapse = ",")
  expectRes <- matrix(c(600,-2147483647
                        ,300,-2147483647
                        ,300,100
                        ,300,300
                        ,600,300
                        ,600,100
                       )
                     ,byrow = TRUE,nrow=6)
  expect_equivalent(pg.extened@boundaries, expectRes)

})

test_that("extend.polygonGate: both x and y axis",{
  bound <- matrix(c(400,3e3
                    ,100,3e3)
                  , byrow = TRUE, nrow = 2
                  , dimnames = list(chnls, c("min", "max")))
  pg.extened <- extend(pg, bound)


  expectRes <- matrix(c(600,-2147483647,-2147483647,-2147483647,400,600,600,-2147483647,-2147483647,100,300,300,300,100), nrow = 7)

  expect_equivalent(pg.extened@boundaries, expectRes)

})

test_that("extend.polygonGate: concave on left with data.range",{
  sqrcut <- matrix(c(300, 50
                     , 450, 150
                     ,300, 300
                     , 600, 300
                     , 600, 50)
                   , byrow = TRUE, nrow=5
                   , dimnames = list(NULL, chnls))
  pg <- polygonGate(filterId="nonDebris", sqrcut)
  bound <- matrix(c(400,3e3
                    ,10,3e3)
                  , byrow = TRUE, nrow = 2
                  , dimnames = list(chnls, c("min", "max")))
  pg.extened <- extend(pg, bound, data.range = data.range)

  expectRes <- matrix(c(-1,-1,400,450,400,-1,-1,400,600,600,400,50,116.666666666667,116.666666666667,150,200,200,300,300,300,50,50)
                      , nrow = 11)

  expect_equivalent(pg.extened@boundaries, expectRes[-(1:2),])

})

test_that("extend.polygonGate: concave on left& bottom with data.range",{
  sqrcut <- matrix(c(300, 50
                     , 450, 150
                     ,300, 300
                     , 600, 300
                     , 600, 50
                     , 550,100
                     , 500,50
                     , 450,60)
                   , byrow = TRUE, nrow=8
                   , dimnames = list(NULL, chnls))
  pg <- polygonGate(filterId="nonDebris", sqrcut)
  bound <- matrix(c(400,3e3
                    ,100,3e3)
                  , byrow = TRUE, nrow = 2
                  , dimnames = list(chnls, c("min", "max")))
  pg.extened <- extend(pg, bound, data.range = data.range)

  expectRes <- matrix(c(-1,-1,-1,400,450,400,-1,-1,400,600,600,600,550,-1,100,116.666666666667,116.666666666667,150,200,200,300,300,300,100,-1,-1)
                      , nrow = 13)

  expect_equivalent(pg.extened@boundaries, rbind(expectRes[-(1:3),],c(400, 100)))

})

test_that("extend.polygonGate: with off-bound vertex that is outside of bounding intersected points",{
  sqrcut <- matrix(c(-1.17531505192556,0.0277040198781766,-0.0795288778870589,-1.10419733542672,3.13855379324755,3.5332650668046,5.79388417899497,5.79089357705492)
                   , nrow = 4
                   , dimnames = list(NULL, chnls))
  pg <- polygonGate(filterId="nonDebris", sqrcut)
  data.range <- data.frame(min = c(-2, -1), max = c(1, 7), row.names = chnls)
  bound <- matrix(c(-1.098612,8.159161
                    ,-1.098612,8.159161)
                  , byrow = TRUE, nrow = 2
                  , dimnames = list(chnls, c("min", "max")))
  pg.extened <- extend(pg, bound, data.range = data.range)

  expectRes <- matrix(c(-2,-2,-1.098612,0.0277040198781766,-0.0795288778870589,-1.098612,5.79090987843986,3.16372011024189,3.16372011024189,3.5332650668046,5.79388417899497,5.79090987843986)
                      , nrow = 6)

  expect_equivalent(pg.extened@boundaries, expectRes)

})
