#Dataset with the correct labels.
rseed <- 1234
ds_sim <- gen_SimData('4b',rseed,n_ts=100,n_sites=50,true_cpts=c(50))
#Set a GAM formula.
GAM_formula <- Y ~ s(U, V, bs = "tp", k = 5) + s(T, bs = "cr", k = 5) +
  ti(U,V, T, d = c(2, 1), bs = c("tp", "cr"), k = c(5, 5))

#Run GAM PELT - Use a fairly long but valid minimum segment length for speed.
X <- GAM.PELT(ds_sim$data, formula = GAM_formula,verbose=FALSE, minseglen = 45)
test_that('Test GAM.PELT class',{
  expect_s4_class(X,'GAM_PELT')
  expect_equal(class(X@data.set),'data.frame')
  expect_equal(class(X@pen.type),'character')
  expect_equal(class(X@pen.value),'numeric')
  expect_equal(class(X@minseglen),'numeric')
  expect_equal(class(X@cpts),'numeric')
  expect_equal(class(X@GAM.form),'formula')
  expect_equal(class(X@param.est),'list')
  expect_equal(class(X@ncpts),'integer')
  expect_equal(class(data.set(X)),'data.frame')
  expect_equal(class(pen.type(X)),'character')
  expect_equal(class(pen.value(X)),'numeric')
  expect_equal(class(minseglen(X)),'numeric')
  expect_equal(class(cpts(X)),'numeric')
  expect_equal(class(GAM.form(X)),'formula')
  expect_equal(class(param.est(X)),'list')
  expect_equal(class(ncpts(X)),'integer')
  expect_equal(class(summary(X)),'NULL')
  expect_equal(class(show(X)),'NULL')
})

