library(RUnit)
testsuite.pnf <- defineTestSuite("rpnf",
                                 dirs = ".",
                                 testFileRegexp = "^runit.+\\.R",
                                 testFuncRegexp = "^test.+",
                                 rngKind = "Marsaglia-Multicarry",
                                 rngNormalKind = "Kinderman-Ramage")
print(paste("[INFO] Working directory: ",getwd()))
testResult <- runTestSuite(testsuite.pnf)
printTextProtocol(testResult)
errors <- getErrors(testResult)
# check for deactivated tests
if (errors$nDeactivated >0) {
  warning(paste("Identified ",errors$nDeactivated," deactivated tests!"))
}
# check for errors and failiures
if (errors$nErr + errors$nFail >0) {
  stop(paste("Identified ",errors$nErr, " errors and ",errors$nFail," test failures in test suite!"))
}