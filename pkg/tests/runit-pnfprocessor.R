test.signalprocessor.inputcheck <- function() {
  # check empty input
  # TODO check howto
  data <- data.frame()
  checkException(rpnf:::.xo.signalprocessor(data))
  #checkException()
}

#
# bullish signals
test.signalprocessor.DOUBLE_TOP <- function() {
  data <- read.csv("runit-testcase-signalprocessor-doubletop.csv",colClasses=c("Date","integer","character","integer","character"))
  checkEquals(data$result.signal.bs,rpnf:::.xo.signalprocessor(data)$signal.bs)
}
test.signalprocessor.BULLISH_SIGNAL <- function() {
  data <- read.csv("runit-testcase-signalprocessor-bullishsignal.csv",colClasses=c("Date","integer","character","integer","character"))
  checkEquals(data$result.signal.bs,rpnf:::.xo.signalprocessor(data)$signal.bs)
}
test.signalprocessor.TRIPLE_TOP <- function() {
  data <- read.csv("runit-testcase-signalprocessor-tripletop.csv",colClasses=c("Date","integer","character","integer","character"))
  checkEquals(data$result.signal.bs,rpnf:::.xo.signalprocessor(data)$signal.bs)
}
test.signalprocessor.TRIPLE_BULLISH_SIGNAL <- function() {
  data <- read.csv("runit-testcase-signalprocessor-triplebullishsignal.csv",colClasses=c("Date","integer","character","integer","character"))
  checkEquals(data$result.signal.bs,rpnf:::.xo.signalprocessor(data)$signal.bs)
}
test.signalprocessor.BULLISH_CATAPULT <- function() {
  data <- read.csv("runit-testcase-signalprocessor-bullishcatapult.csv",colClasses=c("Date","integer","character","integer","character"))
  checkEquals(data$result.signal.bs,rpnf:::.xo.signalprocessor(data)$signal.bs)
}
test.signalprocessor.BULLISH_TRIANGLE <- function() {
  data <- read.csv("runit-testcase-signalprocessor-bullishtriangle.csv",colClasses=c("Date","integer","character","integer","character"))
  checkEquals(data$result.signal.bs,rpnf:::.xo.signalprocessor(data)$signal.bs)
}
# TODO low pole is a tricky one!!!
# test.signalprocessor.LOW_POLE <- function() {
#   data <- read.csv("runit-testcase-signalprocessor-lowpole.csv",colClasses=c("Date","integer","character","integer","character"))
#   checkEquals(data$result.signal.bs,rpnf:::.xo.signalprocessor(data)$signal.bs)
# }
# test.signalprocessor.BEAR_TRAP <- function() {
#   data <- read.csv("runit-testcase-signalprocessor-beartrap.csv",colClasses=c("Date","integer","character","integer","character"))
#   checkEquals(data$result.signal.bs,rpnf:::.xo.signalprocessor(data)$signal.bs)
# }

#
# bearish signals
# test.signalprocessor.DOUBLE_BOTTOM <- function() {
#   data <- read.csv("runit-testcase-signalprocessor-doublebottom.csv",colClasses=c("Date","integer","character","integer","character"))
#   checkEquals(data$result.signal.bs,rpnf:::.xo.signalprocessor(data)$signal.bs)
# }
# test.signalprocessor.BEARISH_SIGNAL <- function() {
#   data <- read.csv("runit-testcase-signalprocessor-triplebearishsignal.csv",colClasses=c("Date","integer","character","integer","character"))
#   checkEquals(data$result.signal.bs,rpnf:::.xo.signalprocessor(data)$signal.bs)
# }
# test.signalprocessor.TRIPLE_BOTTOM <- function() {
#   data <- read.csv("runit-testcase-signalprocessor-triplebottom.csv",colClasses=c("Date","integer","character","integer","character"))
#   checkEquals(data$result.signal.bs,rpnf:::.xo.signalprocessor(data)$signal.bs)
# }
# test.signalprocessor.TRIPLE_BEARISH_SIGNAL <- function() {
#   data <- read.csv("runit-testcase-signalprocessor-triplebearishsignal.csv",colClasses=c("Date","integer","character","integer","character"))
#   checkEquals(data$result.signal.bs,rpnf:::.xo.signalprocessor(data)$signal.bs)
# }
# test.signalprocessor.BEARISH_CATAPULT <- function() {
#   data <- read.csv("runit-testcase-signalprocessor-bearishcatapult.csv",colClasses=c("Date","integer","character","integer","character"))
#   checkEquals(data$result.signal.bs,rpnf:::.xo.signalprocessor(data)$signal.bs)
# }
# test.signalprocessor.BEARISH_TRIANGLE <- function() {
#   data <- read.csv("runit-testcase-signalprocessor-bearishtriangle.csv",colClasses=c("Date","integer","character","integer","character"))
#   checkEquals(data$result.signal.bs,rpnf:::.xo.signalprocessor(data)$signal.bs)
# }
# test.signalprocessor.BULLISH_SIGNAL_REVERSED <- function() {
#   data <- read.csv("runit-testcase-signalprocessor-bullishsignalreversed.csv",colClasses=c("Date","integer","character","integer","character"))
#   checkEquals(data$result.signal.bs,rpnf:::.xo.signalprocessor(data)$signal.bs)
# }
# test.signalprocessor.HIGH_POLE <- function() {
#   data <- read.csv("runit-testcase-signalprocessor-highpole.csv",colClasses=c("Date","integer","character","integer","character"))
#   checkEquals(data$result.signal.bs,rpnf:::.xo.signalprocessor(data)$signal.bs)
# }
# test.signalprocessor.BULL_TRAP <- function() {
#   data <- read.csv("runit-testcase-signalprocessor-bulltrap.csv",colClasses=c("Date","integer","character","integer","character"))
#   checkEquals(data$result.signal.bs,rpnf:::.xo.signalprocessor(data)$signal.bs)
# }

#
# advanced signal patterns
# test.signalprocessor.BEARISH_SIGNAL_REVERSED <- function() {
#   data <- read.csv("runit-testcase-signalprocessor-bearishsignalreversed.csv",colClasses=c("Date","integer","character","integer","character"))
#   checkEquals(data$result.signal.bs,rpnf:::.xo.signalprocessor(data)$signal.bs)
# }
