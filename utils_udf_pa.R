library(pwr)

get2ptest <- function(base_p, expected_p, n, confidence, power) {
  if (is.null(expected_p)) {
    h = NULL
    testresults = pwr.2p.test(h=h, n=n, sig.level = (1-confidence), power=power, alternative = c("two.sided"))
    expects = (sin((testresults$h + 2*asin(sqrt(base_p)))/2))^2
    return(expects)
  }
  else {
    h = ES.h(expected_p, base_p)
    if (is.null(confidence)) {siglvl = NULL} else {siglvl=1-confidence}
    testresults = pwr.2p.test(h=h, n=n, power = power, sig.level = siglvl, alternative = c("two.sided"))
    if (is.null(n)) {return(testresults$n)}
    else if (is.null(power)) {return(testresults$power)}
    else if (is.null(confidence)) {return(testresults$sig.level)}
  }
}

get2p2ntest <- function(base_p, expected_p, n1, n2, confidence, power) {
  if (is.null(expected_p)) {
    h = NULL
    testresults = pwr.2p2n.test(h=h, n1=n1, n2=n2, sig.level = (1-confidence), power=power, alternative = c("two.sided"))
    expects = (sin((testresults$h + 2*asin(sqrt(base_p)))/2))^2
    return(expects)
  }
  else {
    h = ES.h(expected_p, base_p)
    if (is.null(confidence)) {siglvl = NULL} else {siglvl=1-confidence}
    testresults = pwr.2p2n.test(h=h, n1=n1, n2=n2, power = power, sig.level = siglvl, alternative = c("two.sided"))
    if (is.null(n2)) {return(testresults$n2)}
    else if (is.null(n1)) {return(testresults$n1)}
    else if (is.null(power)) {return(testresults$power)}
    else if (is.null(confidence)) {return(testresults$sig.level)}
  }
}

get2p2ntest_v2 <- function(base_p, expected_p, n1, n2, confidence, power) {
  if (is.null(expected_p)) {
    h = NULL
    testresults = sam.2p2n.test(h=h, n1=n1, n2=n2, sig.level = (1-confidence), power=power, alternative = c("two.sided"))
    expects = (sin((testresults$h + 2*asin(sqrt(base_p)))/2))^2
    return(expects)
  }
  else {
    h = ES.h(expected_p, base_p)
    if (is.null(confidence)) {siglvl = NULL} else {siglvl=1-confidence}
    testresults = pwr.2p2n.test(h=h, n1=n1, n2=n2, power = power, sig.level = siglvl, alternative = c("two.sided"))
    if (is.null(n2)) {return(testresults$n2)}
    else if (is.null(n1)) {return(testresults$n1)}
    else if (is.null(power)) {return(testresults$power)}
    else if (is.null(confidence)) {return(testresults$sig.level)}
  }
}

get2p2ntest_v2(base_p = 0.02, expected_p =NULL, n1=50000000000000, n2=50000000000000000, confidence = 0.95, power = 0.8)

get2p2ntest(base_p = 0.5, expected_p =0.51, n1=40000, n2=NULL, confidence = 0.59, power = 0.42)
# x = 1:10
# x = x*0.01
# y=sapply(x, function(z) get2p2ntest(base_p = z, expected_p =NULL, n1=42000, n2=50000, confidence = 0.95, power = 0.99))


# get2ptest(base_p=0.02, expected_p=0.1, n=109.977, confidence=NULL, power=0.8)
# 
# (h=ES.h(0.5, 0.02))
# n=109
# results = pwr.2p.test(h=h, n=109, sig.level = NULL, power=0.8, alternative = c("two.sided"))
# 



