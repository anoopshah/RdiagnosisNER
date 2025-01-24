require(testthat)
require(Rdiagnosislist)
require(RdiagnosisNER)
require(bit64)
require(data.table)
require(spacyr)

context('Testing evaluation functions')

test_that('Testing evaluation functions', {
	actual = data.table(id = c(2, 2, 3, 3, 3),
		conceptId = as.SNOMEDconcept(c(
		'Diastolic heart failure', 
		'Systolic heart failure', 
		'Systolic dysfunction', 
		'Acute heart failure',
		'Cardiac finding'), SNOMED = sampleSNOMED()))
	# first two are correct, third concept is vaguer than gold standard
	# (i.e. true in 'relaxed' mode but not if relaxed = FALSE),
	# and fourth concept is a false positive and fifth is an ancestor
	# which should be ignored

	goldstandard = data.table(id = c(1, 2, 2, 2, 3, 3),
		conceptId = c('703276005', '78862003', '417996009|703273002',
		'443253003', '56675007', '84114007'))
	# first and second are not present in actual (i.e. false negatives),
	# third has two correct options and sixth is an ancestor which
	# should be ignored

	E <- evaluateNER(actual, goldstandard, SNOMED = sampleSNOMED())
	expect_equal(E$precision, 0.25)
	expect_equal(E$recall, 0.2)
	
	E <- evaluateNER(actual, goldstandard, SNOMED = sampleSNOMED(),
		subset = actual$conceptId[1:4])
	expect_equal(E$precision, 0.5)
	expect_equal(E$recall, 1)
	
	E <- evaluateNER(actual, goldstandard, relaxed = TRUE,
		SNOMED = sampleSNOMED())
	expect_equal(E$precision, 0.5)
	expect_equal(E$recall, 0.4)
	
	E <- evaluateNER(actual, goldstandard, aggregate = FALSE,
		SNOMED = sampleSNOMED())
	expect_equal(E$truepos, c(0, 0, 1))
	expect_equal(E$falseneg, c(1, 3, 0))
	expect_equal(E$falsepos, c(0, 2, 1))
})
