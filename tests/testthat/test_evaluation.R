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
	expect_equal(E$precision, )
	expect_equal(E$recall, )
	
	E <- evaluateNER(actual, goldstandard, aggregate = FALSE,
		SNOMED = sampleSNOMED())
	expect_equal(E$truepos, c(0, 0, 1))
	expect_equal(E$falseneg, c(1, 3, 0))
	expect_equal(E$falsepos, c(0, 2, 1))
})

test_that('Testing NER sentence with concept composition', {
	# Create CDB for NER
	data.table::setDTthreads(threads = 1)
	miniSNOMED <- Rdiagnosislist::sampleSNOMED()
	miniCDB <- createCDB(SNOMED = miniSNOMED,
		MANUAL_SYNONYMS = NULL)
	DEC <- decompose(SNOMEDconcept(c('83291003', '19829001'),
		SNOMED = miniSNOMED), CDB = miniCDB, SNOMED = miniSNOMED)
	miniCDB <- addComposeLookupToCDB(DEC, CDB = miniCDB)
	miniCDB <- addLemmaToCDB(miniCDB)

	# SNOMED concepts for testing
	sct_corpulmonale <- SNOMEDconcept('Cor pulmonale',
		SNOMED = miniSNOMED)
	sct_lungdisease <- SNOMEDconcept('Disorder of lung',
		SNOMED = miniSNOMED)

	# Test NER sentence with SNOMED concept composition
	OUT <- NERsentence('Right heart failure caused by lung disease',
		CDB = miniCDB, SNOMED = miniSNOMED)
	expect_setequal(attr(OUT, 'findings')$conceptId,
		c(sct_lungdisease, sct_corpulmonale))
})

test_that('Testing NER document and corpus', {
	# Create CDB for NER
	data.table::setDTthreads(threads = 1)
	miniSNOMED <- Rdiagnosislist::sampleSNOMED()
	miniCDB <- createCDB(SNOMED = miniSNOMED,
		MANUAL_SYNONYMS = NULL)
	DEC <- decompose(SNOMEDconcept(c('83291003', '19829001'),
		SNOMED = miniSNOMED), CDB = miniCDB, SNOMED = miniSNOMED)
	miniCDB <- addComposeLookupToCDB(DEC, CDB = miniCDB)
	miniCDB <- addLemmaToCDB(miniCDB)

	# SNOMED concepts for testing
	sct_corpulmonale <- SNOMEDconcept('Cor pulmonale',
		SNOMED = miniSNOMED)
	sct_lungdisease <- SNOMEDconcept('Disorder of lung',
		SNOMED = miniSNOMED)
	sct_cardiomyopathy <- SNOMEDconcept('Cardiomyopathy',
		SNOMED = miniSNOMED)
	zeroconcept <- sct_cardiomyopathy[0]

	# Test NER document
	OUT <- NERdocument('Cardiomyopathy. 
		Right heart failure caused by lung disease',
		CDB = miniCDB, SNOMED = miniSNOMED)
	expect_equal(attr(OUT, 'findings')[sentence == 1]$conceptId,
		sct_cardiomyopathy)
	expect_equal(sort(attr(OUT, 'findings')[sentence == 2]$conceptId),
		c(sct_lungdisease, sct_corpulmonale))

	# Test NER corpus
	OUT <- NERcorpus(c('No concepts here', 'Cardiomyopathy. 
		Right heart failure caused by lung disease'), 1:2,
		CDB = miniCDB, SNOMED = miniSNOMED)
	expect_equal(OUT[id == 1]$conceptId, zeroconcept)
	expect_setequal(OUT[id == 2]$conceptId,
		c(sct_lungdisease, sct_corpulmonale, sct_cardiomyopathy))
})
