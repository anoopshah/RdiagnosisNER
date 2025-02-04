require(testthat)
require(Rdiagnosislist)
require(RdiagnosisNER)
require(bit64)
require(data.table)
require(spacyr)

context('Testing NER functions')

test_that('Testing NER sentence -- simple', {
	# Create CDB for NER
	data.table::setDTthreads(threads = 1)
	SNOMED <- Rdiagnosislist::sampleSNOMED()
	miniCDB <- createCDB(SNOMED = SNOMED)
	miniCDB <- addLemmaToCDB(miniCDB)

	# SNOMED concepts for testing
	sct_cardiomyopathy <- SNOMEDconcept('Cardiomyopathy',
		SNOMED = SNOMED)
	zeroconcept <- sct_cardiomyopathy[0]

	# Test NER sentence - simple
	OUT <- NERsentence('Cardiomyopathy',
		CDB = miniCDB, SNOMED = SNOMED)
	expect_setequal(attr(OUT, 'findings')$conceptId,
		sct_cardiomyopathy)

	# Test NER sentence - simple with no concepts
	OUT <- NERsentence('No concepts here',
		CDB = miniCDB, SNOMED = SNOMED)
	expect_setequal(sort(attr(OUT, 'findings')$conceptId),
		zeroconcept)
	
	OUT <- NERsentence('Adjectival modifier',
		CDB = miniCDB, SNOMED = SNOMED)
	expect_setequal(sort(attr(OUT, 'findings')$conceptId),
		zeroconcept)
})

test_that('Testing NER sentence with concept composition', {
	# Create CDB for NER
	data.table::setDTthreads(threads = 1)
	SNOMED <- Rdiagnosislist::sampleSNOMED()
	miniCDB <- createCDB(SNOMED = SNOMED,
		MANUAL_SYNONYMS = NULL)
	DEC <- decompose(SNOMEDconcept(c('83291003', '19829001'),
		SNOMED = SNOMED), CDB = miniCDB, SNOMED = SNOMED)
	miniCDB <- addComposeLookupToCDB(DEC, CDB = miniCDB)
	miniCDB <- addLemmaToCDB(miniCDB)

	# SNOMED concepts for testing
	sct_corpulmonale <- SNOMEDconcept('Cor pulmonale',
		SNOMED = SNOMED)
	sct_lungdisease <- SNOMEDconcept('Disorder of lung',
		SNOMED = SNOMED)

	# Test NER sentence with SNOMED concept composition
	OUT <- NERsentence('Right heart failure caused by lung disease',
		CDB = miniCDB, SNOMED = SNOMED)
	expect_setequal(attr(OUT, 'findings')$conceptId,
		c(sct_lungdisease, sct_corpulmonale))
})

test_that('Testing NER document and corpus', {
	# Create CDB for NER
	data.table::setDTthreads(threads = 1)
	SNOMED <- Rdiagnosislist::sampleSNOMED()
	miniCDB <- createCDB(SNOMED = SNOMED,
		MANUAL_SYNONYMS = NULL)
	DEC <- decompose(SNOMEDconcept(c('83291003', '19829001'),
		SNOMED = SNOMED), CDB = miniCDB, SNOMED = SNOMED)
	miniCDB <- addComposeLookupToCDB(DEC, CDB = miniCDB)
	miniCDB <- addLemmaToCDB(miniCDB)

	# SNOMED concepts for testing
	sct_corpulmonale <- SNOMEDconcept('Cor pulmonale',
		SNOMED = SNOMED)
	sct_lungdisease <- SNOMEDconcept('Disorder of lung',
		SNOMED = SNOMED)
	sct_cardiomyopathy <- SNOMEDconcept('Cardiomyopathy',
		SNOMED = SNOMED)
	zeroconcept <- sct_cardiomyopathy[0]

	# Test NER document
	OUT <- NERdocument('Cardiomyopathy. 
		Right heart failure caused by lung disease',
		CDB = miniCDB, SNOMED = SNOMED)
	expect_equal(attr(OUT, 'findings')[sentence == 1]$conceptId,
		sct_cardiomyopathy)
	expect_equal(sort(attr(OUT, 'findings')[sentence == 2]$conceptId),
		c(sct_lungdisease, sct_corpulmonale))

	# Test NER corpus
	OUT <- NERcorpus(c('No concepts here', 'Cardiomyopathy. 
		Right heart failure caused by lung disease'), 1:2,
		CDB = miniCDB, SNOMED = SNOMED)
	expect_equal(OUT[id == 1]$conceptId, zeroconcept)
	expect_setequal(OUT[id == 2]$conceptId,
		c(sct_lungdisease, sct_corpulmonale, sct_cardiomyopathy))
})
