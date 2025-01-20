require(testthat)
require(Rdiagnosislist)
require(RdiagnosisNER)
require(bit64)
require(data.table)

context('Testing NER functions')

test_that('Testing NER', {
	# Create CDB for NER
	data.table::setDTthreads(threads = 1)
	miniSNOMED <- Rdiagnosislist::sampleSNOMED()
	miniCDB <- createCDB(SNOMED = miniSNOMED,
		MANUAL_SYNONYMS = NULL)
	DEC <- decompose(SNOMEDconcept(c('83291003', '19829001'),
		SNOMED = miniSNOMED), CDB = miniCDB, SNOMED = miniSNOMED)
	miniCDB <- addComposeLookupToCDB(DEC, CDB = miniCDB)
	miniCDB <- addLemmaToCDB(miniCDB)
	# At least one allergy concept must be included for this to work
	# for sample (would not be a problem with full SNOMED dictionary)
	miniCDB$allergyConcepts <- SNOMEDconcept('281647001',
		SNOMED = miniSNOMED)

	# SNOMED concepts for testing
	sct_corpulmonale <- SNOMEDconcept('Cor pulmonale',
		SNOMED = miniSNOMED)
	sct_lungdisease <- SNOMEDconcept('Disorder of lung',
		SNOMED = miniSNOMED)
	sct_cardiomyopathy <- SNOMEDconcept('Cardiomyopathy',
		SNOMED = miniSNOMED)
	sct_mi <- SNOMEDconcept('Myocardial infarction',
		SNOMED = miniSNOMED)

	# Test NER sentence
	OUT <- NERsentence('Right heart failure caused by lung disease',
		CDB = miniCDB, SNOMED = miniSNOMED)
	expect_equal(sort(attr(OUT, 'findings')$conceptId),
		c(sct_lungdisease, sct_corpulmonale))

	# Test NER document
	OUT <- NERdocument('Cardiomyopathy. 
		Right heart failure caused by lung disease',
		CDB = miniCDB, SNOMED = miniSNOMED)
	expect_equal(attr(OUT, 'findings')[sentence == 1]$conceptId,
		sct_cardiomyopathy)
	expect_equal(sort(attr(OUT, 'findings')[sentence == 2]$conceptId),
		c(sct_lungdisease, sct_corpulmonale))

	# Test NER corpus
	OUT <- NERcorpus(c('Myocardial infarction', 'Cardiomyopathy. 
		Right heart failure caused by lung disease'), 1:2,
		CDB = miniCDB, SNOMED = miniSNOMED)
	expect_equal(attr(OUT, 'findings')[id == 1]$conceptId,
		sct_mi)
	expect_equal(sort(attr(OUT, 'findings')[id == 2]$conceptId),
		c(sct_lungdisease, sct_corpulmonale, sct_cardiomyopathy))
})
