require(testthat)
require(Rdiagnosislist)
require(RdiagnosisNER)
require(bit64)
require(data.table)

context('Testing spacy-based link detection functions')

test_that('Testing addAnd', {
	data('testlink_data')
	expect_equal(unique(testlink_from_data(
		testlink_data[linkFunction == 'addAnd'])), 'TRUE')
})

test_that('Testing findLaterality', {
	data('testlink_data')
	expect_equal(unique(testlink_from_data(
		testlink_data[linkFunction == 'findLaterality'])), 'TRUE')
})

test_that('Testing findAttr', {
	data('testlink_data')
	expect_equal(unique(testlink_from_data(
		testlink_data[linkFunction == 'findAttr'])), 'TRUE')
})

test_that('Testing findBody', {
	data('testlink_data')
	expect_equal(unique(testlink_from_data(
		testlink_data[linkFunction == 'findBody'])), 'TRUE')
})

test_that('Testing findCause', {
	data('testlink_data')
	expect_equal(unique(testlink_from_data(
		testlink_data[linkFunction == 'findCause'])), 'TRUE')
})

test_that('Testing findAllergy', {
	data('testlink_data')
	expect_equal(unique(testlink_from_data(
		testlink_data[linkFunction == 'findAllergy'])), 'TRUE')
})
