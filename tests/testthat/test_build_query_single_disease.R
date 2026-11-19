# library(NVIdb)
# library(NVIpjsr)
library(testthat)

test_that("build query single disease, analytt only", {
  queries <- build_query_single_disease(period = 2020,
                                        analytt = c("01220104%", "1502010235"))

  correct_result <- paste("SELECT *",
                          "FROM v_sak_prove_konkl",
                          "WHERE aar = 2020 AND",
                          "(konkl_analyttkode = '1502010235' OR konkl_analyttkode LIKE '01220104%')")
  expect_equal(queries["v_sak_prove_konkl"][[1]], correct_result, ignore_attr = TRUE)

  correct_result <- paste("SELECT *",
                          "FROM v_sak_prove_res",
                          "WHERE aar = 2020 AND",
                          "(analyttkode_funn = '1502010235' OR analyttkode_funn LIKE '01220104%')")
  expect_equal(queries["v_sak_prove_res"][[1]], correct_result, ignore_attr = TRUE)

  correct_result <- paste("SELECT TOP (0) * FROM v_sak_prove_und")
  expect_equal(queries["v_sak_prove_und"][[1]], correct_result, ignore_attr = TRUE)

  correct_result <- paste("SELECT TOP (0) * FROM v2_sak_m_res")
  expect_equal(queries["v2_sak_m_res"][[1]], correct_result, ignore_attr = TRUE)

  correct_result <- paste("SELECT v_sakskonklusjon.*, sak.mottatt_dato, sak.uttaksdato, sak.sak_avsluttet,",
                          "sak.hensiktkode, sak.eier_lokalitetstype, sak.eier_lokalitetnr",
                          "FROM v_innsendelse AS sak",
                          "INNER JOIN v_sakskonklusjon",
                          "ON (v_sakskonklusjon.aar = sak.aar AND",
                          "v_sakskonklusjon.ansvarlig_seksjon = sak.ansvarlig_seksjon AND",
                          "v_sakskonklusjon.innsendelsesnummer = sak.innsendelsesnummer)",
                          "WHERE sak.aar = 2020 AND",
                          "(analyttkode = '1502010235' OR analyttkode LIKE '01220104%')")
  expect_equal(queries["v_sakskonklusjon"][[1]], correct_result, ignore_attr = TRUE)
})

test_that("build query single disease, analytt and metode", {
  queries <- build_query_single_disease(period = 2020,
                                        analytt = c("01220104%", "1502010235"),
                                        metode = c("070070", "070231"))

  correct_result <- paste("SELECT *",
                          "FROM v_sak_prove_konkl",
                          "WHERE aar = 2020 AND",
                          "(konkl_analyttkode = '1502010235' OR konkl_analyttkode LIKE '01220104%')")
  expect_equal(queries["v_sak_prove_konkl"][[1]], correct_result, ignore_attr = TRUE)

  correct_result <- paste("SELECT *",
                          "FROM v_sak_prove_res",
                          "WHERE aar = 2020 AND",
                          "(metodekode IN ('070070', '070231') OR",
                          "analyttkode_funn = '1502010235' OR",
                          "analyttkode_funn LIKE '01220104%')")
  expect_equal(queries["v_sak_prove_res"][[1]], correct_result, ignore_attr = TRUE)

  correct_result <- paste("SELECT TOP (0) * FROM v_sak_prove_und")
  expect_equal(queries["v_sak_prove_und"][[1]], correct_result, ignore_attr = TRUE)

  correct_result <- paste("SELECT *",
                          "FROM v2_sak_m_res",
                          "WHERE aar = 2020 AND",
                          "analyttkode_funn IS NULL AND",
                          "konkl_analyttkode IS NULL AND konklusjonkode IS NOT NULL",
                          "AND (metodekode IN ('070070', '070231'))")
  expect_equal(queries["v2_sak_m_res"][[1]], correct_result, ignore_attr = TRUE)

  correct_result <- paste("SELECT v_sakskonklusjon.*, sak.mottatt_dato, sak.uttaksdato, sak.sak_avsluttet,",
                          "sak.hensiktkode, sak.eier_lokalitetstype, sak.eier_lokalitetnr",
                          "FROM v_innsendelse AS sak",
                          "INNER JOIN v_sakskonklusjon",
                          "ON (v_sakskonklusjon.aar = sak.aar AND",
                          "v_sakskonklusjon.ansvarlig_seksjon = sak.ansvarlig_seksjon AND",
                          "v_sakskonklusjon.innsendelsesnummer = sak.innsendelsesnummer)",
                          "WHERE sak.aar = 2020 AND",
                          "(analyttkode = '1502010235' OR analyttkode LIKE '01220104%')")
  expect_equal(queries["v_sakskonklusjon"][[1]], correct_result, ignore_attr = TRUE)
})

test_that("build query single disease, analytt and hensikt", {
  queries <- build_query_single_disease(period = 2020,
                                        analytt = c("01220104%", "1502010235"),
                                        hensikt = c("0100101007", "0100102003", "0200130%"))

  correct_result <- paste("SELECT *",
                          "FROM v_sak_prove_konkl",
                          "WHERE aar = 2020 AND",
                          "(konkl_analyttkode = '1502010235' OR konkl_analyttkode LIKE '01220104%' OR",
                          "((hensiktkode IN ('0100101007', '0100102003') OR hensiktkode LIKE '0200130%') AND",
                          "konkl_analyttkode IS NULL))")
  expect_equal(queries["v_sak_prove_konkl"][[1]], correct_result, ignore_attr = TRUE)

  correct_result <- paste("SELECT *",
                          "FROM v_sak_prove_res",
                          "WHERE aar = 2020 AND",
                          "(analyttkode_funn = '1502010235' OR",
                          "analyttkode_funn LIKE '01220104%' OR",
                          "((hensiktkode IN ('0100101007', '0100102003') OR hensiktkode LIKE '0200130%') AND",
                          "metodekode IS NULL))")
  expect_equal(queries["v_sak_prove_res"][[1]], correct_result, ignore_attr = TRUE)

  correct_result <- paste("SELECT *",
                          "FROM v_sak_prove_und",
                          "WHERE aar = 2020 AND",
                          "(hensiktkode IN ('0100101007', '0100102003') OR hensiktkode LIKE '0200130%')")
  expect_equal(queries["v_sak_prove_und"][[1]], correct_result, ignore_attr = TRUE)

  correct_result <- paste("SELECT TOP (0) * FROM v2_sak_m_res")
  expect_equal(queries["v2_sak_m_res"][[1]], correct_result, ignore_attr = TRUE)

  correct_result <- paste("SELECT v_sakskonklusjon.*, sak.mottatt_dato, sak.uttaksdato, sak.sak_avsluttet,",
                          "sak.hensiktkode, sak.eier_lokalitetstype, sak.eier_lokalitetnr",
                          "FROM v_innsendelse AS sak",
                          "INNER JOIN v_sakskonklusjon",
                          "ON (v_sakskonklusjon.aar = sak.aar AND",
                          "v_sakskonklusjon.ansvarlig_seksjon = sak.ansvarlig_seksjon AND",
                          "v_sakskonklusjon.innsendelsesnummer = sak.innsendelsesnummer)",
                          "WHERE sak.aar = 2020 AND",
                          "(analyttkode = '1502010235' OR analyttkode LIKE '01220104%')")
  expect_equal(queries["v_sakskonklusjon"][[1]], correct_result, ignore_attr = TRUE)
})

test_that("build query single disease, analytt and utbrudd", {
  queries <- build_query_single_disease(period = 2020,
                                        analytt = c("01220104%", "1502010235"),
                                        utbrudd = c("22", "52"))

  correct_result <- paste("SELECT *",
                          "FROM v_sak_prove_konkl",
                          "WHERE aar = 2020 AND",
                          "(konkl_analyttkode = '1502010235' OR konkl_analyttkode LIKE '01220104%' OR",
                          "((utbrudd_id IN ('22', '52')) AND",
                          "konkl_analyttkode IS NULL))")
  expect_equal(queries["v_sak_prove_konkl"][[1]], correct_result, ignore_attr = TRUE)

  correct_result <- paste("SELECT *",
                          "FROM v_sak_prove_res",
                          "WHERE aar = 2020 AND",
                          "(analyttkode_funn = '1502010235' OR",
                          "analyttkode_funn LIKE '01220104%' OR",
                          "((utbrudd_id IN ('22', '52')) AND",
                          "metodekode IS NULL))")
  expect_equal(queries["v_sak_prove_res"][[1]], correct_result, ignore_attr = TRUE)

  correct_result <- paste("SELECT *",
                          "FROM v_sak_prove_und",
                          "WHERE aar = 2020 AND",
                          "(utbrudd_id IN ('22', '52'))")
  expect_equal(queries["v_sak_prove_und"][[1]], correct_result, ignore_attr = TRUE)

  correct_result <- paste("SELECT TOP (0) * FROM v2_sak_m_res")
  expect_equal(queries["v2_sak_m_res"][[1]], correct_result, ignore_attr = TRUE)

  correct_result <- paste("SELECT v_sakskonklusjon.*, sak.mottatt_dato, sak.uttaksdato, sak.sak_avsluttet,",
                          "sak.hensiktkode, sak.eier_lokalitetstype, sak.eier_lokalitetnr",
                          "FROM v_innsendelse AS sak",
                          "INNER JOIN v_sakskonklusjon",
                          "ON (v_sakskonklusjon.aar = sak.aar AND",
                          "v_sakskonklusjon.ansvarlig_seksjon = sak.ansvarlig_seksjon AND",
                          "v_sakskonklusjon.innsendelsesnummer = sak.innsendelsesnummer)",
                          "WHERE sak.aar = 2020 AND",
                          "(analyttkode = '1502010235' OR analyttkode LIKE '01220104%')")
  expect_equal(queries["v_sakskonklusjon"][[1]], correct_result, ignore_attr = TRUE)
})



test_that("build query single disease, analytt, hensikt and utbrudd", {
  queries <- build_query_single_disease(period = 2020,
                                        analytt = c("01220104%", "1502010235"),
                                        hensikt = c("0100101007", "0100102003", "0200130%"),
                                        utbrudd = c("22", "52"))

  correct_result <- paste("SELECT *",
                          "FROM v_sak_prove_konkl",
                          "WHERE aar = 2020 AND",
                          "(konkl_analyttkode = '1502010235' OR konkl_analyttkode LIKE '01220104%' OR",
                          "((hensiktkode IN ('0100101007', '0100102003') OR hensiktkode LIKE '0200130%' OR",
                          "utbrudd_id IN ('22', '52')) AND",
                          "konkl_analyttkode IS NULL))")
  expect_equal(queries["v_sak_prove_konkl"][[1]], correct_result, ignore_attr = TRUE)

  correct_result <- paste("SELECT *",
                          "FROM v_sak_prove_res",
                          "WHERE aar = 2020 AND",
                          "(analyttkode_funn = '1502010235' OR",
                          "analyttkode_funn LIKE '01220104%' OR",
                          "((hensiktkode IN ('0100101007', '0100102003') OR hensiktkode LIKE '0200130%' OR",
                          "utbrudd_id IN ('22', '52')) AND",
                          "metodekode IS NULL))")
  expect_equal(queries["v_sak_prove_res"][[1]], correct_result, ignore_attr = TRUE)

  correct_result <- paste("SELECT *",
                          "FROM v_sak_prove_und",
                          "WHERE aar = 2020 AND",
                          "(hensiktkode IN ('0100101007', '0100102003') OR hensiktkode LIKE '0200130%' OR",
                          "utbrudd_id IN ('22', '52'))")
  expect_equal(queries["v_sak_prove_und"][[1]], correct_result, ignore_attr = TRUE)

  correct_result <- paste("SELECT TOP (0) * FROM v2_sak_m_res")
  expect_equal(queries["v2_sak_m_res"][[1]], correct_result, ignore_attr = TRUE)

  correct_result <- paste("SELECT v_sakskonklusjon.*, sak.mottatt_dato, sak.uttaksdato, sak.sak_avsluttet,",
                          "sak.hensiktkode, sak.eier_lokalitetstype, sak.eier_lokalitetnr",
                          "FROM v_innsendelse AS sak",
                          "INNER JOIN v_sakskonklusjon",
                          "ON (v_sakskonklusjon.aar = sak.aar AND",
                          "v_sakskonklusjon.ansvarlig_seksjon = sak.ansvarlig_seksjon AND",
                          "v_sakskonklusjon.innsendelsesnummer = sak.innsendelsesnummer)",
                          "WHERE sak.aar = 2020 AND",
                          "(analyttkode = '1502010235' OR analyttkode LIKE '01220104%')")
  expect_equal(queries["v_sakskonklusjon"][[1]], correct_result, ignore_attr = TRUE)
})



test_that("build_query_single_disease error testing", {

  linewidth <- options("width")
  options(width = 80)

  expect_error(build_query_single_disease(analytt = c("01220104%", "1502010235")),
               regexp = 'argument "period" is missing')

  expect_error(build_query_single_disease(period = 2020, metode = "070070"),
               regexp = 'argument "analytt" is missing, with no default', fixed = TRUE)

  expect_error(build_query_single_disease(period = (as.numeric(format(Sys.Date(), "%Y")) + 1),
                                          analytt = c("01220104%", "1502010235")),
               regexp = "Element 1 is not <= ")

  expect_error(build_query_single_disease(analytt = c("01220104%", "1502010235"),
                                          period = 2020,
                                          metode = "0"),
               regexp = "Variable 'metode': All elements must have at least 2 characters", fixed = TRUE)

  expect_error(build_query_single_disease(analytt = c("01220104%", "1502010235"),
                                          period = 2020,
                                          utbrudd = 123),
               regexp = "Variable 'utbrudd': Must be of type 'character' (or 'NULL'), not", fixed = TRUE)

  expect_error(build_query_single_disease(analytt = c("01220104%", "1502010235"),
                                          period = 2020,
                                          hensikt = 123),
               regexp = "Variable 'hensikt': Must be of type 'character' (or 'NULL'),", fixed = TRUE)

  options(width = unlist(linewidth))
})
