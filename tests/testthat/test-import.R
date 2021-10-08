test_that("import data", {
    example_xlsx <- system.file("extdata/expdb_example.xlsx", package = "expDB")
    db_file <- tempfile(fileext = ".db")
    con <- expdbCreateDB(db_file)
    dbImportXLSX(con, example_xlsx, tz = "Australia/Brisbane", 
                 ignore_genotype = FALSE, ignore_trait = FALSE)    
})


