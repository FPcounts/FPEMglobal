### Test access to bundled files

test_that("All files in 'pkg_files_included()' actually exist.", {
    fn <- pkg_files_included(result = "filepath")
    f_exist <- rapply(fn, f = function(z) file.exists(z), classes = "character")
    expect_true(all(f_exist))
})
