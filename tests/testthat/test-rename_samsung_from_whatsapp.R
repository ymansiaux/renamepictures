# Unit tests for rename_samsung_from_whatsapp
test_that("rename_samsung_from_whatsapp removes prefixes correctly", {
  tmp <- tempdir()
  files <- c("IMG-20240101.jpg", "VID-20240101.mp4", "20240101.jpg", "IMG_20240101.jpg")
  paths <- file.path(tmp, files)
  file.create(paths)

  # Run function
  rename_samsung_from_whatsapp(tmp)

  dest <- file.path(tmp, "renamed_pictures")
  expect_true(dir.exists(dest))
  expect_true(file.exists(file.path(dest, "20240101.jpg")))
  expect_true(file.exists(file.path(dest, "20240101.mp4")))
  expect_true(file.exists(file.path(dest, "20240101.jpg"))) # from both IMG- and no prefix
  expect_false(file.exists(file.path(dest, "IMG_20240101.jpg"))) # should remain unchanged

  # Clean up
  unlink(file.path(tmp, "renamed_pictures"), recursive = TRUE)
  file.remove(paths)
})

test_that("rename_samsung_from_whatsapp handles empty folders", {
  tmp <- tempdir()
  dir.create(file.path(tmp, "emptydir"))
  expect_invisible(rename_samsung_from_whatsapp(file.path(tmp, "emptydir")))
  unlink(file.path(tmp, "emptydir"), recursive = TRUE)
})
