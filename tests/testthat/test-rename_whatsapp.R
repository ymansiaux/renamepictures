test_that("rename_a_picture works", {
  expect_equal(
    rename_a_picture("IMG-20250104-WA0006.jpg"),
    "IMG_20250104_000000.jpg"
  )

  expect_equal(
    rename_a_picture("IMG-20250104-WA0006.jpg", time = "123456"),
    "IMG_20250104_123456.jpg"
  )
})

test_that("rename_many_pictures works", {
  pic_list <- c(
    "IMG-20250104-WA0006.jpg",
    "IMG-20250104-WA0007.jpg",
    "IMG-20250105-WA0008.jpg"
  )

  df_res <-
    data.frame(
      old_name = pic_list,
      new_name_before_renaming = c(
        "IMG_20250104_000000.jpg",
        "IMG_20250104_000000.jpg",
        "IMG_20250105_000000.jpg"
      ),
      new_name_after_renaming = c(
        "IMG_20250104_000000_1.jpg",
        "IMG_20250104_000000_2.jpg",
        "IMG_20250105_000000_1.jpg"
      )
    )

  expect_equal(
    rename_many_pictures(pic_list),
    structure(list(old_name = c(
      "IMG-20250104-WA0006.jpg", "IMG-20250104-WA0007.jpg",
      "IMG-20250105-WA0008.jpg"
    ), new_name_before_renaming = c(
      "IMG_20250104_000000.jpg",
      "IMG_20250104_000000.jpg", "IMG_20250105_000000.jpg"
    ), new_name_after_renaming = c(
      "IMG_20250104_000000_1.jpg",
      "IMG_20250104_000000_2.jpg", "IMG_20250105_000000_1.jpg"
    )), row.names = c(
      NA,
      -3L
    ), class = c("tbl_df", "tbl", "data.frame"))
  )
})

test_that("add_id_to_filename works", {
  expect_equal(
    add_id_to_filename("IMG_20250104_000000.jpg", 1),
    "IMG_20250104_000000_1.jpg"
  )
})


test_that("rename_whatsapp works", {
  expect_error(
    rename_whatsapp(path = "truc"),
    "le dossier specifie n'existe pas"
  )

  dir_photo <- tempfile("dirphoto")
  dir.create(dir_photo)

  expect_message(
    rename_whatsapp(path = dir_photo),
    "le dossier est vide"
  )

  file.create(file.path(dir_photo, "IMG-20250104-WA0006.jpg"))
  file.create(file.path(dir_photo, "IMG-20250104-WA0007.jpg"))
  file.create(file.path(dir_photo, "IMG-20250105-WA0008.jpg"))

  rename_whatsapp(path = dir_photo)

  expect_true(
    file.exists(
      file.path(dir_photo, "renamed_pictures", "IMG_20250104_000000_1.jpg")
    )
  )

  expect_true(
    file.exists(
      file.path(dir_photo, "renamed_pictures", "IMG_20250104_000000_2.jpg")
    )
  )


  expect_true(
    file.exists(
      file.path(dir_photo, "renamed_pictures", "IMG_20250105_000000_1.jpg")
    )
  )

  unlink(dir_photo, recursive = TRUE)
})
