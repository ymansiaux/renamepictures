test_that("add_prefix_to_filename works correctly", {
    # Test image files
    expect_equal(add_prefix_to_filename("20240101.jpg"), "IMG_20240101.jpg")
    expect_equal(add_prefix_to_filename("20240101_123.jpg"), "IMG_20240101_123.jpg")
    expect_equal(add_prefix_to_filename("/path/to/20240101.jpg"), file.path("/path/to", "IMG_20240101.jpg"))

    # Test video files
    expect_equal(add_prefix_to_filename("20240101.mp4"), "VID_20240101.mp4")
    expect_equal(add_prefix_to_filename("20240101_123.mov"), "VID_20240101_123.mov")

    # Test already prefixed files (should remain unchanged)
    expect_equal(add_prefix_to_filename("IMG_20240101.jpg"), "IMG_20240101.jpg")
    expect_equal(add_prefix_to_filename("VID_20240101.mp4"), "VID_20240101.mp4")

    # Test files without 8 digits at start (should remain unchanged)
    expect_equal(add_prefix_to_filename("photo_2024.jpg"), "photo_2024.jpg")
    expect_equal(add_prefix_to_filename("1234567.jpg"), "1234567.jpg")

    # Test non-image/video files (should remain unchanged)
    expect_equal(add_prefix_to_filename("20240101.txt"), "20240101.txt")
    expect_equal(add_prefix_to_filename("20240101.pdf"), "20240101.pdf")
})

test_that("rename_samsung creates folder and handles errors", {
    # Setup a temporary directory for testing
    temp_dir <- tempdir()
    test_dir <- file.path(temp_dir, "test_rename_samsung")
    dir.create(test_dir, showWarnings = FALSE)

    # Test with non-existent directory
    expect_error(rename_samsung("non-existent-directory"), "The specified folder does not exist")

    # Test with empty directory
    expect_message(rename_samsung(test_dir), "No image or video files found in the folder")

    # Create test files
    writeLines("test", file.path(test_dir, "20240101.jpg"))
    writeLines("test", file.path(test_dir, "20240102.mp4"))

    # Test processing files
    # Note: We can't fully test the copy operation in a unit test without mocking,
    # but we can verify the function runs without errors
    expect_no_error(rename_samsung(test_dir))

    # Verify the renamed_pictures folder was created
    expect_true(dir.exists(file.path(test_dir, "renamed_pictures")))

    expect_true(file.exists(file.path(test_dir, "renamed_pictures", "IMG_20240101.jpg")))
    expect_true(file.exists(file.path(test_dir, "renamed_pictures", "VID_20240102.mp4")))
    # Clean up
    unlink(test_dir, recursive = TRUE)
})
