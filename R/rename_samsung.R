#' Add appropriate prefix to file names
#'
#' This function adds "IMG_" to image files and "VID_" to video files
#' that start with 8 digits.
#'
#' @param filename Character string of the filename
#' @return Character string of the processed filename
#' @importFrom tools file_ext
#' @export
add_prefix_to_filename <- function(filename) {
    # Get file extension
    file_extension <- tools::file_ext(filename)

    # Check if filename contains 8 digits at the beginning
    has_digits <- grepl("^\\d{8,8}", basename(filename))

    # Skip if filename already has IMG_ or VID_ prefix
    already_has_prefix <- grepl("^(IMG|VID)_", basename(filename))

    if (!has_digits || already_has_prefix) {
        return(filename) # Return unchanged if no match or already prefixed
    }

    # Extract the 8 digits
    digits <- regmatches(basename(filename), regexpr("^\\d{8,8}", basename(filename)))

    # Determine if it's an image or video file
    is_image <- tolower(file_extension) %in% c("jpg", "jpeg", "png", "gif", "bmp", "tiff", "webp")
    is_video <- tolower(file_extension) %in% c("mp4", "mov", "avi", "wmv", "mkv", "webm", "flv")

    if (is_image) {
        prefix <- "IMG_"
    } else if (is_video) {
        prefix <- "VID_"
    } else {
        return(filename) # Not an image or video file, return unchanged
    }

    # Construct the new filename
    new_name <- sub(
        paste0("^", digits),
        paste0(prefix, digits),
        basename(filename)
    )

    # If full path was provided, maintain it
    if (dirname(filename) != ".") {
        new_name <- file.path(dirname(filename), new_name)
    }

    return(new_name)
}


# ...existing code...

#' Process and copy files with prefixes to a destination folder
#'
#' This function processes image and video files by adding the appropriate prefix
#' to those starting with 8 digits and copies them to a "renamed_pictures" folder.
#'
#' @param path Character string of the source directory path
#' @return Invisible list of processed files
#' @importFrom cli cli_alert_info cli_alert_success
#' @export
rename_samsung <- function(path) {
    if (!dir.exists(path)) {
        stop("The specified folder does not exist")
    }

    # Get all image and video files
    file_extensions <- c(
        "jpg", "jpeg", "png", "gif", "bmp", "tiff", "webp",
        "mp4", "mov", "avi", "wmv", "mkv", "webm", "flv"
    )
    pattern <- paste0("\\.(", paste(file_extensions, collapse = "|"), ")$")

    all_files <- list.files(path, pattern = pattern, full.names = TRUE, ignore.case = TRUE)

    if (length(all_files) == 0) {
        message("No image or video files found in the folder")
        return(invisible(NULL))
    }

    # Get basenames for display
    all_files_basename <- basename(all_files)

    cli_alert_info(
        paste0(
            "The following files were detected:\n",
            paste0(all_files_basename, collapse = "\n")
        )
    )

    # Process file names
    new_names <- sapply(all_files_basename, add_prefix_to_filename)

    cli_alert_info(
        paste0(
            "Their new names will be:\n",
            paste0(new_names, collapse = "\n")
        )
    )

    # Create destination folder if it doesn't exist
    dest_folder <- file.path(path, "renamed_pictures")
    if (!dir.exists(dest_folder)) {
        dir.create(dest_folder)
        cli_alert_info("Created the renamed_pictures folder")
    } else {
        cli_alert_info("The renamed_pictures folder already exists")
    }

    # Copy files with new names to destination folder
    result <- Map(function(old_file, new_name) {
        file.copy(
            old_file,
            file.path(dest_folder, new_name),
            overwrite = TRUE
        )
    }, all_files, new_names)

    cli_alert_success("Files have been successfully renamed and copied")

    invisible(list(
        source_files = all_files,
        new_names = new_names
    ))
}
