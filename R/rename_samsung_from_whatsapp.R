#' Remove WhatsApp-style prefix from file names
#'
#' This function removes "IMG-" or "VID-" from the start of file names
#' and copies them to a "renamed_pictures" folder.
#'
#' @param path Character string of the source directory path
#' @return Invisible list of processed files
#' @importFrom cli cli_alert_info cli_alert_success
#' @export
rename_samsung_from_whatsapp <- function(path) {
    if (!dir.exists(path)) {
        stop("The specified folder does not exist")
    }

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

    all_files_basename <- basename(all_files)

    cli_alert_info(
        paste0(
            "The following files were detected:\n",
            paste0(all_files_basename, collapse = "\n")
        )
    )

    # Remove "IMG-" or "VID-" prefix if present
    new_names <- sub("^(IMG-|VID-|IMG_|VID_)", "", all_files_basename, ignore.case = FALSE)

    cli_alert_info(
        paste0(
            "Their new names will be:\n",
            paste0(new_names, collapse = "\n")
        )
    )

    dest_folder <- file.path(path, "renamed_pictures")
    if (!dir.exists(dest_folder)) {
        dir.create(dest_folder)
        cli_alert_info("Created the renamed_pictures folder")
    } else {
        cli_alert_info("The renamed_pictures folder already exists")
    }

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
