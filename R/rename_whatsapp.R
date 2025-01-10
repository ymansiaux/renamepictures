#' @noRd
#' @importFrom tools file_ext
rename_a_picture <- function(pic, time = "000000") {
    file_extension <- tools::file_ext(pic)
    date <- regmatches(pic, regexpr("\\d{8}", pic))
    return(
        paste0("IMG_", date, "_", time, ".", file_extension)
    )
}

#' @noRd
#' @importFrom cli cli_alert_info cli_alert_success
rename_whatsapp <- function(path, time = "000000") {
    if (!dir.exists(path)) {
        stop("le dossier specifie n'existe pas")
    }

    pic_list <- list.files(path, pattern = "IMG.*WA.*jpg", full.names = TRUE)

    if (length(pic_list) == 0) {
        message("le dossier est vide")
        return(NULL)
    }

    pic_list_basename <- basename(pic_list)

    cli_alert_info(
        paste0(
            "Les fichiers suivants ont ete detectes:\n",
            paste0(pic_list_basename, collapse = "\n")
        )
    )

    new_names <- rename_many_pictures(pic_list_basename)

    cli_alert_info(
        paste0(
            "Leurs nouveaux noms seront:\n",
            paste0(new_names$new_name_after_renaming, collapse = "\n")
        )
    )

    if (!dir.exists(file.path(path, "renamed_pictures"))) {
        dir.create(file.path(path, "renamed_pictures"))
        cli_alert_info("Creation du dossier renamed_pictures")
    } else {
        cli_alert_info("Le dossier renamed_pictures existe deja")
    }

    invisible(
        purrr::map2(
            new_names$old_name,
            new_names$new_name_after_renaming,
            function(.x, .y) {
                file.copy(
                    file.path(path, .x),
                    file.path(path, "renamed_pictures", .y),
                    overwrite = TRUE
                )
            }
        )
    )
    cli_alert_success("Les fichiers ont ete renommes avec succes")
}


add_id_to_filename <- function(filename, id) {
    extension <- tools::file_ext(filename)
    return(
        paste0(
            sub(paste0("\\.", extension), paste0("_", id, ".", extension), filename)
        )
    )
}

#' @noRd
#' @importFrom dplyr group_by mutate ungroup select
#' @importFrom purrr map2
rename_many_pictures <- function(pic_list) {
    data.frame(
        old_name = pic_list,
        new_name_before_renaming = sapply(pic_list, rename_a_picture)
    ) |>
        dplyr::group_by(new_name_before_renaming) |>
        dplyr::mutate(id = dplyr::row_number()) |>
        dplyr::ungroup() |>
        dplyr::mutate(
            new_name_after_renaming = unlist(purrr::map2(.data[["new_name_before_renaming"]], .data[["id"]], add_id_to_filename))
        ) |>
        dplyr::select(-id)
}

# df_res |>
# dplyr::group_by(new_name_before_renaming) |>
# dplyr::mutate(id = dplyr::row_number())
