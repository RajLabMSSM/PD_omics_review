#' Merge data from excel sheets
#' 
#' #' @examples  
#' dat <- merge_data(googledrive_url="https://docs.google.com/spreadsheets/d/19jz9l2P7W2f1PWT9t3x0L8h6VQL021RdRI7D4FAOjX0/edit?usp=sharing#gid=1366974120",
#'                   file_name = "TableS2.xlsx",
#'                   sheet_search = "corr")
merge_data <- function(googledrive_url,
                       file_name,
                       sheet_search,
                       save_dir= here::here("data/metaanalysis"),
                       target_cols = c("Reference",
                                       "Source",
                                       "Dataset"),
                       exclude_sources = c("step1_2_summary.txt"),
                       save_merged = TRUE,
                       keep_extra_cols = FALSE
                       ){ 
  if(sheet_search=="tissues"){
    target_cols <- c("Tissue","Sig","PP.H4",target_cols)
    labels <- c("Tissue")
  } else if (sheet_search=="celltypes"){
    target_cols <- c("Cell_Type","Sig","Count",target_cols)
    labels <- c("Cell_Type")
  } else if (sheet_search=="corr"){
    target_cols <- c("Trait1","Trait2","corr",target_cols)
    labels <- c("Trait1","Trait2")
  }
  target_cols <- unique(target_cols)
  labels <- unique(labels)
  #### Download data from Google Drive ####
  file_path <- file.path(save_dir,file_name)
  dir.create(dirname(file_path),showWarnings = FALSE, recursive = TRUE)
  googledrive::drive_download(file = googledrive_url,
                              path = file_path,
                              overwrite = TRUE)
  #### Get sheets ####
  sheets <- readxl::excel_sheets(file_path)  
  target_sheets <- sheets[endsWith(sheets,sheet_search)]
  if(length(target_sheets)==0) stop("No sheets identified matching sheet_search.")
  
  #### Merge data #####
  dat_merged <- lapply(target_sheets, function(sheet){
    print(sheet)
    dat <- readxl::read_excel(path = file_path, 
                              sheet = sheet,
                              na = "NA")
    dat$Sheet <- gsub(paste0("_",sheet_search),"",sheet) 
    cols <- target_cols[target_cols %in% colnames(dat)]
    missing_cols <- target_cols[!target_cols %in% colnames(dat)]
    if(length(missing_cols)>0){
      message("Warning: Missing target_cols:\n",
              paste("-",missing_cols,collapse = "\n"))  
    }
    return(dplyr::relocate(dat, dplyr::all_of(cols), .before = 1))
  }) %>% `names<-`(target_sheets) %>%
    data.table::rbindlist(fill = TRUE)
  #### Exclude certain data ####
  if("Source" %in% colnames(dat_merged) && length(exclude_sources)>0) {
    dat_merged <- subset(dat_merged, !(Source %in% exclude_sources))
  }
  #### Harmonise strings ####
  message("Harmonising labels:\n",paste("-",labels,collapse = "\n"))
  dat_merged <- dat_merged %>% 
    dplyr::mutate_at(.vars = labels,
                     .funs = fix_str)
  #### Remove dataset-specific columns ####
  if(keep_extra_cols==FALSE){
    message("Dropping dataset-specific columns.")
    target_cols <- c("Sheet", target_cols)
    dat_merged <- dat_merged[, ..target_cols]
  }
  #### Report ####
  message(formatC(nrow(dat_merged),big.mark = ",")," rows merged.")
  #### Save results ####
  if(save_merged){
    merged_path <- file.path(save_dir, paste0("merged_",sheet_search,".csv"))
    message("Saving merged data: ==> ",merged_path)
    data.table::fwrite(x = dat_merged,
                       file = merged_path)
  }
  return(dat_merged)
}


## Harmonise names
fix_str <- function(vec){
  vec %>% 
    tolower() %>%
    gsub(pattern = "[ ]|-|[.]|[:]|[\\]|[\\/]",replacement = "_") %>%
    gsub(pattern = "\'|\"|[(]|[)]",replacement = "") %>%
    gsub(pattern = "_+",replacement = "_") %>%
    # stringr::str_wrap(width = 50) %>%
    stringr::str_trunc(width = 50) 
}

## FDR plots
plot_fdr <- function(dat,
                     x="Score",
                     y="-log10(FDR)",
                     color="Sheet",
                     size=y,
                     label=NULL,
                     show_plot=FALSE){
  gg <- ggplot(dat, 
               aes_string(x=x, y=y, color=color, size=size, 
                          label=label)) + 
    geom_point(alpha=.5) + 
    theme_bw()
  if(show_plot) print(gg)
  return(gg)
}


#' Interactive DT
#'
#' Generate an interactive data table with download buttons.
#'
#' @family general
#' @keywords internal
createDT <- function(DF, caption="", scrollY=400){
  data <- DT::datatable(DF, caption=caption,
                        extensions = 'Buttons',
                        options = list( dom = 'Bfrtip',
                                        buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                        scrollY = scrollY, scrollX=T, scrollCollapse = T, paging = F,
                                        columnDefs = list(list(className = 'dt-center', targets = "_all"))
                        )
  )
  return(data)
}

