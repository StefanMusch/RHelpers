#' @title Create an RMD report template in your working directory.
#' @description Create an RMD report template for PDF's, FlexDashboard, HTML (WiP), Shiny (WiP) and Power Points (WiP).
#' 
#' @return Copies a .rmd template to a reports folder in your current working directory with required dependencies (.css or .png files). 
#' Never overwrites files with the same names. 
#' 
#' @param dir Where the template should be created? By default your working directory. 
#' Automatically puts the files in a 'report' folder. If there is already a 'reports'
#' folder in the specified directory, it will put the files in there.
#' @param type The type of the project. Values can be 'pdf', 'flexdb', 'html', 'shiny' or 'ppt'. Default is "pdf"
#' 
#' 
#' @export
create_RMD_report <- function(dir = getwd(), type = "pdf"){
    stopifnot(is.character(type) & type %in% c("pdf", "flexdb", "html", "shiny", "ppt"))
    
    if(type %in% c("pdf", "flexdb")){
        
        #Creates a reports directory in your working directory if it does not yet exist. 
        ifelse(!dir.exists(file.path(dir, "reports")),
               dir.create(file.path(dir, "reports")), invisible(FALSE))
        
        #Directory of the chosen template
        template_dir <- system.file(paste0('markdown_templates/', type), package = 'g6tr')
        
        #Copy files into the new reports directory silently. Never overwrites same filename.
        invisible(file.copy(from = list.files(template_dir, full.names = TRUE),
                            to = file.path(dir,"reports"),
                            overwrite = FALSE,
                            recursive = TRUE))
        
        message(paste0("Template created in the directory:", dir))
        
        
    } else {
        stop("We currently have no working ", type, " R Markdown template yet ")
    }
}

#' @example 
#' \dontrun{
#'   g6tr_rmd_report(type = 'pdf')
#' }