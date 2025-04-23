# --- Script to Save sf objects ending in _sf to RDS files ---

#' Saves sf objects from the Global Environment ending with "_sf" to individual RDS files.
#'
#' This function iterates through all objects in the .GlobalEnv,
#' identifies objects that inherit from the 'sf' class and whose names
#' end with the suffix '_sf', and saves each identified object to an
#' RDS file named after the object in the current working directory.
#'
#' @return Invisibly returns a character vector of the files saved, or NULL if none were saved.
#'         Prints messages to the console indicating which files are being saved.
#' @export
#'
#' @examples
#' \dontrun{
#' # Assuming you have sf objects named roads_sf, buildings_sf etc. in your environment:
#' # save_sf_objects_to_rds()
#' # This would create roads_sf.rds, buildings_sf.rds etc. in your working directory.
#' }
save_sf_objects_to_rds <- function() {
  
  # Get names of all objects in the Global Environment
  all_object_names <- ls(envir = .GlobalEnv)
  
  # Vector to store names of files saved
  saved_files <- character()
  
  print(paste("Searching for sf objects ending with '_sf' in the Global Environment..."))
  print(paste("RDS files will be saved to the current working directory:", getwd()))
  
  # Loop through each object name
  for (obj_name in all_object_names) {
    
    # Check if the name ends with '_sf' (quick check before getting the object)
    if (endsWith(obj_name, "_sf")) {
      
      # Get the actual object from the environment
      # Using tryCatch just in case 'get' fails for some reason (unlikely for ls results)
      obj <- tryCatch(get(obj_name, envir = .GlobalEnv), error = function(e) NULL)
      
      # Check if retrieval worked and the object inherits from 'sf' class
      if (!is.null(obj) && inherits(obj, "sf")) {
        
        # Construct the output RDS filename
        rds_filename <- paste0("Data/SF/", obj_name, ".rds")
        
        # Print message before saving
        print(paste(" --> Found sf object:", obj_name, "- Saving to:", rds_filename))
        
        # Save the object using saveRDS
        # Add basic error handling for the save operation
        save_success <- tryCatch({
          saveRDS(obj, file = rds_filename)
          TRUE # Return TRUE on success
        }, error = function(e) {
          warning(paste("Failed to save", obj_name, "to", rds_filename, ":", e$message))
          FALSE # Return FALSE on failure
        })
        
        # If save was successful, add filename to list
        if (save_success) {
          saved_files <- c(saved_files, rds_filename)
        }
      }
    }
  }
  
  # Final summary message
  saved_count <- length(saved_files)
  if (saved_count > 0) {
    print(paste("--- Finished saving. Total files saved:", saved_count, "---"))
  } else {
    print("--- No sf objects ending with '_sf' found or saved. ---")
  }
  
  # Return the list of saved files invisibly
  invisible(saved_files)
}

# --- Example Usage ---
# To run the function, simply call it after your main script has populated the environment:
save_sf_objects_to_rds()

# If you want to see the list of files it saved:
# saved_file_list <- save_sf_objects_to_rds()
# print(saved_file_list)