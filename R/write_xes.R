#' @title Write XES file
#' @description Function for writing xes-file
#' @param eventlog An event log object
#' @param case_attributes Data object containing case attributes
#' @param file Destination file
#'
#' @export write_xes

write_xes <- function(eventlog,
					  case_attributes = NULL,
					  file = file.choose()) {
	e <- eventlog
	colnames(e)[colnames(e) == case_id(eventlog)] <- "case_classifier"

	if(is.null(case_attributes)){
		case_attributes <- data.frame(unique(e$case_classifier))
		colnames(case_attributes)[1] <- case_id(eventlog)
	}

	case_attributes %>%
		rename_("concept:name" = case_id(eventlog)) -> case_attributes


	eventlog %>% rename_("concept:name" = case_id(eventlog),
						"lifecycle:transition" = lifecycle_id(eventlog),
						"concept:instance" = activity_id(eventlog),
						"time:timestamp" = timestamp(eventlog)) -> eventlog

	case_id <- "concept:name"

	print(head(case_attributes))
	print(head(eventlog))
	print(case_id)
	createXES(file , traces = case_attributes , events = as.data.frame(eventlog), caseid_field = case_id)
}
