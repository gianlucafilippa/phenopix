extractDateFilename <- function(filename, date.code){
	code <- stringr::str_replace_all(date.code,"[[:alpha:]]","\\\\d")
  	file <- basename(filename)
  	filename.cleaned <- stringr::str_extract_all(file, pattern = code)[[1]]
 	separated <- str_split(filename.cleaned, '')[[1]]	
 	year.char <- ifelse(length(gregexpr('y', date.code)[[1]]) == 2, '%y', '%Y')
 	year.pos <- unlist(gregexpr('y', date.code)[[1]])
 	year.val <- paste(separated[year.pos], collapse='')
 	month.pos <- unlist(gregexpr('m', date.code)[[1]])
 	month.val <- paste(separated[month.pos], collapse='')
 	day.pos <- unlist(gregexpr('d', date.code)[[1]])
 	day.val <- paste(separated[day.pos], collapse='')
 	hour.pos <- unlist(gregexpr('H', date.code)[[1]])
 	hour.val <- ifelse(hour.pos[1] > 0, paste(separated[hour.pos], collapse=''), '12')
 	min.pos <- unlist(gregexpr('M', date.code)[[1]])
 	min.val <- ifelse(min.pos[1] > 0, paste(separated[min.pos], collapse=''), '00')
	sec.char <- ifelse(length(gregexpr('S', date.code)[[1]]) == 2, ':%S', NA)
  	sec.pos <- unlist(gregexpr('S', date.code)[[1]])
  	sec.val <- ifelse(sec.pos[1] > 0, paste(separated[sec.pos], collapse=''), '00')
 	final.date <- paste(year.val, month.val, day.val, sep='-')
 	final.time <- paste(hour.val, min.val, sec.val,sep=':')
 	final.datetime <- paste(final.date, final.time)
 	final.format <- ifelse(!is.na(sec.char),paste0(year.char,'-%m-%d %H:%M',sec.char),paste0(year.char,'-%m-%d %H:%M'))
 	date <- as.POSIXct(strptime(final.datetime, format=final.format))
 #    string_length <- nchar(filename.cleaned)
	# string_split <-suppressWarnings(as.numeric(str_split_fixed(filename.cleaned,"", string_length+1)))
	# sub_string <- string_split[which(is.na(string_split)==FALSE)]
 #    yyyy <- str_c(as.character(sub_string[1]),as.character(sub_string[2]),as.character(sub_string[3]),as.character(sub_string[4]))
 #    MM <- str_c(as.character(sub_string[5]),as.character(sub_string[6]))
 #    DD <- str_c(as.character(sub_string[7]),as.character(sub_string[8]))  
	# yyyyMMDD<-paste(yyyy,"-",MM,"-",DD,sep="")  
	# if (is.na(as.character(sub_string[9])))  HH <- '00' else {
	# HH<- str_c(as.character(sub_string[9]),as.character(sub_string[10]))
	# 	}
	# if (is.na(as.character(sub_string[11])))  mm <- '00' else {
	# 	mm<- str_c(as.character(sub_string[11]),as.character(sub_string[12]))	
	# 	}
 #    date<-as.POSIXct(strptime(paste(yyyyMMDD,' ',HH,':',mm,sep=''),"%Y-%m-%d %H:%M"),'GMT')
 	if (is.na(date)) stop(paste('Date extraction from',filename,'failed'))
	return(date)	
}
