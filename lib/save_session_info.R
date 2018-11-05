si <- capture.output(sessionInfo())

other_attached <- which(si == "other attached packages:")

writeLines(si[c(other_attached:length(si))], "session_info.txt")
