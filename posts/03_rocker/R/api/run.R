r = plumber::plumb("plumber.R")
r$run(host="0.0.0.0", port=8000, swagger=TRUE)