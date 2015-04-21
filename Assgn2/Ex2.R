data79 = read.table("light1879.txt", header=F, sep=" ")
data82 = read.table("light1882.txt", header=F, sep=" ")

data79 = gsub(",", "", data79)
data79 = as.numeric(data79)
hist(data79)

data82 = gsub(",", "", data82)
data82 = as.numeric(data82)
hist(data82)
