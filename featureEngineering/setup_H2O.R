### Setup H2O packages - http://h2o-release.s3.amazonaws.com/h2o/rel-wolpert/4/docs-website/h2o-r/docs/articles/getting_started.html

#Remove any previously installed packages for R.
if ("package:h2o" %in% search()) { detach("package:h2o", unload=TRUE) }
if ("h2o" %in% rownames(installed.packages())) { remove.packages("h2o") }

pkgs <- c("RCurl","jsonlite")
for (pkg in pkgs) {
  if (! (pkg %in% rownames(installed.packages()))) { install.packages(pkg) }
}


#Download and install the latest H2O package for R.
install.packages("h2o", type="source", repos=(c("http://h2o-release.s3.amazonaws.com/h2o/latest_stable_R")))

#Initialize H2O and run a demo to see H2O at work.
library(h2o)
h2o.init()
demo(h2o.kmeans)