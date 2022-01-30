library(archive)
tf <- tempfile() ; td <- tempdir()
file.path <- "https://ec.europa.eu/eurostat/estat-navtree-portlet-prod/BulkDownloadListing?sort=1&downfile=comext%2FCOMEXT_DATA%2FPRODUCTS%2Ffull200101.7z"
download.file( file.path , tf , mode = "wb" )
archive_extract(tf)
