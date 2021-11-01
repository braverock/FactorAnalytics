# Chicago's own James Lamb made pkgnet. I like the graph visualization
library(pkgnet)
CreatePackageReport('FactorAnalytics')

# output and save as a .Rmd file titled "FA-dependency-pkgnet-report.Rmd"
CreatePackageVignette(vignette_path = file.path("sandbox", "FA-dependency-pkgnet-report.Rmd"))

## pkgndep. Provides a heatmap which is useful
library(pkgndep)
FA_pkgdepends = pkgndep("FactorAnalytics")
plot(FA_pkgdepends)

 # output and save as pdf in the sandbox titled "FA-dependency-heatmap.pdf"
pdf(file = "sandbox/FA-dependency-heatmap.pdf",
    width = as.numeric(size[1]), 
    height = as.numeric(size[2]))
plot(FA_pkgdepends)
dev.off()
