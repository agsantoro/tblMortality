# create directories to place outputs

dir.create(file.path("outputs"), showWarnings = FALSE)
dir.create(file.path("outputs/data"), showWarnings = FALSE)
dir.create(file.path("outputs/figures"), showWarnings = FALSE)

# run process

scripts = list.files("scripts")
for (i in scripts) {
  source(paste0("scripts/",i))
}
