Important:
- The data should be loaded from cleaned up copies in data rather than data-raw.  This would allow some of the processing like grabbing only drugs associated with cancer regimens to be propaged more cleanly.
- The sunburst code seems to still use the data lists - not great.
- Move the drug data input/output to /data/clin
- Move R2.1.3 in front of R2.1.2.
- Read over the material for R2.1.3 and beyond to make sure it still makes sense.

Not too important:
- The save_oncokb_annotated_data.R file is not working, something about permissions being denied.  It's easy enough to just drag 3 files in as a workaround, but this would be nice to troubleshoot sometime.
- Add a "folder_setup.R" file to create all the subfolders needed for this project.  It's a mess to have them all over because different files depend on them.

Open questions:
- Does using PRAD or similar have a different result for mutations than using "PROSTATE" on mutations?