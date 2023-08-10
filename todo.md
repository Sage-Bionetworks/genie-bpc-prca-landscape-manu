

Important:
- The data should be loaded from cleaned up copies in data rather than data-raw.  This would allow some of the processing like grabbing only drugs associated with cancer regimens to be propaged more cleanly.
- The sunburst code seems to still use the data lists - not great.
- Move the drug data input/output to /data/clin

Not too important:
- The save_oncokb_annotated_data.R file is not working, something about permissions being denied.  It's easy enough to just drag 3 files in as a workaround, but this would be nice to troubleshoot sometime.