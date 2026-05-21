# Raw data placeholder

This repository does not include raw cohort data.

Before running the pipeline, extract the supplied data archive into this folder. The
code scans recursively, so exact subfolder names may vary, but the recommended layout is:

```text
data_raw/
  CHARLS/
  HRS/
  KLoSA/
  LASI/
  MHAS/
  SHARE/
  ELSA/
```

The six-country final analysis excludes LASI after interval construction, but the raw
LASI folder is kept here because the full harmonization workflow can still scan all
HRS-family cohorts from the supplied archive.
