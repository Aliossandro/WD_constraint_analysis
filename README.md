# Wikidata constraint analysis
This repository contains the scripts and data produced to analyse the influence of property constraints on output and community behaviour in Wikidata.
An account of this examination can be found in the paper "The costs of freedom: on the role of property constraints in collaborative knowledge graphs", submitted at K-CAP 2017 and currently under review.

All the data has been extracted from the Wikidata historical dumps updated to February 3, 2016.

The *script* folder contains four files:
* *controversy_scores.py* generates monthly controversy scores for each Wikidata property;
* *property_uses_extractor.py* extracts the property used in each statement;
* *violation_extraction.R* extracts the number of monthly violations per constraint;
* *property_text_cleaner.R* extracts constraints from property talk pages and includes code to perform the regression analyses described in the paper.

All the datasets referred in the scripts are contained in the *data* folder.
