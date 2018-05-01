# eurobarometer 0.0.0.9001 - 0.0.0.9009

* Added a `NEWS.md` file to track changes to the package.
* The package is detached from the more general surveyreader, and does not handle multi-language surveys. Currently it is aimed to correctly handle the Eurobarometer archives of GESIS.
* 0.0.0.9001 Handles same naming exceptions, and avoids a faulty error message that caused R to crash.
* 0.0.0.9002 Class and methods for handling 1 positive, 1 neutral, 1 negative levels. Adding unit tests. 
* 0.0.0.9003 Class and methods for handling 4 non-negative categories.
* 0.0.0.9004 Correctly convert haven labelled variables.
* 0.0.0.9005 % of variables that can be automatically converted added to summary. Added a few typical answer options.
* 0.0.0.9006 A few new typical questions recognized.
* 0.0.0.9007 Eurobarometer vocabulary now part of a large spreadsheet
* 0.0.0.9008 Much improved type detection and error handling 
* 0.0.0.9009 Improved name suggestion, better conversions. 

# eurobarometer 0.0.1.0001 -
* 0.0.1.0001 Now uses a single vocabulary file. 
* 0.0.1.0004 Adds frequency factors gesis_metadata_get(). Many new exceptions are handled.
* 0.0.1.0005 Very long functions are simplified and separated to smaller units. Further naming exceptions are added. Naming exceptions are matched first exactly than partially.
