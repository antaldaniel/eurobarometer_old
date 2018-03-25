# eurobarometer 0.0.0.9001 - 0.0.0.9004

* Added a `NEWS.md` file to track changes to the package.
* The package is detached from the more general surveyreader, and does not handle multi-language surveys. Currently it is aimed to correctly handle the Eurobarometer archives of GESIS.
* 0.0.0.9001 Handles same naming exceptions, and avoids a faulty error message that caused R to crash.
* 0.0.0.9002 Class and methods for handling 1 positive, 1 neutral, 1 negative levels. Adding unit tests. 
* 0.0.0.9003 Class and methods for handling 4 non-negative categories.
* 0.0.0.9004 Correctly convert haven labelled variables.
