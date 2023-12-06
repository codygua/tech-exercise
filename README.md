# biostats-sample
Consider a cohort study design with a defined exposure and an outcome of interest (which is not mortality). 

* Study period from July 1st though Dec 31st 2022
* Exposure enters a subject into the exposed cohort and serves as the index date
* Exposed subjects are exact matched with unexposed subjects based on age, gender, and a healthcare encounter within the same week
* Matching is performed without replacement
* Exposed subjects may serve as unexposed matches during any week before the week of their exposure date

The provided tables (in csv format) contain the following data elements:

### Patient table
* Patient ID
* Gender 
* Date of birth
* Date of death | NULL

### Encounter table
* Patient ID
* Encounter date 
* Exposure binary indicator
* Outcome binary indicator

For each of the following analysis steps, provide working code (in a language of your choosing):

1. Load the data tables into dataframe(s) or a database
1. Compute median age at index date for the exposed cohort (including CI at 95% confidence level)
1. Compute aggregate time at risk for the exposed cohort, in person-days

Finally, provide pseudo-code to select a 1:1 unexposed cohort based on the method outlined above.
