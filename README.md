# Cook County Criminal Justice
Understanding trends and lags in the Cook County Criminal Justice system using SAO open source data and exploring impact of bail reform act using data collected by ChicagoDataCollaborative

Links to data:

SAO data: https://datacatalog.cookcountyil.gov/browse?tags=state%27s%20attorney%20case-level&sortBy=most_accessed

Chicago Data Collaborative: https://chicagodatacollaborative.org/

Link to analysis: https://dsharm.github.io/CookCountyCriminalJustice/

# Details of SAO Data
1) Initiation: This data represents all the arrests that come through the SAO’s office. After an arrest is made, there are a number of ways it can turn into a “case” in court. This usually happens through a process called Felony Review Process, but not always. Each row is a charge associated with a case participant (not person, because a person could be in the data multiple times for different cases, and each person can have multiple charges)

2)	Intake: This data represents all the cases brought in to the Felony Review process. Each row is a case participant

3)	Disposition: Represents result of fact-finding process that leads to the resolution of a case. Each row is a charge that has been disposed of (a participant can have multiple charges)

4)	Sentencing: Reflects the judgment imposed by the court on people that have been found guilty. Each row is a charge that has been sentenced


# Scripts
Contains code used to explore the datasets which produce the graphs seen in "data_output" folder
