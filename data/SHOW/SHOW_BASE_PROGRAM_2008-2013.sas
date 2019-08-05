*********************************************************************************
*** SHOW base program that reads in formats									  ***
*** The formats file (formatset) and the dataset (filename)      			  ***
*** must be located in the folder specified in the libname (saslib) statment  ***
*********************************************************************************

* Define the libref for the analytic data set; 
LIBNAME SASLIB 'T:\CORE\CORE_Main\Analytical\Data requests\2017\314_Gangnon_classroom'; 	*Change 'T:\...' to location on your computer where you keep the unzipped files;

* This line reads in the show formats and convert it to a format catalog; 
* Use this code if you are working without access to the SHOW network; 
PROC FORMAT CNTLIN=SASLIB.FORMATSET; RUN;		

* Read in the permanent analytic data set from the library defined above and create a temporary SAS data set;
DATA WORK.SHOW_DATA; SET SASLIB.SHOW_PHS; RUN;

* Explore the contents of the analytic data set; 
PROC CONTENTS DATA = WORK.SHOW_DATA;
RUN;

