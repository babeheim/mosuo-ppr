Analysis script for "Offspring sex preferences among patrilineal and matrilineal Mosuo in Southwest China revealed by differences in parity progression" by
Siobhán M. Mattison, Bret Beheim, Bridget Chak, Peter Buston
R. Soc. open sci. 2016 3 160526; DOI: 10.1098/rsos.160526. Published 14 September 2016

Requirements:
- R (3.3.1 or greater) https://cran.r-project.org/
- rethinking package (v1.59 or greater), http://xcelab.net/rm/software/

Instructions:

In R, set the working directory to that containing this readme file. For example, on a Mac or Linux machine, you might say

    setwd('~/Desktop/mosuo-ppr')

if the folder containing the project is named 'mosuo-ppr' and on your Desktop. You can tell if you are in the right place by typing in

    dir()

and seeing the folders 'code' and 'inputs' and this readme.txt file. The analysis takes as input two files:

'final_regression_data.csv' - this file contains information about all our women and their reproductive histories. It tells us of a woman's decade of birth, her current educational status, whether she has an MI job or not, and the age at which she had each of her children. It also says whether she lives in a matrilineal or patrilineal area, but not which village or anything about the villages. The women are indexed by my random alphanumeric 'uid' which contains no information about household or sampling order.

'Mosuo_pop_reg.csv' - this is a pedigree file, so it contains both men and women, indexed by random alphanumeric code 'pid'. It records whether they are a man or a woman and their exact year of birth and year of death. If we know who their father or mother was, that person will also have a random alphanumeric and the connection between the two codes is given in the 'f.pid' or 'm.pid' column. There is also a code to indicate the region they live: mat.tourist, mat.traditional, or patrilineal. This is used as the starting point for the ppr simulation - we grow the population forward 100 years and observe the consequences of living under the PPR regime estimated in our models, and then empirically calculate the honeycomb numbers.

The analysis itself is broken up into independent modules that pass outputs to each other. The whole process runs by typing one command into R,

    source('./code/runproject.r')

with the project folder as the working directory. If all goes well, each step of the analysis will execute in sequence, and write the final tables and figures into an 'output' folder, along with a runtime log.

By default the analysis will delete all temporary files and folders, but if you want to see all intermediate steps you can disable this by flipping the 'save_temp' variable in 'project_variables.r' from FALSE to TRUE.

The total time until completion will vary by machine; the slowest we have seen has been about 2 hours.

The project is maintained by Bret Beheim (beheim@gmail.com) and is hosted at https://github.com/babeheim/mosuo-ppr.