clear all
*********************
*Necessary packages**
*********************

*ssc install reghdfe, all replace
*ssc install ftools, all replace
*ssc install estout, all replace
*ssc install rsource, all replace
*ssc install ppmlhdfe, all replace


*********************
********Paths********
*********************

global rpath "/usr/local/bin/R"
global path "~/Dropbox/I4R/AI paper"
cd "$path"

log using "output/master_log_stata.log", replace text


*********************
**Cleaning raw data**
*********************

do "code/Stata code/cleaning.do"

*********************
*****Proccesing******
*********************

******Tables
do "code/Stata code/main.do"

do "code/Stata code/logit poisson.do"

do "code/Stata code/full controls.do"

do "code/Stata code/softwares.do"

do "code/Stata code/study 2.do"

**Branch differencess
rsource, terminator(END_OF_R) rpath("$rpath") ro(--vanilla)
setwd("~/Dropbox/I4R/AI paper/R code")
library(haven);
df <- data.frame(read_dta("data/AI games.dta"));
source("code/R code/branches.R");
q();
END_OF_R


**Balance tables
rsource, terminator(END_OF_R) rpath("$rpath") ro(--vanilla)
library(haven);
df <- data.frame(read_dta("data/AI games.dta"));
source("code/R code/balance.R");
q();
END_OF_R


**ChatGPT tables
rsource, terminator(END_OF_R) rpath("$rpath") ro(--vanilla)
library(haven);
df <- data.frame(read_dta("data/AI games.dta"));
source("code/R code/gpt skill.R");
q();
END_OF_R


**Prompts usage tables
rsource, terminator(END_OF_R) rpath("$rpath") ro(--vanilla)
library(haven);
df <- data.frame(read_dta("data/AI games.dta"));
source("code/R code/prompts.R");
q();
END_OF_R


******Figures
do "code/Stata code/time to first.do"

do "code/Stata code/reproduction rates.do"


log close
