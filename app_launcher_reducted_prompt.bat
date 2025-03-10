@echo off
CD /D "C:/Users/LPO-PH/OneDrive - LPO/Programmation/Bird Quizz/Bird Quizz/"
start /min "" "C:/Program Files/R/R-4.4.2/bin/R.exe" --vanilla -e "library(shiny); runApp('ext_app_test10.R')"