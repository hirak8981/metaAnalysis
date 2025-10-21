library(fs)
library(here)

# check if the directory exists -------------------------------------------

dir_exists('D:/meta_analysis_R/metaAnalysis')


# show the directory structure --------------------------------------------

dir_tree('D:/meta_analysis_R/metaAnalysis/R')
dir_tree('D:/meta_analysis_R/metaAnalysis/www')
dir_tree('D:/meta_analysis_R/metaAnalysis/www')


# css and js files directory ----------------------------------------------

dir_create('www')

# css
dir_create('www/css')
file_create('./www/css/loading-indicator.css')
file_create('./www/css/banter-loader.css')
file_create('./www/css/landing-page.css')
# javascript
dir_create('www/js')
file_create('./www/js/loading-indicator.js')
file_create('./www/js/banter-loader.js')
file_create('./www/js/carousel.js')
# modules -----------------------------------------------------------------

# create the necessary files ----------------------------------------------
file_create('global.R')
file_create('ui.R')
file_create('server.R')
file_create('app.R')


# landing page files ------------------------------------------------------
file_create('./R/landing/mod_landing_page.R')


# ROB files ---------------------------------------------------------------

file_create('./R/rob/mod_rob_main.R ')
