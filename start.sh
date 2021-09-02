#!/usr/bin/bash
R -f daemon.R &
R -e "shiny::runApp('viewer', host='0.0.0.0', port=3838)"