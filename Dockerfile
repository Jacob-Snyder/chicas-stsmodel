FROM rocker/geospatial:4.0.0

RUN  install2.r --error -r http://cran.rstudio.com/ \
        openxlsx \
	pacman \
	dplyr \
 	sf \
	surveillance \
	plotly \
	timeDate \
	tmap \
        readxl \
	ggplot2 && installGithub.r jbracher/hhh4addon@f41ba522ee073ac73dc56d361ff97ddba7e7583a

WORKDIR /app

ADD ./docker/ /app

CMD ["/bin/bash"]


