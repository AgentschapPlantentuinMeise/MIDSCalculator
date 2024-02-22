FROM rocker/r-ver:4.2

EXPOSE 3000

# install R packages we need
RUN install2.r --error \
    shiny \
    shinyBS \
    ggplot2 \
    DT \
    shinyjs \
    sortable \
    shinybusy \
    generics \
    tidyselect \
    data.table \
    purrr \
    dplyr


# just copy the whole lot
COPY . /usr/local/src/MIDSCalculator

WORKDIR /usr/local/src/MIDSCalculator

CMD ["Rscript", "dockerApp.R"]
