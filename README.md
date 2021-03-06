# ci-mispron

Code to replicate results from:

Cychosz, M., Mahr, T., Munson, B., Newman, R., & Edwards, J. R. (*submitted*). Phonetic detail, not auditory deprivation, shapes how preschoolers with cochlear implants process speech variation. [10.31234/osf.io/cj8qp](https://psyarxiv.com/cj8qp/)


# datasets

* `model.csv.gz` - contains props of looks to target for all children

* `scores.csv` - contains scores for standardized assessments 

* `audio_info.csv` - contains audiological information for children with cochlear implants

* `hearing_age.csv` - contains hearing ages for all participants

* `audiological_info.csv` - generated by `1_audiological_information.R`

# modeling and scripts to replicate results

* `0_match_children.Rmd` - match children with cochlear implants to children with typical hearing by vocabulary size and hearing age

* `1_audiological_information.R` - organize etiological information for children with cochlear implants

* `2_modeling.Rmd` - replicate the results section of the manuscript

# manuscript

`manuscript.pdf`

# extra scripts

* `99_elogit_demo.Rmd` - verify that we don't need to weight observations within the GAMMs

* `create_dark_tables.R` - a function to render darker in-line Kable tables when using a dark RStudio theme
