library(jsonlite)
library(dplyr)
hawthornFly = GET('https://www.marinespecies.org/rest/AphiaRecordsByName/Bibio%20marci?like=true&marine_only=false&extant_only=true&offset=1')
str(hawthornFly$content)
my_content = httr::content(hawthornFly, as = 'text')
str(my_content)

my_content_from_json = jsonlite::fromJSON(my_content)
dplyr::glimpse(my_content_from_json)
dplyr::glimpse(my_content_from_json$order[1])

######################
# Cerastoderma edule #
######################
cockle = GET('https://www.marinespecies.org/rest/AphiaRecordsByName/Cerastoderma%20edule?like=true&marine_only=false&extant_only=true&offset=1')
str(cockle$content)
cockle_content = httr::content(cockle, as = 'text')
str(cockle_content)
cockle_content_from_json = jsonlite::fromJSON(cockle_content)
dplyr::glimpse(cockle_content_from_json)
dplyr::glimpse(cockle_content_from_json$order[1])
