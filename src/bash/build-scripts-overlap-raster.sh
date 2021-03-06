# Create an overlap raster script for each insect species for each model
# Jeff Oliver
# jcoliver@arizona.edu
# 2021-07-08

SCRIPTTYPE="overlap-raster"
MODELS=("glm" "svm")
PRODUCT="overlap raster"

# Read the file with insect names in as an array, skipping the header row 
# (hence -n +2 in call to tail); need to also sort & pull out only unique 
# values as an insect species may be represented multiple times in file
readarray -t NAMES < <(tail -n +2 data/insect-host.csv | cut -d "," -f1 | sort | uniq)

# Iterate over all the types of models (e.g. glm, svm)
for MODEL in "${MODELS[@]}"
do
  MODELUPPER=$(echo $MODEL | tr '[:lower:]' '[:upper:]')
  # Iterate over unique insect species names
  for NAME in "${NAMES[@]}"
  do
      # Split NAME into constituent parts
      NAME=($NAME)  # turns it into an array
      GENUS=$(echo "${NAME[0]}" | sed 's/\"//g')
      SPECIES=$(echo "${NAME[1]}" | sed 's/\"//g')

      # For the filename, want genus to be lower case
      GENUSLOWER=$(echo $GENUS | tr '[:upper:]' '[:lower:]')
      FILENAME="src/indiv/${GENUSLOWER}_${SPECIES}-${SCRIPTTYPE}-${MODEL}.R"
  
      # Read in the template file contents into the MODELFILE variable, 
      # skipping very first line
      TEMPLATE="templates/template-${SCRIPTTYPE}-${MODEL}-building.R"
      MODELFILE=$(cat "$TEMPLATE" | tail -n +2)
  
      # Add lines at top of file explaining script & message warning against 
      # editing
      MESSAGE="# This file is auto-generated by build-${SCRIPTTYPE}-files.sh. Do NOT edit."
      MODELFILE=$(echo -e "# Generate ${PRODUCT} for ${GENUS} ${SPECIES} from ${MODELUPPER}\n${MESSAGE}\n${MODELFILE}")
  
      # Use sed to find/replace GENUS/SPECIES values and write to file
      echo "${MODELFILE}" | \
        sed "s/GENUS/${GENUS}/g" | \
        sed "s/SPECIES/${SPECIES}/g" > "$FILENAME"
      echo "Wrote to ${FILENAME}"
  done; # end iterating over all species names
done; # end iterating over all modelsdone; 
