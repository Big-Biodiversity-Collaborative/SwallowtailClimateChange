# Create SDM script for each species for each model
# Jeff Oliver
# jcoliver@arizona.edu
# 2021-06-02

# Read the file with organism names in as an array, skipping the header row 
# (hence -n +2 in call to tail)
readarray -t NAMES < <(tail -n +2 data/gbif-reconcile.csv)

SCRIPTTYPE="model"
MODELS=("glm" "svm")
PRODUCT="SDM"

# Iterate over all the types of models (e.g. glm, svm)
for MODEL in "${MODELS[@]}"
do
  MODELUPPER=$(echo $MODEL | tr '[:lower:]' '[:upper:]')
  # Iterate over all lines in that names file
  for LINE in "${NAMES[@]}"
  do
      # Create an array out of that line
      IFS="," read -r -a ONENAME <<< "${LINE}"
  
      # Extract genus & species name
      GENUSSPECIES=$(echo "${ONENAME[3]}" | sed 's/\"//g')
      GENUS=$(echo "$GENUSSPECIES" | cut -d' ' -f 1)
      SPECIES=$(echo "$GENUSSPECIES" | cut -d' ' -f 2)
      
      # For the filename, want genus to be lower case
      GENUSLOWER=$(echo $GENUS | tr '[:upper:]' '[:lower:]')
      FILENAME="src/indiv/${GENUSLOWER}_${SPECIES}-${SCRIPTTYPE}-${MODEL}.R"
      
      # Read in the template file contents into the MODELFILE variable, 
      # skipping very first line
      TEMPLATE="templates/template-${SCRIPTTYPE}-${MODEL}-building.R"
      MODELFILE=$(cat "$TEMPLATE" | tail -n +2)
      
      # Add lines at top of file explaining script & message warning against 
      # editing
      MESSAGE="# This file is auto-generated by build-${SCRIPTTYPE}-${MODEL}-files.sh. Do NOT edit."
      MODELFILE=$(echo -e "# Generate ${PRODUCT} for ${GENUS} ${SPECIES} from ${MODELUPPER}\n${MESSAGE}\n${MODELFILE}")
  
      # Use sed to find/replace GENUS/SPECIES values and write to file
      echo "${MODELFILE}" | \
        sed "s/GENUS/${GENUS}/g" | \
        sed "s/SPECIES/${SPECIES}/g" > "$FILENAME"
      echo "Wrote to ${FILENAME}"
  done; # end iterating over all species names
done; # end iterating over all models