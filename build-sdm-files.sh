# Create an SDM script for each species
# Jeff Oliver
# jcoliver@arizona.edu
# 2021-06-02

# Read the file in as an array, skipping the header row (hence -n +2)
# readarray -t NAMES < <(tail -n +2 data/gbif-reconcile.csv)
# for testing on two lines
readarray -t NAMES < <(tail -n +2 data/gbif-reconcile.csv | head -n 2)

# Iterate over all lines in that gbif names file
for LINE in "${NAMES[@]}"
do
    # echo "$LINE"
    # Create an array out of that line
    IFS="," read -r -a ONENAME <<< "${LINE}"
    GENUS=$(echo "${ONENAME[0]}" | sed 's/\"//g')
    SPECIES=$(echo "${ONENAME[1]}" | sed 's/\"//g')
    # echo "GENUS: ${GENUS}, SPECIES: ${SPECIES}"
    FILENAME="${GENUS}_${SPECIES}-sdm.R"
    MODELFILE=$(cat template-model-building.R)
    echo "${MODELFILE}" | sed "s/GENUS/${GENUS}/g" | sed "s/SPECIES/${SPECIES}/g" > "$FILENAME"
    echo "Wrote to ${FILENAME}"
done;
