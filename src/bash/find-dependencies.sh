# Identify dependencies through search for "require"
# Jeff Oliver
# jcoliver@arizona.edu
# 2023-01-18

# Search each directory where scripts are living (not indiv, though)
TEMPLATES=$(grep -r "require(" templates/)
DATA=$(grep -r "require(" src/data/)
WRAPPERS=$(grep -r "require(" src/run-indiv/)
SUMMARY=$(grep -r "require(" src/summary/)

# Concatenate all these strings together
ALL=$TEMPLATES$DATA$WRAPPERS$SUMMARY$FUNREQUIRE

# Just extract library names
LEFT=$(sed 's/.*(//g' <<< "$ALL")
BOTH=$(sed 's/).*//g' <<< "$LEFT")

# Things in functions are different (only sometimes pass raw names to require)
# Pull out double colon references, e.g. dplyr::select
FUNCTIONS=$(grep -roP '[[:alpha:]\d]*(?=::)' functions/)
FUNLEFT=$(sed 's/.*://g' <<< "$FUNCTION")

# Sometimes explicit libraries are mentioned in conditionals
FUNREQ=$(grep -r "require(" functions/) # won't pick up all, see below
FUNREQLEFT=$(sed 's/.*(//g' <<< "$FUNREQ")
FUNREQBOTH=$(sed 's/).*/\n/g' <<< "$FUNREQLEFT")

# Now add in those functions libraries to the larger string
ALL=$BOTH$'\n'$FUNLEFT$'\n'$FUNREQBOTH

# Sort and pull out uniques
LIBS=$(echo $ALL | xargs -n1 | sort | uniq | xargs)
# Write to a file (a one-column csv sans header)
echo "$LIBS" > src/dependencies.csv
