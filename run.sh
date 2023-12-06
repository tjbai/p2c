# Cleans the project
rm -f *.coverage
dune clean

# Runs Coverage
find . -name '*.coverage' | xargs rm -f
dune runtest --instrument-with bisect_ppx --force

# Displays the report
bisect-ppx-report html
firefox ./_coverage/index.html

# builds the project
dune build