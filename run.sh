

################## UNCOMMENT BELOW FOR TESTING ###############################

# Cleans the project


# # # Builds
# cd src
# rm -f *.coverage

# dune clean
# # dune test
# dune build


# # test files
# files="./testPythonFiles/sample1.py ./testPythonFiles/sample2.py "

# # Runs the tests
# location=$(../_build/default/src/convert.exe $files)
# echo $location

# cd ..

################## UNCOMMENT BELOW FOR COVERAGE AND PACKAGING ##################
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