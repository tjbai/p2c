# clean and compile
make clean
cd ../..
dune clean 
dune build
cd src/testPythonFiles

# remove c and h files
rm *.c
rm *.h

# Copy ./convert.exe from proj folder to current folder
cp ../../convert.exe ./

# Run the program 
./convert.exe ./sample1.py ./sample2.py

# make file compile
make

# run c file
./run