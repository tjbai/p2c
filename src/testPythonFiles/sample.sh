# clean and compile
make clean

# remove c and h files
rm -f *.c
rm -f *.h
rm -f *.exe

cd ../..
chmod 777 .
dune clean 
dune build


# Copy ./convert.exe from proj folder to current folder

cp ./convert.exe ./src/testPythonFiles


cd src/testPythonFiles

# Run the program 
./convert.exe ./sample1.py ./sample2.py

# # make file compile
make

# # run c file
./run