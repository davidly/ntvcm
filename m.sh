# with RSS
g++ -ggdb -Ofast -fopenmp -fno-builtin -D NTVCM_RSS_SUPPORT -D DEBUG -I . ntvcm.cxx x80.cxx -lssl -lcrypto -o ntvcm 

# without RSS
# g++ -ggdb -Ofast -fno-builtin -D DEBUG -I . ntvcm.cxx x80.cxx -o ntvcm 
