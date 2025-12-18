# with RSS
#g++ -ggdb -flto -O3 -fopenmp -fno-builtin -D NTVCM_RSS_SUPPORT -D NDEBUG -I . ntvcm.cxx x80.cxx -lssl -lcrypto -o ntvcm -static

# without RSS
g++ -ggdb -flto -O3 -fno-builtin -D NDEBUG -I . ntvcm.cxx x80.cxx -o ntvcm
