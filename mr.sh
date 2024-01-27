# with RSS
#g++ -ggdb -flto -Ofast -fopenmp -fno-builtin -D NTVCM_RSS_SUPPORT -D NDEBUG -I . ntvcm.cxx x80.cxx -lssl -lcrypto -o ntvcm -static

# without RSS
g++ -ggdb -flto -Ofast -fno-builtin -D NDEBUG -I . ntvcm.cxx x80.cxx -o ntvcm -static
