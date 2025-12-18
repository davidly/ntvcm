# with RSS
#g++ -ggdb -Og -fno-builtin -D NTVCM_RSS_SUPPORT -D DEBUG -I . ntvcm.cxx x80.cxx -lssl -lcrypto -o ntvcm  -static

# without RSS
g++ -ggdb -O3 -fno-builtin -D DEBUG -I . ntvcm.cxx x80.cxx -o ntvcm
