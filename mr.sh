COMMIT=$(git log -1 HEAD --format=%h 2> /dev/null)
BUILD=$(printf "%04d" "$(git rev-list --count HEAD 2> /dev/null)" )
#echo $COMMIT $BUILD
if [[ -z ${COMMIT} ]]; then
  # with RSS
  #g++ -ggdb -flto -Ofast -fopenmp -fno-builtin -D NTVCM_RSS_SUPPORT -D NDEBUG -I . ntvcm.cxx x80.cxx -lssl -lcrypto -o ntvcm -static
  # without RSS
  g++ -ggdb -flto -Ofast -fno-builtin -D NDEBUG -I . ntvcm.cxx x80.cxx -o ntvcm -static
else
  # with RSS
  #g++ -ggdb -flto -Ofast -fopenmp -fno-builtin -D NTVCM_RSS_SUPPORT -D COMMIT_ID="\" [Commit Id:$COMMIT]\"" -D DEBUG -I . ntvcm.cxx x80.cxx -lssl -lcrypto -o ntvcm  -static
  # without RSS
  g++ -ggdb-flto -Ofast -fopenmp -fno-builtin -D BUILD="\".$BUILD\"" -D COMMIT_ID="\" [Commit Id:$COMMIT]\"" -D DEBUG -I . ntvcm.cxx x80.cxx -o ntvcm -static
fi


