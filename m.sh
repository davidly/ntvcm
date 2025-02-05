
COMMIT=$(git log -1 HEAD --format=%h 2> /dev/null)
# echo $COMMIT
if [[ -z ${COMMIT} ]]; then
  # with RSS
  #g++ -ggdb -Og -fno-builtin -D NTVCM_RSS_SUPPORT -D DEBUG -I . ntvcm.cxx x80.cxx -lssl -lcrypto -o ntvcm  -static
  # without RSS
  g++ -ggdb -Ofast -fno-builtin -D DEBUG -I . ntvcm.cxx x80.cxx -o ntvcm -static
else
  # with RSS
  #g++ -ggdb -Og -fno-builtin -D NTVCM_RSS_SUPPORT -D COMMIT_ID="\" [Commit Id:$COMMIT]\"" -D DEBUG -I . ntvcm.cxx x80.cxx -lssl -lcrypto -o ntvcm  -static
  # without RSS
  g++ -ggdb -Ofast -fno-builtin -D COMMIT_ID="\" [Commit Id:$COMMIT]\"" -D DEBUG -I . ntvcm.cxx x80.cxx -o ntvcm -static
fi
