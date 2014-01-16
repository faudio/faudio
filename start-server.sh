
# A server to test streaming
#
# ./start-server.sh
# curl http://localhost:2233/test/test.wav
# ./stop-server-sh

(python -m SimpleHTTPServer 2233)>/dev/null 2>&1 & echo "kill $$;" > stop-server.sh &
chmod a+x stop-server.sh
