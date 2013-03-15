watch :
	chloric -u test_runners/chloric-profile.clj -t 10000 -v \
	-w 'core-cl2/src/cl2/,core-cl2/test/cl2/' \
	core-cl2/test/test_runners.cl2
watch-test :
	cd test_runners && npm install && npm run-script livetest

compile :
	chloric -1 -u test_runners/chloric-profile.clj -v \
	core-cl2/test/test_runners.cl2
test : compile
	cd test_runners && npm install && npm test
