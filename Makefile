install:
	cd test_runners && npm install
local-watch:
	cd test_runners && ~/bin/chloric -u chloric-profile.clj -t 10000 -v -w '../core-cl2/src/cl2/,../core-cl2/test/cl2/' ../core-cl2/test/test_runners.cl2
watch : install
	cd test_runners && npm run-script watch
watch-test : install
	cd test_runners && npm run-script livetest
compile : install
	cd test_runners && npm run-script compile
ci-test : compile
	cd test_runners && npm run-script ci-test
test : compile
	cd test_runners && npm run-script test
