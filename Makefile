install:
	cd test_runners && npm install
watch : install
	cd test_runners && npm run-script watch
watch-test : install
	cd test_runners && npm run-script livetest
compile : install
	cd test_runners && npm run-script compile
test : compile
	cd test_runners && npm test
