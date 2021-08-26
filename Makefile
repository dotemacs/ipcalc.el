.PHONY: test
test: ipcalc.el ipcalc-tests.el
	emacs -batch -l subr-x -l ert -l ipcalc-tests.el -f ert-run-tests-batch-and-exit
