site:
	@echo Generating website ...
	sbcl --script site.lisp
	@echo Done; echo

dist:
	@echo Generating distributable website ...
	sbcl --eval '(defvar *params* (list (cons "index" "index.html") (cons "main" "https://susam.in/")))' --script site.lisp
	@echo Done; echo

loop:
	while true; do make dist; sleep 5; done

live: site
	@echo Setting up live directory ...
	mv _live _gone || :
	mv _site _live
	rm -rf _gone
	@echo Done; echo

FORCE:

pub: push web gh

push:
	git push

web:
	ssh -t susam.in "cd /opt/maze/ && sudo git pull && sudo make live"

gh: site
	rm -rf /tmp/live
	mv _site /tmp/live
	REPO_DIR="$$PWD"; cd /tmp/live && make -f "$$REPO_DIR/Makefile" ghlive

ghlive:
	pwd | grep live$ || false
	git init
	git config user.name susam
	git config user.email susam@susam.in
	git checkout -b live
	git add .
	git commit -m "Publish live ($$(date -u +"%Y-%m-%d %H:%M:%S"))"
	git log
	git remote add origin https://github.com/susam/maze.git
	git push -f origin live

# Checks
test:
	sbcl --noinform --eval "(defvar *quit* t)" --script test.lisp

checks:
	# Ensure punctuation goes inside inline-math.
	! grep -IErn '\\)[^ ]' content | grep -vE '\\)(th|-|</h[1-6]>|\)|:)'
	! grep -IErn '(th|-|</h[1-6]>|:) \\)' content
	@echo Done; echo
