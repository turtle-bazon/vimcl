LISP ?= sbcl

.PHONY: build test clean

build:
	@rm -rf ~/.cache/common-lisp/sbcl-*/tmp/vimcl
	$(LISP) --non-interactive \
	  --eval '(ql:quickload "vimcl")' \
	  --eval '(ensure-directories-exist #p"build/vimcl")' \
	  --eval '(asdf:make "vimcl")'

test:
	@rm -rf ~/.cache/common-lisp/sbcl-*/tmp/vimcl
	$(LISP) --non-interactive \
	  --eval '(ql:quickload "vimcl-tests")' \
	  --eval '(unless (fiveam:run! :vimcl) (uiop:quit 1))'

clean:
	rm -rf build
