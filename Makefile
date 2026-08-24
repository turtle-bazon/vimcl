LISP ?= sbcl

.PHONY: build test clean

build:
	$(LISP) --non-interactive \
	  --eval '(ql:quickload "vimcl")' \
	  --eval '(ensure-directories-exist #p"build/vimcl")' \
	  --eval '(asdf:make "vimcl")'

test:
	@rm -rf ~/.cache/common-lisp/sbcl-*/tmp/vimcl
	$(LISP) --non-interactive \
	  --eval '(ql:quickload "vimcl")' \
	  --eval '(format t "vimcl smoke: ok~%")'

clean:
	rm -rf build
