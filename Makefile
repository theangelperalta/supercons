# Makefile for supercons
#
# Requires SBCL. Dependencies are installed via Quicklisp, which is
# bootstrapped automatically into QUICKLISP_HOME if not already present.

LISP          ?= sbcl
QUICKLISP_HOME ?= $(HOME)/quicklisp
QUICKLISP_SETUP = $(QUICKLISP_HOME)/setup.lisp

# Run SBCL non-interactively with Quicklisp loaded.
SBCL = $(LISP) --non-interactive --load $(QUICKLISP_SETUP)

.PHONY: all deps build test clean

all: build

# Bootstrap Quicklisp if it isn't installed yet.
$(QUICKLISP_SETUP):
	@echo "Bootstrapping Quicklisp into $(QUICKLISP_HOME)..."
	curl -o /tmp/quicklisp.lisp https://beta.quicklisp.org/quicklisp.lisp
	$(LISP) --non-interactive \
	  --load /tmp/quicklisp.lisp \
	  --eval '(quicklisp-quickstart:install :path "$(QUICKLISP_HOME)/")'
	@rm -f /tmp/quicklisp.lisp

# Install the system's dependencies via Quicklisp.
deps: $(QUICKLISP_SETUP)
	$(SBCL) \
	  --eval '(ql:quickload :supercons)' \
	  --eval '(ql:quickload :supercons/tests)'

build: $(QUICKLISP_SETUP)
	$(SBCL) \
	  --eval '(ql:quickload :supercons)'

test: $(QUICKLISP_SETUP)
	$(SBCL) \
	  --eval '(ql:quickload :supercons/tests)' \
	  --eval '(uiop:quit (if (rove:run-suite :supercons/tests) 0 1))'

clean:
	rm -rf $(HOME)/.cache/common-lisp
