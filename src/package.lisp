;;;; Package definitions for supercons.
;;;;
;;;; A Common Lisp port of the `superconsole` Rust crate (Meta Platforms, Inc.).
;;;; Dual-licensed under MIT / Apache-2.0, matching the upstream project.

(defpackage #:supercons
  (:use #:cl)
  (:export
   ;; conditions / errors
   #:superconsole-error
   #:output-error
   #:output-error-write
   #:output-error-spawn-thread
   #:output-error-terminal
   #:output-error-cause
   #:draw-error
   #:draw-error-message
   #:draw-error-cause
   #:span-error
   #:span-error-invalid-whitespace
   #:span-error-word

   ;; style: colors
   #:color
   #:make-rgb-color
   #:make-ansi-color
   #:rgb-color-r #:rgb-color-g #:rgb-color-b
   #:ansi-color-value
   #:+reset+ #:+black+ #:+dark-grey+ #:+red+ #:+dark-red+
   #:+green+ #:+dark-green+ #:+yellow+ #:+dark-yellow+
   #:+blue+ #:+dark-blue+ #:+magenta+ #:+dark-magenta+
   #:+cyan+ #:+dark-cyan+ #:+white+ #:+grey+

   ;; style: content-style / styled-content
   #:content-style
   #:make-content-style
   #:content-style-foreground-color
   #:content-style-background-color
   #:content-style-attributes
   #:content-style=
   #:styled-content
   #:make-styled-content
   #:styled-content-style
   #:styled-content-content

   ;; style: stylize helpers
   #:stylize
   #:with-style
   #:red #:dark-red #:green #:dark-green #:yellow #:dark-yellow
   #:blue #:dark-blue #:magenta #:dark-magenta #:cyan #:dark-cyan
   #:white #:grey #:black #:dark-grey
   #:on-red #:on-dark-red #:on-green #:on-dark-green #:on-yellow
   #:on-dark-yellow #:on-blue #:on-dark-blue #:on-magenta
   #:on-dark-magenta #:on-cyan #:on-dark-cyan #:on-white #:on-grey
   #:on-black #:on-dark-grey
   #:bold #:dim #:italic #:underlined #:reverse-video #:crossed-out

   ;; unicode width
   #:char-column-width
   #:string-column-width

   ;; dimensions
   #:dimensions
   #:make-dimensions
   #:dimensions-width
   #:dimensions-height
   #:dimensions=
   #:dimensions-multiply
   #:dimensions-saturating-sub
   #:dimensions-dimension
   #:dimensions-intersect
   #:dimensions-union
   #:dimensions-contains-p
   #:dimension-from-output-truncated
   #:direction
   #:+direction-horizontal+
   #:+direction-vertical+

   ;; content: span
   #:span
   #:span-dash
   #:span-valid-p
   #:span-sanitized
   #:span-content
   #:span-style
   #:span-hyperlink
   #:span-padding
   #:span-mergeable-with-p
   #:make-span-unstyled
   #:make-span-unstyled-lossy
   #:make-span-styled
   #:make-span-styled-lossy
   #:make-span-colored
   #:make-span-colored-lossy
   #:span-with-hyperlink
   #:span-len
   #:span-empty-p
   #:span-graphemes
   #:span=
   #:span-fmt-for-test
   #:make-hyperlink
   #:hyperlink-uri

   ;; content: line
   #:line
   #:make-line
   #:line-spans
   #:line-push
   #:line-push-front
   #:line-len
   #:line-empty-p
   #:line-pad-left #:line-pad-right
   #:line-truncate-line
   #:line-trim-ends
   #:line-to-exact-width
   #:line-render
   #:line=
   #:line-fmt-for-test
   #:line-from-spans
   #:line-from-strings
   #:line-unstyled
   #:line-sanitized
   #:line-to-unstyled

   ;; content: lines
   #:lines
   #:make-lines
   #:lines-vec
   #:lines-len
   #:lines-empty-p
   #:lines-push
   #:lines-extend
   #:lines-truncate-lines
   #:lines-max-line-length
   #:lines-pad-lines-right
   #:lines-pad-lines-left
   #:lines-justify
   #:lines-set-lines-to-exact-width
   #:lines-pad-lines-bottom
   #:lines-pad-lines-top
   #:lines-truncate-lines-bottom
   #:lines-set-lines-to-exact-length
   #:lines-shrink-lines-to-dimensions
   #:lines-dimensions
   #:lines-set-lines-to-exact-dimensions
   #:lines-join-horizontally
   #:lines-from-multiline-string
   #:lines-from-multiline-string-raw
   #:lines-from-colored-multiline-string
   #:lines=
   #:lines-fmt-for-test

   ;; component protocol
   #:component
   #:draw
   #:draw-unchecked
   #:draw-mode
   #:+draw-mode-normal+
   #:+draw-mode-final+
   #:copy-line
   #:copy-lines

   ;; components
   #:blank #:make-blank
   #:echo #:make-echo
   #:bounded #:make-bounded
   #:spinner #:make-spinner #:+braille-spinner+
   #:aligned #:make-aligned
   #:padded #:make-padded
   #:bordered #:make-bordered #:bordered-spec #:make-bordered-spec
   #:split #:make-split
   #:draw-vertical #:make-draw-vertical #:draw-vertical-draw #:draw-vertical-finish
   #:draw-horizontal #:make-draw-horizontal #:draw-horizontal-draw #:draw-horizontal-finish

   ;; output
   #:output-target
   #:+output-target-main+
   #:+output-target-aux+
   #:superconsole-output
   #:should-render
   #:output
   #:output-to
   #:aux-stream-is-tty
   #:terminal-size
   #:finalize
   #:blocking-superconsole-output
   #:make-blocking-superconsole-output
   #:non-blocking-superconsole-output
   #:make-non-blocking-superconsole-output
   #:stream-is-tty-p
   #:query-terminal-size

   ;; superconsole engine + builder
   #:superconsole
   #:make-superconsole
   #:make-superconsole-forced
   #:make-superconsole-with-output
   #:superconsole-compatible-p
   #:superconsole-render
   #:superconsole-render-with-mode
   #:superconsole-render-general
   #:superconsole-finalize
   #:superconsole-emit
   #:superconsole-emit-aux
   #:superconsole-emit-now
   #:superconsole-clear
   #:superconsole-output
   #:superconsole-to-emit
   #:superconsole-aux-to-emit
   #:superconsole-canvas-contents
   #:builder
   #:make-builder
   #:builder-non-blocking
   #:builder-write-to
   #:builder-build
   #:builder-build-forced

   ;; async stdin
   #:stdin-reader
   #:make-stdin-reader
   #:stdin-read-chunk
   #:stdin-eof-p

   ;; testing
   #:test-output
   #:test-output-frames
   #:test-output-should-render
   #:test-output-terminal-size
   #:test-output-aux-stream-is-tty
   #:aux-output-with-prefix
   #:test-console
   #:test-console-aux-incompatible
   #:frame-contains-p
   #:assert-frame-contains))
