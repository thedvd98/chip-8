;; Development environment for the CHIP-8 emulator on Guix System.
;;
;;   guix shell -m manifest.scm          # enter the environment
;;   make eggs                           # first time only: install the eggs
;;   make test / make run
;;
;; Guix does not package the CHICKEN eggs this project needs (sdl2, bitstring,
;; srfi-151, ...), so they are installed with chicken-install into the
;; project-local repository .chicken/eggs (see scripts/env.sh).

(specifications->manifest
 (list
  ;; CHICKEN Scheme 5: csi, csc, chicken-install
  "chicken"
  ;; C toolchain used by csc/chicken-install to build eggs
  "gcc-toolchain"
  "make"
  "pkg-config"
  ;; SDL2 headers, libraries and sdl2-config, needed by the sdl2 egg
  "sdl2"
  ;; Basic tools so the environment also works with `guix shell --pure`
  "bash"
  "coreutils"
  "findutils"
  "grep"
  "sed"))
