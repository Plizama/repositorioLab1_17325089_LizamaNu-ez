#lang racket


;;CONSTRUCTOR

;; Constructor system (falta trash)
(define (make-system name drives-ingresados users log drive-actual)
  (list name drives-ingresados users log drive-actual))
;;P--(make-system "hola" "C" "pamela" "luko" "D")

;; Constructor drives-ingresados
(define (make-drives-ingresados letters)
  (list letters))
;;P--(make-drives-ingresados  "C")

;; Constructor letters
(define (make-letters letter name capacity directory file)
  (list letter name capacity directory file))

;;P--(make-letters "A" "primera" 200 "c/file/hola" "carpeta")

;; Constructor directory
(define (make-name-directory name-directory)
  (list name-directory))

(define (make-directory name ruta user-creator fecha-creacion fecha-modificacion seguridad file)
  (list name ruta user-creator fecha-creacion fecha-modificacion seguridad file))

;; Constructor file
(define (make-name-file name-file)
  (list name-file))

(define (make-file name ruta tipo texto atributos)
  (list name ruta tipo texto atributos))

;; Constructor users
(define (make-users name-user)
  (list name-user))

;;SELECTOR
