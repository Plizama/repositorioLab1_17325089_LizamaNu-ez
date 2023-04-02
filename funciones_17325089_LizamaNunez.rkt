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
;; Nombre systema
(define get-nameSystem car)
;; lista de drives
(define get-drives-ingresados cadr)
;; lista de usuarios ingresados
(define get-users caddr)
;;usuario logeado
(define get-user-log cadddr)
;;drive en uso actualmente
(define (get-drive-actual system)
  (car (reverse system)))

;; modificadora


;; type filesystem = list name X drives-ingresados X users X log X drive_actual

;; Funcion 1: Creacion de nuevos sistemas
(define (system nombre)
  (list nombre '() '() "" ""  ))

;; Funcion 2 : TDA system -run
(define(run system command)
  (command system))

;; Funcion 3: TDA system - add-drive
(define (addDrive system letter nameDrive capacity)
  (make-system (get-nameSystem system)(make-drives-ingresados(make-letters letter nameDrive capacity "" '()))(get-users system)(get-user-log system)(get-drive-actual system)))
