#lang racket


;;CONSTRUCTOR

;; Constructor system 
(define (make-system name drives-ingresados users log drive-actual trash fechaCreacion)
  (list name drives-ingresados users log drive-actual trash fechaCreacion))
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
;; Constructor fecha creacion
(define make-fechaCreacion (current-seconds))

;;SELECTOR
;; Nombre systema
(define get-nameSystem car)
;; lista de drives
(define get-drives-ingresados cadr)
;; lista de usuarios ingresados
(define get-users caddr)
;; usuario logeado
(define get-user-log cadddr)
;; drive en uso actualmente
(define (get-drive-actual system)
  (car( cdr (cdr(reverse system)))))
;; trash
(define (get-trash system)
  (car ( cdr(reverse system))))
;; fechaCreacion
(define (get-fechaCreacion)
  (car(reverse system)))

;; MODIFICADORES

;; type filesystem = list name X drives-ingresados X users X log X drive_actual X trash X fechaCreacion 

;; Funcion 1: Creacion de nuevos sistemas
(define (system nombre)
  (list nombre '() '() "" "" '() make-fechaCreacion ))

;; Funcion 2 : TDA system -run
(define(run system command)
  (command system))
;; type letters = list letter X name X capacity X directory X file
  
;; Funcion 3: TDA system - add-drive
(define (add-drive system)
    (lambda (letter nameDrive capacity)
      (make-system (get-nameSystem system)(cons (make-letters letter nameDrive capacity "" '())(get-drives-ingresados system))(get-users system)(get-user-log system)(get-drive-actual system)(get-trash system) make-fechaCreacion)))

;; Funcion 4: TDA system -register
(define (add-user system)
  (lambda (userName)
    (make-system (get-nameSystem system)(get-drives-ingresados system)(cons userName (get-users system))(get-user-log system)(get-drive-actual system)(get-trash system) make-fechaCreacion)))

;; Funcion 5: TDA system -login
(define (login system)
  (lambda (userName-log)
    (cond
      [(eq? "" (get-user-log system))
      (make-system (get-nameSystem system)(get-drives-ingresados system)(get-users system) userName-log (get-drive-actual system)(get-trash system) make-fechaCreacion)]
      [else system])))

;; Funcion 6: TDA system logout
(define (logout system)
  ( make-system (get-nameSystem system)(get-drives-ingresados system)(get-users system) "" (get-drive-actual system)(get-trash system) make-fechaCreacion))

;; Funcion 7: TDA system - switch-drive
(define(switch-drive system)
  (lambda (letter)
    (make-system (get-nameSystem system)(get-drives-ingresados system)(get-users system) (get-user-log system) letter (get-trash system) make-fechaCreacion)))

;; type name-directory = list name X ruta X user-creador X fecha-creacion X fecha-modificacion X seguridad X file
;; Funcion 8: TDA system-md (make directory)
