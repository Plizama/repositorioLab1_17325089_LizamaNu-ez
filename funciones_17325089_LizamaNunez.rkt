#lang racket

;; type filesystem = list name X drives X users X userLog X rutaActual X fechaCreacion X directory X file X trash

;; CAPA CONSTRUCTORA

;; Constructor sistema 
(define (make-system name drives users userLog rutaActual fechaCreacion directory file trash)
  (list name drives users userLog rutaActual fechaCreacion directory file trash))

;; Constructor drives
(define (make-drives letter name capacity directory file)
  (list letter name capacity directory file))

;; Constructor directory
(define (make-directory name ruta user-creator fecha-creacion fecha-modificacion seguridad)
  (list name ruta user-creator fecha-creacion fecha-modificacion seguridad))

;; Constructor files
(define (make-file filename extension contents)
  (list filename extension contents))

;; Constructor users
(define (make-users name-user)
  (list name-user))

;; Constructor fecha creacion
(define make-fechaCreacion (current-seconds))
;; Constructor Trash
(define (make-trash deletedFile trash)
 (cons deletedFile '(trash)))

;; CAPA SELECTORA

;; type filesystem = list name X drives X users X userLog X rutaActual X fechaCreacion X directory X file X trash


;; Selector nombre systema
(define get-nameSystem car)

;; Selector lista de drives
(define get-drives cadr)

   ;; Selector nombre drive
(define (get-name-drive listadoDrives)
   (car listadoDrives ))

;; Selector lista de usuarios ingresados
(define get-users caddr)

;; Selector usuario logeado
(define get-user-log cadddr)

;; Selector ruta actual
(define (get-rutaActual system)
  (car( cdr (cdr (cdr(cdr(reverse system)))))))

;; Selector Fecha creacion
(define (get-fechaCreacion system)
  (car(cdr(cdr(cdr(reverse system))))))

;; Selector directory
(define (get-directory system)
  (car(cdr(cdr(reverse system)))))

;; Selector Listado Nombre directorio
(define (get-name-directory listDirectory)
  (car listDirectory))

;; Selector Cada nombre directorio
(define (get-name-Cadadirectory listDirectory)
  (car (car listDirectory)))

;; Selector Ruta directorio
(define (get-ruta-directory listDirectory)
  (cadr listDirectory))

;;selector files
(define (get-files system)
  (car(cdr(reverse system))))

;;Selector nombre files
(define (get-name-file file)
  (car(car file)))

;;Listado Nombre files
(define (get-listname-file file)
  (car file))

;; Selector trash
(define (get-trash system)
  (car(reverse system)))

;; Selecionar ruta de carpeta indicada
(define (get-rutaIndicada system folder)
  (car (cdr (car (filter (lambda (str) (eq? (get-name-directory str) folder)) (get-directory system))))))

;; CAPA MODIFICADORA

;; Agregar drives sistema
(define (set-drives system letter nameDrive capacity)
  (make-system (get-nameSystem system)(cons (make-drives letter nameDrive capacity "" '())(get-drives system))(get-users system)(get-user-log system)(get-rutaActual system) make-fechaCreacion (get-directory system)(get-files system)(get-trash system)))

;; Agregar users al sistema
(define (set-users system userName)
  (make-system (get-nameSystem system)(get-drives system)(cons userName (get-users system))(get-user-log system)(get-rutaActual system) make-fechaCreacion (get-directory system)(get-files system)(get-trash system)))

;; Agregar usuario Logueado
(define (set-log system userName-log)
  (make-system (get-nameSystem system)(get-drives system)(get-users system) userName-log (get-rutaActual system)make-fechaCreacion (get-directory system)(get-files system)(get-trash system)))

;; Quitar usuario logueado
(define (set-logout system)
  (make-system (get-nameSystem system)(get-drives system)(get-users system) "" (get-rutaActual system) make-fechaCreacion (get-directory system)(get-files system)(get-trash system)))

;; Agregar unidad fijada
(define (set-rutaActual system letter)
  (make-system (get-nameSystem system)(get-drives system)(get-users system) (get-user-log system) (list letter) make-fechaCreacion (get-directory system)(get-files system) (get-trash system)))

;; type name-directory = list name X ruta X user-creador X fecha-creacion X fecha-modificacion X seguridad

;; Agregar directorio
(define (set-directory system nameDirectory)
  (make-system (get-nameSystem system)(get-drives system)(get-users system) (get-user-log system) (get-rutaActual system) make-fechaCreacion (cons (make-directory nameDirectory (get-rutaActual system) (get-user-log system) make-fechaCreacion make-fechaCreacion "") (get-directory system))(get-files system)(get-trash system)))

;; Cambiar ruta

(define (set-newRutaActual system RutaAntigua newRuta)
  (make-system (get-nameSystem system)(get-drives system)(get-users system) (get-user-log system) (reverse (cons newRuta (reverse RutaAntigua))) make-fechaCreacion (get-directory system)(get-files system)(get-trash system)))

;; Agregar file al sistema
(define (set-files system filename extension content)
  (make-system (get-nameSystem system)(get-drives system)(get-users system) (get-user-log system)(get-rutaActual system) make-fechaCreacion (get-directory system)(cons (make-file filename extension content) (get-files system))(get-trash system) ))

;; Agregar File al basurero
(define (set-trash system deletedFile)
  (make-system (get-nameSystem system)(get-drives system)(get-users system) (get-user-log system)(get-rutaActual system) make-fechaCreacion (get-directory system)(get-files system)(cons '(deletedFile) (get-trash system))))


;; CAPA PERTENENCIA

;; Nombre drive pertenece a listado de drives
(define (isNombreDrive? nombreDrive system)
  (member nombreDrive (map (lambda (listadoDrives) (get-name-drive listadoDrives))
                                 (get-drives system))))

;; Nombre de usuario pertenece a lista de usuarios
(define (isUserList? nombreUser system)
  (member nombreUser (get-users system)))

;; Nombre de usuario se encuentra logueado
(define (isUserLog? nombreUser system)
  (eq? nombreUser (get-user-log system)))

                  
;; Nombre de directorio ya se encuentra ocupado
(define (isNameDirectory? nameDirectory system)
  (member nameDirectory (map (lambda (listNAmeDirectory) (get-name-directory listNAmeDirectory))
                             (get-directory system))))

;; Nombre de Files existe
(define (isNameFile? nameFile system)
  (member nameFile (map (lambda (listNamesFiles) (get-listname-file listNamesFiles))
                             (get-files system))))

;; Encontrar String en nombre de archivos


;; Encontrar Letra inicial y extension


;;CAPA : OTRAS FUNCIONES

;; redefinir file - trash
(define (redefine-files system files trash)
  (make-system (get-nameSystem system)(get-drives system)(get-users system) (get-user-log system)(get-rutaActual system) make-fechaCreacion (get-directory system) files (cons trash (get-trash system))))
;; redefinir directory - trash
(define (redefine-directory system listDirectory trash)
  (make-system (get-nameSystem system)(get-drives system)(get-users system) (get-user-log system)(get-rutaActual system) make-fechaCreacion listDirectory (get-files system) (cons trash (get-trash system))))


;; FUNCIONES SISTEMA

;; type filesystem = list name X drives X users X userLog X rutaActual X fechaCreacion X directory X file X trash

;; Funcion 1: Creacion de nuevos sistemas
(define (system nombre)
  (list nombre '() '() "" '() make-fechaCreacion '() '() '()))

;; Funcion 2 : TDA system -run
(define (run system command)
  (command system))

;; Funcion 3: TDA system - add-drive
(define (add-drive system)
  (lambda (letter nameDrive capacity)
    (if (null? (get-drives system)) (set-drives system letter nameDrive capacity)
               (if (isNombreDrive? letter system) system
                   (set-drives system letter nameDrive capacity)))))
      
;; Funcion 4: TDA system -register
(define (add-user system)
  (lambda (userName)
    ( if (null? (get-users system)) (set-users system userName)
         (if (isUserList? userName system) system
             (set-users system userName)))))

;; Funcion 5: TDA system -login
(define (login system)
  (lambda (userName-log)
    (if (eq? "" (get-user-log system)) (set-log system userName-log)
        system )))

;; Funcion 6: TDA system logout
(define (logout system)
  (set-logout system))

;; Funcion 7: TDA system - switch-drive
(define(switch-drive system)
  (lambda (letter)
    (if (eq? "" (get-user-log system)) system
        (if (isNombreDrive? letter system ) (set-rutaActual system letter)
            system))))

;; type name-directory = list name X ruta X user-creador X fecha-creacion X fecha-modificacion X seguridad
;; Funcion 8: TDA system-md (make directory)
(define (md system)
  (lambda (nameDirectory)
    ( if (null? (get-directory system)) (set-directory system nameDirectory)
         (if (isNameDirectory? nameDirectory system) system
             (set-directory system nameDirectory)))))

;;Funcion 9: TDA system -cd (change directory)
;; FALTA S30
(define (cd system)
  (lambda (folderName)
    (if (eq? folderName "..") (set-rutaActual system (reverse(cdr(reverse (get-rutaActual system)))))
        (if (eq? folderName "/") (set-rutaActual system (car(get-rutaActual system)))
            ( if (isNameDirectory? folderName system) (set-newRutaActual system (get-rutaIndicada system folderName) folderName)
                 system)))))


;;Agregar files
(define (add-file system)
  (lambda (filename extension content)
    (set-files system filename extension content)))

;; Funcion 10: TDA system add-file





;; Funcion 11: TDA system-del
(define (del system)
  (lambda (archivoBorrado)
    ;; Funcion interna identifica presencia de trozo de string en nombre de archivos.
    (define (isInFile? str files acumTrash acumFiles)
      (cond
        [(null? files) acumTrash acumFiles
                       (redefine-files system acumFiles acumTrash)]
        [(string-contains? (get-name-file files) str) (isInFile? str (cdr files) (cons (car files) acumTrash) acumFiles)]
        [ else (cons (car files) (isInFile? str (cdr files) acumTrash (cons (car files) acumFiles)))]))
    
    ;; Funcion interna identifica la presencia de letra de inicio de archivo y final de string
    (define (isInFilExtenLetter? str files acumTrash acumFiles)
      (cond
        [(null? files) acumTrash acumFiles
                       (redefine-files system acumFiles acumTrash)]
        [(and (eq? (string-ref (get-name-file files) 0) (string-ref str 0))(string-contains? (get-name-file files) (substring str 2))) (isInFilExtenLetter? str (cdr files)(cons (car files) acumTrash) acumFiles)]
        [else (cons (car files) (isInFilExtenLetter? str (cdr files) acumTrash (cons (car files) acumFiles)))]))
    ;; Funcion interna revisa file con nombre indicado
    (define (isNameFileinFile? str files acumTrash acumFiles)
      (cond
        [(null? files) acumTrash acumFiles
                       (redefine-files system acumFiles acumTrash)]
        [(equal? (get-name-file files) str) (isNameFileinFile? str (cdr files) (cons (car files) acumTrash) acumFiles)]
        [ else (cons (car files) (isNameFileinFile? str (cdr files) acumTrash (cons (car files) acumFiles)))]))

    ;; Funcion interna revisa directorio con nombre indicado
    (define (isNameDirectoryinDirectory? str directories acumTrash acumDirectory)
      (cond
        [(null? directories) acumTrash acumDirectory
                       (redefine-directory system acumDirectory acumTrash)]
        [(equal? (get-name-Cadadirectory directories) str) (isNameDirectoryinDirectory? str (cdr directories) (cons (car directories) acumTrash) acumDirectory)]
        [ else (cons (car directories) (isNameDirectoryinDirectory? str (cdr directories) acumTrash (cons (car directories) acumDirectory)))]))

    
    (if (isNameDirectory? archivoBorrado system)(isNameDirectoryinDirectory? archivoBorrado (get-directory system) '()'())
        (if (eq? archivoBorrado "*.*") (redefine-files system '() (get-files system))
            (if (isNameFile? archivoBorrado system)(isNameFileinFile? archivoBorrado (get-files system) '()'())
                (if (eq? (string-ref archivoBorrado 0) #\*) (isInFile? (substring archivoBorrado 1) (get-files system) '() '())
                    (if (eq? (string-ref archivoBorrado 1) #\*) (isInFilExtenLetter? archivoBorrado (get-files system) '()'()) system)))))))