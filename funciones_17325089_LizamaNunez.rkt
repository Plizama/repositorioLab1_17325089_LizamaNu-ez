#lang racket

(require "TDA_lab1_17325089_Lizama.rkt")

;; FUNCIONES SISTEMA

;; Funcion 1: Creacion de nuevos sistemas
;Descripción: Funcion crea un nuevo sistema
;Dom: name sistema(str)
;Rec: sistema (list)

(define (system nombre)
  (list nombre '() '() "" '() make-fechaCreacion '() '() '()))

;; Funcion 2 : TDA system -run
;Descripción: Funcion que permite ejercutar un comando
;Dom: sistema(list) X comando (función)
;Rec: comando (función) X sistema(list)
(define (run system command)
  (command system))

;; Funcion 3: TDA system - add-drive
;Descripción: Funcion ingresa un nuevo drive al sistema
;Dom: sistema(list) X letter drive (str) X name drive (str) X capacity drive (num)
;Rec: sistema(list)
(define (add-drive system)
  (lambda (letter nameDrive capacity)
    (if (null? (get-drives system)) (set-drives system letter nameDrive capacity)
               (if (isNombreDrive? letter system) system
                   (set-drives system letter nameDrive capacity)))))
      
;; Funcion 4: TDA system -register
;Descripción: Funcion que permite registrar usuarios en el sistema
;Dom: sistema(list) X nombre usuario (str)
;Rec: sistema(list)
(define (add-user system)
  (lambda (userName)
    ( if (null? (get-users system)) (set-users system userName)
         (if (isUserList? userName system) system
             (set-users system userName)))))

;; Funcion 5: TDA system -login
;Descripción: Funcion que permite loguear a un usuario
;Dom: sistema(list) X nombre usuario (str)
;Rec: sistema(list)
(define (login system)
  (lambda (userName-log)
    (if (eq? "" (get-user-log system)) (set-log system userName-log)
        system )))

;; Funcion 6: TDA system logout
;Descripción: Funcion que permite desloguear a un usuario
;Dom: sistema(list)
;Rec: sistema(list)
(define (logout system)
  (set-logout system))

;; Funcion 7: TDA system - switch-drive
;Descripción: Funcion que permite fijar una unidad en la ruta
;Dom: sistema(list) X nombre unidad (str)
;Rec: sistema(list)
(define(switch-drive system)
  (lambda (letter)
    (if (eq? "" (get-user-log system)) system
        (if (isNombreDrive? letter system ) (set-rutaActual system letter)
            system))))

;; Funcion 8: TDA system-md (make directory)
;Descripción: Función que permite crear un nuevo directorio
;Dom: sistema(list) X nombre directorio (str)
;Rec: sistema(list)
(define (md system)
  (lambda (nameDirectory)
    ( if (null? (get-directory system)) (set-directory system nameDirectory)
         (if (isNameDirectory? nameDirectory system) system
             (set-directory system nameDirectory)))))

;;Funcion 9: TDA system -cd (change directory)
;Descripción: Función que permite cambiar la ruta actual
;Dom: sistema(list) X folderName (str)
;Rec: sistema(list)
(define (cd system)
  (lambda (folderName)
    (if (eq? folderName "..") (set-rutaActual system (reverse(cdr(reverse (get-rutaActual system)))))
        (if (eq? folderName "/") (set-rutaActual system (car(get-rutaActual system)))
            ( if (isNameDirectory? folderName system) (set-newRutaActual system (get-rutaIndicada system folderName) folderName)
                 (if (eq? (string-ref folderName 2) #\/)(set-newRutaCompleta system (string-split folderName "/"))
                     system))))))



;; Funcion 10: TDA system add-file
;Descripción: Función que permite agregar files al sistema
;Dom: sistema(list) X file(función)
;Rec: sistema(list)

(define add-file
  (lambda (system)
    (lambda (file)
      (set-files system (make-file file system)))))



;; Funcion 11: TDA system-del
;Descripción: Función que permite eliminar un archivo o carpeta 
;Dom: sistema(list) X patron para borrar archivo (str)
;Rec: sistema(list)
(define (del system)
  (lambda (archivoBorrado)
    ;; Funcion interna identifica presencia de trozo de string en nombre de archivos.
    (define (isInFile? str files acumTrash acumFiles)
      (cond
        [(null? files) acumTrash acumFiles
                       (redefine-files-trash system acumFiles acumTrash)]
        [(string-contains? (get-name-file files) str) (isInFile? str (cdr files) (cons (car files) acumTrash) acumFiles)]
        [ else (cons (car files) (isInFile? str (cdr files) acumTrash (cons (car files) acumFiles)))]))
    
    ;; Funcion interna identifica la presencia de letra de inicio de archivo y final de string
    (define (isInFilExtenLetter? str files acumTrash acumFiles)
      (cond
        [(null? files) acumTrash acumFiles
                       (redefine-files-trash system acumFiles acumTrash)]
        [(and (eq? (string-ref (get-name-file files) 0) (string-ref str 0))(string-contains? (get-name-file files) (substring str 2))) (isInFilExtenLetter? str (cdr files)(cons (car files) acumTrash) acumFiles)]
        [else (cons (car files) (isInFilExtenLetter? str (cdr files) acumTrash (cons (car files) acumFiles)))]))
    ;; Funcion interna revisa file con nombre indicado
    (define (isNameFileinFile? str files acumTrash acumFiles)
      (cond
        [(null? files) acumTrash acumFiles
                       (redefine-files-trash system acumFiles acumTrash)]
        [(equal? (get-name-file files) str) (isNameFileinFile? str (cdr files) (cons (car files) acumTrash) acumFiles)]
        [ else (cons (car files) (isNameFileinFile? str (cdr files) acumTrash (cons (car files) acumFiles)))]))

    ;; Funcion interna revisa directorio con nombre indicado
    (define (isNameDirectoryinDirectory? str directories acumTrash acumDirectory)
      (cond
        [(null? directories) acumTrash acumDirectory
                       (redefine-directory-trash system acumDirectory acumTrash)]
        [(equal? (get-name-Cadadirectory directories) str) (isNameDirectoryinDirectory? str (cdr directories) (cons (car directories) acumTrash) acumDirectory)]
        [ else (cons (car directories) (isNameDirectoryinDirectory? str (cdr directories) acumTrash (cons (car directories) acumDirectory)))]))

    
    (if (isNameDirectory? archivoBorrado system)(isNameDirectoryinDirectory? archivoBorrado (get-directory system) '()'())
        (if (eq? archivoBorrado "*.*") (redefine-files-trash system '() (get-files system))
            (if (isNameFile? archivoBorrado system)(isNameFileinFile? archivoBorrado (get-files system) '()'())
                (if (eq? (string-ref archivoBorrado 0) #\*) (isInFile? (substring archivoBorrado 1) (get-files system) '() '())
                    (if (eq? (string-ref archivoBorrado 1) #\*) (isInFilExtenLetter? archivoBorrado (get-files system) '()'()) system)))))))

;; Funcion 12: TDA system- rd
;Descripción: Función que permite borrar carpetas cuando esten vacias
;Dom: sistema(list) X folderName (str)
;Rec: sistema(list)
(define (rd system)
  (lambda (folderName)
    (define (directoryVacio? str listaRutas)
      (cond
        [(null? listaRutas) #t]
        [(member str (car listaRutas)) #f]
        [else (directoryVacio? str (cdr listaRutas))]))
    
    (define (borrar-folder str directories acumTrash acumDirectory)
      (cond
        [(null? directories) acumTrash acumDirectory
                       (redefine-directory-trash system acumDirectory acumTrash)]
        [(equal? (get-name-Cadadirectory directories) str) (borrar-folder str (cdr directories) (cons (car directories) acumTrash) acumDirectory)]
        [ else (cons (car directories) (borrar-folder str (cdr directories) acumTrash (cons (car directories) acumDirectory)))]))

    (if (and (directoryVacio? folderName (map (lambda (listaRutasFiles) (get-ruta-files listaRutasFiles)) (get-files system)))(directoryVacio? folderName (map (lambda (listaRutasDirectories) (get-ruta-directory listaRutasDirectories)) (get-directory system)))) (borrar-folder folderName (get-directory system) '() '()) system )))

;; Función 13: TDA system - copy
;Descripción: Función que permite copiar un archivo o carpeta en otra ruta
;Dom: sistema(list) X folderName (str) X ruta (str)
;Rec: sistema(list)
(define (copy system)
  (lambda (archivo ruta)
    (if (existNamefile? archivo (get-files system)) (set-files system (set-ruta-files (string-split ruta "/") (buscar-file archivo (get-files system)) system))
        (if (existNameDirectory? archivo (get-directory system))  (set-neWdirectory system (set-ruta-directory (string-split ruta "/") (buscar-directory archivo (get-directory system)) system))system))))

;; Función 14: TDA system - move
;Descripción: Función que permite mover un archivo o carpeta a otra ruta
;Dom: sistema(list) X folderName (str) X ruta (str)
;Rec: sistema(list)
(define (move system)
  (lambda (folder ruta)
    ;; buscar file files =get-files system
    (define (isInFile? folder files acumFiles)
      (cond
        [(null? files)  acumFiles
                       (redefine-files system acumFiles)]
        [(equal? (get-listname-file (car files)) folder) (isInFile? folder (cdr files) (cons (set-ruta-files (string-split ruta "/") files system) acumFiles))]
        [ else (isInFile? folder (cdr files) (cons (car files) acumFiles))]))

    ;;buscar directory  directories = get-directory
    (define (isInDirectory? folder directories acumDirectories)
      (cond
        [(null? directories)  acumDirectories
                       (redefine-directory system acumDirectories)]
        [(equal? (get-name-directory (car directories)) folder) (isInDirectory? folder (cdr directories) (cons (set-ruta-directory (string-split ruta "/") directories system) acumDirectories))]
        [ else (isInDirectory? folder (cdr directories) (cons (car directories) acumDirectories))]))

    (if (existNamefile? folder (get-files system)) (isInFile? folder (get-files system) '())
        (if (existNameDirectory? folder (get-directory system))  (isInDirectory? folder (get-directory system) '()) system))))

;; Función 15: TDA system ren (rename)
;Descripción: Función que permite cambiar nombre de archivo
;Dom: sistema(list) X Nombre original (str) X Nuevo nombre (str)
;Rec: sistema(list)
(define (ren system)
  (lambda (originalName newName)
    ;; buscar file files
    (define (isInFile? originalName files acumFiles)
      (cond
        [(null? files)  acumFiles
                       (redefine-files system acumFiles)]
        [(equal? (get-listname-file (car files)) originalName) (isInFile? originalName (cdr files) (cons (list newName (get-extension-file files) (get-texto-file files) (get-atributos-file files) (get-ruta-files (car files)) (get-user-files files)) acumFiles))]
        [ else (isInFile? originalName (cdr files) (cons (car files) acumFiles))]))

    ;;buscar directory  directories = get-directory
    (define (isInDirectory? originalName directories acumDirectories)
      (cond
        [(null? directories)  acumDirectories
                       (redefine-directory system acumDirectories)]
        [(equal? (get-name-directory (car directories)) originalName) (isInDirectory? originalName (cdr directories) (cons (list newName (get-ruta-directory (car directories)) (get-user-directory  (car directories)) (get-fechacreacion-directory (car directories)) make-fechaCreacion ) acumDirectories))]
        [ else (isInDirectory? originalName (cdr directories) (cons (car directories) acumDirectories))]))

    (if (or (existNamefile? newName (get-files system)) (existNameDirectory? newName (get-directory system))) system 
        (if (existNamefile? originalName (get-files system)) (isInFile? originalName (get-files system) '())
            (if (existNameDirectory? originalName (get-directory system))  (isInDirectory? originalName (get-directory system) '()) system)))))

;; Función 16: TDA system - dir
;Descripción: Función que permite listar el contenido de un directorio
;Dom: sistema(list) X parametro (str)
;Rec: contenido directorio (str)
(define (dir system)
  (lambda(parametro)
    (define (recorridoRutasFiles files rutaEscogida acumContenido)
      (cond
        [(null? files) acumContenido]
        [(equal? (car (reverse (get-ruta-files (car files)))) rutaEscogida) (recorridoRutasFiles (cdr files) rutaEscogida (cons (get-listname-file (car files)) acumContenido))]
        [else (recorridoRutasFiles (cdr files) rutaEscogida acumContenido)]))

    (define (recorridoRutasDirectories directories rutaEscogida acumContenido)
      (cond
        [(null? directories) acumContenido]
        [(equal? (car (reverse (get-ruta-directory (car directories)))) rutaEscogida) (recorridoRutasDirectories (cdr directories) rutaEscogida (cons (get-name-directory (car directories)) acumContenido))]
        [else (recorridoRutasDirectories (cdr directories) rutaEscogida acumContenido)]))

   ;; (if (null? parametro) (append (recorridoRutasFiles (get-files system) (car (reverse (get-rutaActual system))) '()) (recorridoRutasDirectories (get-directory system) (car (reverse (get-rutaActual system))) '())))
   (if (equal? parametro "/a") (append (recorridoRutasFiles (get-files system) (car (reverse (get-rutaActual system))) '()) (recorridoRutasDirectories (get-directory system) (car (reverse (get-rutaActual system))) '()))
       (if (equal? parametro "/o N") (sort (append (recorridoRutasFiles (get-files system) (car (reverse (get-rutaActual system))) '()) (recorridoRutasDirectories (get-directory system) (car (reverse (get-rutaActual system))) '())) string<? ) system) )))