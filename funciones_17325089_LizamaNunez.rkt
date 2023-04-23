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
(define (file filename extension contents . atributos)
  (list filename extension contents atributos))

;; Constructor files con formato deseado
(define set-file-partes
         (lambda (fileCompleto system)
           (list (car fileCompleto) (cadr fileCompleto) (caddr fileCompleto) (cadddr fileCompleto) (get-rutaActual system) (get-user-log system))))
  

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

;;Selector user_creador directorio
(define (get-user-directory listDirectory)
  ( car (cdr (cdr listDirectory))))
;; Selector fecha creacion directory
(define (get-fechacreacion-directory listDirectory)
  ( car (cdr (cdr ( cdr listDirectory)))))
;; Selector seguridad directory
(define (get-seguridad-directory listDirectory)
  ( car (reverse listDirectory)))

;;selector files
(define (get-files system)
  (car(cdr(reverse system))))

;;Selector nombre files
(define (get-name-file file)
  (car(car file)))

;;Selector extensión files
(define (get-extension-file file)
  (car(cdr (car file))))

;;Selector texto files
(define (get-texto-file file)
  (car(cdr(cdr (car file)))))

;;Selector atributos file
(define (get-atributos-file file)
  (car (cdr (cdr (reverse (car file))))))
;;Selector user file
(define (get-user-files files)
  (car (reverse (car files))))

;;Selector ruta files
(define (get-ruta-files files)
  (car (cdr (reverse files))))

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

;; Agregar nuevo directorio desde el nombre
(define (set-directory system nameDirectory)
  (make-system (get-nameSystem system)(get-drives system)(get-users system) (get-user-log system) (get-rutaActual system) make-fechaCreacion (cons (make-directory nameDirectory (get-rutaActual system) (get-user-log system) make-fechaCreacion make-fechaCreacion "") (get-directory system))(get-files system)(get-trash system)))

;; Copiar nuevo directorio
(define (set-neWdirectory system newDirectory)
  (make-system (get-nameSystem system)(get-drives system)(get-users system) (get-user-log system) (get-rutaActual system) make-fechaCreacion (cons newDirectory (get-directory system)) (get-files system)(get-trash system)))

;; Agregar directorio a ruta

(define (set-newRutaActual system RutaAntigua newRuta)
  (make-system (get-nameSystem system)(get-drives system)(get-users system) (get-user-log system) (reverse (cons newRuta (reverse RutaAntigua))) make-fechaCreacion (get-directory system)(get-files system)(get-trash system)))

;; Cambiar ruta por completo
(define (set-newRutaCompleta system newRutaCompleta )
  (make-system (get-nameSystem system)(get-drives system)(get-users system) (get-user-log system) newRutaCompleta make-fechaCreacion (get-directory system)(get-files system)(get-trash system)))

;; Agregar file al sistema
(define (set-files system neWfile)
  (make-system (get-nameSystem system)(get-drives system)(get-users system) (get-user-log system)(get-rutaActual system) make-fechaCreacion (get-directory system)(cons neWfile ( get-files system))(get-trash system) ))

;; Agregar File al basurero
(define (set-trash system deletedFile)
  (make-system (get-nameSystem system)(get-drives system)(get-users system) (get-user-log system)(get-rutaActual system) make-fechaCreacion (get-directory system)(get-files system)(cons '(deletedFile) (get-trash system))))

;; Cambiar ruta de file
(define (set-ruta-files ruta file system)
  (list (get-name-file file) (get-extension-file file) (get-texto-file file) (get-atributos-file file) ruta (get-user-log system)))

;; Cambiar ruta de directory
(define (set-ruta-directory ruta directory system)
  (make-directory (get-name-Cadadirectory directory) ruta (get-user-log system) make-fechaCreacion make-fechaCreacion (car (reverse (car directory)))))


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

                ;;CAMBIAR!!!!!!!  
;; Nombre de directorio ya se encuentra ocupado
(define (isNameDirectory? nameDirectory system)
  (member nameDirectory (map (lambda (listNAmeDirectory) (get-name-directory listNAmeDirectory))
                             (get-directory system))))
;; listDirectory (get-directory system)
;; Existe nombre directory?
(define (existNameDirectory? nameDirectory listDirectory)
  (cond
    [(null? listDirectory) #f]
    [ (equal? (get-name-directory (car listDirectory)) nameDirectory) #t]
    [else (existNameDirectory? nameDirectory (cdr listDirectory))]))

;;listFile (get-files system)
;; Existe nombre file?
(define (existNamefile? nameFile listFile)
  (cond
    [(null? listFile) #f]
    [ (equal? (get-listname-file (car listFile)) nameFile) #t]
    [else (existNamefile? nameFile (cdr listFile))]))

;; Nombre de Files existe
(define (isNameFile? nameFile system)
  (member nameFile (map (lambda (listNamesFiles) (get-listname-file listNamesFiles))
                             (get-files system))))

;; Encontrar String en nombre de archivos


;; Encontrar Letra inicial y extension


;;CAPA : OTRAS FUNCIONES

;; redefinir file - trash
(define (redefine-files-trash system files trash)
  (make-system (get-nameSystem system)(get-drives system)(get-users system) (get-user-log system)(get-rutaActual system) make-fechaCreacion (get-directory system) files (cons trash (get-trash system))))
;; redefinir file
(define (redefine-files system files)
  (make-system (get-nameSystem system)(get-drives system)(get-users system) (get-user-log system)(get-rutaActual system) make-fechaCreacion (get-directory system) files (get-trash system)))


;; redefinir directory - trash
(define (redefine-directory-trash system listDirectory trash)
  (make-system (get-nameSystem system)(get-drives system)(get-users system) (get-user-log system)(get-rutaActual system) make-fechaCreacion listDirectory (get-files system) (cons trash (get-trash system))))

;; redefinir directory
(define (redefine-directory system listDirectory)
  (make-system (get-nameSystem system)(get-drives system)(get-users system) (get-user-log system)(get-rutaActual system) make-fechaCreacion listDirectory (get-files system) (get-trash system)))




;;Recorrido : (("nombre" "extension"......)) (lista de lista)
;; Recorrer file en busca de nameFile
(define (buscar-file nombreArchivo listaFile)
      (cond
        [(null? listaFile) null]
        [(equal? (get-name-file listaFile) nombreArchivo) (cons (car listaFile)(buscar-file nombreArchivo (cdr listaFile)))]
        [else (buscar-file nombreArchivo (cdr listaFile))]))

;;Recorrer directory en busca de nameDirectory
(define (buscar-directory nombreArchivo listaDirectory)
      (cond
        [(null? listaDirectory) null]
        [(equal? (get-name-Cadadirectory listaDirectory) nombreArchivo) (cons (car listaDirectory)(buscar-directory nombreArchivo (cdr listaDirectory)))]
        [else (buscar-directory nombreArchivo (cdr listaDirectory))]))


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
                 (if (eq? (string-ref folderName 2) #\/)(set-newRutaCompleta system (string-split folderName "/"))
                     system))))))


;;Agregar files
;;(define (add-file system)
  ;;(lambda (filename extension content)
    ;;(set-files system filename extension content)))

;; Funcion 10: TDA system add-file

(define add-file
  (lambda (system)
    (lambda (file)
      (set-files system (set-file-partes file system)))))



;; Funcion 11: TDA system-del
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

;; Funcion 12: TDA system- rd (revisar)
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
(define (copy system)
  (lambda (archivo ruta)
    (if (existNamefile? archivo (get-files system)) (set-files system (set-ruta-files (string-split ruta "/") (buscar-file archivo (get-files system)) system))
        (if (existNameDirectory? archivo (get-directory system))  (set-neWdirectory system (set-ruta-directory (string-split ruta "/") (buscar-directory archivo (get-directory system)) system))system))))

;; Función 14: TDA system - move
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
(define (ren system)
  (lambda (originalName newName)
    ;; buscar file files =get-files system
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
        [(equal? (get-name-directory (car directories)) originalName) (isInDirectory? originalName (cdr directories) (cons (list newName (get-ruta-directory (car directories)) (get-user-directory  (car directories)) (get-fechacreacion-directory (car directories)) make-fechaCreacion (get-seguridad-directory (car directories))) acumDirectories))]
        [ else (isInDirectory? originalName (cdr directories) (cons (car directories) acumDirectories))]))

    (if (or (existNamefile? newName (get-files system)) (existNameDirectory? newName (get-directory system))) system 
        (if (existNamefile? originalName (get-files system)) (isInFile? originalName (get-files system) '())
            (if (existNameDirectory? originalName (get-directory system))  (isInDirectory? originalName (get-directory system) '()) system)))))

;; Función 16: TDA system - dir
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