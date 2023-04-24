#lang racket
(provide make-fechaCreacion)
(provide get-drives)
(provide set-drives)
(provide isNombreDrive?)
(provide get-users)
(provide set-users)
(provide isUserList?)
(provide get-user-log)
(provide set-log)
(provide set-logout)
(provide set-rutaActual)
(provide get-directory)
(provide set-directory)
(provide isNameDirectory?)
(provide get-rutaActual)
(provide set-newRutaActual)
(provide get-rutaIndicada)
(provide set-newRutaCompleta)
(provide set-files)
(provide make-file)
(provide redefine-files-trash)
(provide get-name-file)
(provide redefine-directory-trash)
(provide get-name-Cadadirectory)
(provide get-files)
(provide isNameFile?)
(provide get-ruta-files)
(provide get-ruta-directory)
(provide existNamefile?)
(provide set-ruta-files)
(provide buscar-file)
(provide existNameDirectory?)
(provide set-neWdirectory)
(provide set-ruta-directory)
(provide buscar-directory)
(provide redefine-files)
(provide get-listname-file)
(provide redefine-directory)
(provide get-name-directory)
(provide get-extension-file)
(provide get-texto-file)
(provide get-atributos-file)
(provide get-user-files)
(provide get-user-directory)
(provide get-fechacreacion-directory)
(provide file)

;; type filesystem = list name(str) X drives(list) X users(list) X userLog(str) X rutaActual(list) X fechaCreacion (num) X directories(list) X files(list) X trash(list)
;; type name = str name

;; type drives = list drive(list)
  ;; type drive = list letter(str) X name(str) X capacity (num)

;; type users = list user(str)

;; type userLog = userLog(str)

;; type rutaActual = list drive X directory X subdirectory

;; type fechaCreacion = fechaCreacion (num)

;; type directories = list directory
  ;; type directory = list name(str) X ruta(list) X userCreador(str) X fechaCreacion(num) X fechaModificación(num)

;; type files = list file
  ;; type file = list name(str) X extension(str) X contenido(str) | atrubutos(str) X ruta(list) X userCreador(str)


;; CAPA CONSTRUCTORA

;; Constructor sistema 
;Descripción: construye el sistema base
;Dom: name(str) X drives(list) X users(list) X userLog(str) X rutaActual(list) X fechaCreacion (num) X directories(list) X files(list) X trash(list)
;Rec: list name(str) X drives(list) X users(list) X userLog(str) X rutaActual(list) X fechaCreacion (num) X directories(list) X files(list) X trash(list)
(define (make-system name drives users userLog rutaActual fechaCreacion directory file trash)
  (list name drives users userLog rutaActual fechaCreacion directory file trash))

;; Constructor drives
;Descripción: construye drives
;Dom: letter(str) X name(str) X capacity (num)
;Rec: list letter(str) X name(str) X capacity (num)
(define (make-drives letter name capacity)
  (list letter name capacity))


;; Constructor directory
;Descripción: Crea directorios
;Dom: name(str) X ruta(list) X userCreador(str) X fechaCreacion(num) X fechaModificación(num)
;Rec:list name(str) X ruta(list) X userCreador(str) X fechaCreacion(num) X fechaModificación(num)
(define (make-directory name ruta user-creator fecha-creacion fecha-modificacion )
  (list name ruta user-creator fecha-creacion fecha-modificacion ))

;; Constructor file información desde usuario
;Descripción: Crea lista con información del file entregada por el usuario
;Dom: name(str) X extension(str) X contenido(str) | atrubutos(str)
;Rec: list name(str) X extension(str) X contenido(str) | atrubutos(str)
(define (file filename extension contents . atributos)
  (list filename extension contents atributos))

;; Constructor files
;Descripción: Crea file
;Dom: name(str) X extension(str) X contenido(str) | atrubutos(str) X ruta(list) X userCreador(str)
;Rec: list name(str) X extension(str) X contenido(str) | atrubutos(str) X ruta(list) X userCreador(str)
(define make-file
         (lambda (fileCompleto system)
           (list (car fileCompleto) (cadr fileCompleto) (caddr fileCompleto) (cadddr fileCompleto) (get-rutaActual system) (get-user-log system))))

;; Constructor users
;Descripción: Crea lista de usuarios
;Dom: usuarios (str)
;Rec: Lista usuarios (list)
(define (make-users name-user)
  (list name-user))

;; Constructor fecha creacion
;Descripción: Crea dato numerico unixtime
;Dom: función current-second (num)
;Rec: cantidad de segundos desde 1970 (num)
(define make-fechaCreacion (current-seconds))


;; CAPA SELECTORA

;; Selector nombre systema
;Descripción: Selecciona nombre sistema
;Dom: System (list)
;Rec: NameSystem(str)
(define get-nameSystem car)

;; Selector lista de drives
;Descripción: Selecciona lista de drives
;Dom: System (list)
;Rec: List drive
(define get-drives cadr)

;; Selector nombre drive
;Descripción: Selecciona nombre de un drive
;Dom: Drive (list)
;Rec: Name drive (str)
(define (get-name-drive listadoDrives)
   (car listadoDrives ))


;; Selector lista de usuarios ingresados
;Descripción: Selecciona lista de los usuarios registrados
;Dom: Sistema (list)
;Rec: Usuarios (list)
(define get-users caddr)


;; Selector usuario logeado
;Descripción: Selecciona usuario que se encuentra logueado
;Dom: Sistema (list)
;Rec: Usuario (str)
(define get-user-log cadddr)


;; Selector ruta actual
;Descripción: Selecciona la ruta actual del sistema
;Dom: Sistema (list)
;Rec: Ruta (list)
(define (get-rutaActual system)
  (car( cdr (cdr (cdr(cdr(reverse system)))))))

;; Selector Fecha creacion
;Descripción: Selecciona la fecha de creación del sistema
;Dom: Sistema (list)
;Rec: fecha creación (num)
(define (get-fechaCreacion system)
  (car(cdr(cdr(cdr(reverse system))))))

;; Selector directories
;Descripción: Selecciona el listado de directorios del sistema
;Dom: Sistema (list)
;Rec: directorios (list)
(define (get-directory system)
  (car(cdr(cdr(reverse system)))))

;; Selector Nombre un directorio
;Descripción: Selecciona el nombre de un directorio
;Dom: Directorio (list)
;Rec: Nombre directorio (str)
(define (get-name-directory listDirectory)
  (car listDirectory))

;; Selector primer nombre de lista directorios
;Descripción: Selecciona el primer nombre de un listado de directorios
;Dom: Directorios (list)
;Rec: Nombre primer directorio (str)
(define (get-name-Cadadirectory listDirectory)
  (car (car listDirectory)))

;; Selector Ruta directorio
;Descripción: Selecciona la ruta de un directorio
;Dom: Directorio (list)
;Rec: Ruta del directorio (list)
(define (get-ruta-directory listDirectory)
  (cadr listDirectory))

;;Selector user_creador directorio
;Descripción: Selecciona el usuario creador de un directorio
;Dom: Directorio (list)
;Rec: Usuario creador directorio (str)
(define (get-user-directory listDirectory)
  ( car (cdr (cdr listDirectory))))


;; Selector fecha creacion directory
;Descripción: Selecciona la fecha de creación de un directorio
;Dom: Directorio (list)
;Rec: Fecha creación de directorio (num)
(define (get-fechacreacion-directory listDirectory)
  ( car (cdr (cdr ( cdr listDirectory)))))

;;selector files
;Descripción: Selecciona el listado de files en el sistema
;Dom: sistema (list)
;Rec: listado de files (list)
(define (get-files system)
  (car(cdr(reverse system))))

;;Selector nombre primer files
;Descripción: Selecciona el nombre del primer file en un listado de files
;Dom: listado files(list)
;Rec: nombre primer file(str)
(define (get-name-file files)
  (car(car files)))

;;Selector extensión del primer file en listado de files
;Descripción: Selecciona la extensión del primer file en un listado de files
;Dom: listado files(list)
;Rec: extensión (str)
(define (get-extension-file file)
  (car(cdr (car file))))

;;Selector del contenido del primer file en listado de files
;Descripción: Selecciona el contenido del primer file en un listado de files
;Dom: listado files(list)
;Rec: contenido (str)
(define (get-texto-file file)
  (car(cdr(cdr (car file)))))

;;Selector atributos del primer file en listado de files
;Descripción: Selecciona los atributos del primer file en un listado de files
;Dom: listado files(list)
;Rec: atributos(str)
(define (get-atributos-file file)
  (car (cdr (cdr (reverse (car file))))))

;;Selector user creador del primer file en listado de files
;Descripción: Selecciona el user creador del primer file en un listado de files
;Dom: listado files(list)
;Rec: user creador(str)
(define (get-user-files files)
  (car (reverse (car files))))

;;Selector ruta de un file
;Descripción: Selecciona la ruta de un file
;Dom: file (list)
;Rec: ruta(list)
(define (get-ruta-files files)
  (car (cdr (reverse files))))


;;Selector nombre de un file
;Descripción: Selecciona el nombre de un file
;Dom: file (list)
;Rec: name(str)
(define (get-listname-file file)
  (car file))

;; Selector trash del sistema
;Descripción: Selecciona la lista de trash del sistema
;Dom: sistema (list)
;Rec: trash(list)
(define (get-trash system)
  (car(reverse system)))

;; Selecionar ruta de carpeta indicada
;Descripción: Selecciona la ruta de un archivo
;Dom: sistema (list) X Nombre archivo (str)
;Rec: ruta(list)
(define (get-rutaIndicada system folder)
  (car (cdr (car (filter (lambda (str) (eq? (get-name-directory str) folder)) (get-directory system))))))




;; CAPA MODIFICADORA
;; Agregar drives al sistema
;Descripción: Agrega drives al sistema
;Dom: sistema (list) X letter Drive(str) X name drive(str) X capacity drive(num)
;Rec: sistema (list)
(define (set-drives system letter nameDrive capacity)
  (make-system (get-nameSystem system)(cons (make-drives letter nameDrive capacity )(get-drives system))(get-users system)(get-user-log system)(get-rutaActual system) make-fechaCreacion (get-directory system)(get-files system)(get-trash system)))

;; Agregar users al sistema
;Descripción: Agrega users al sistema
;Dom: sistema (list) X user (str)
;Rec: sistema (list)
(define (set-users system userName)
  (make-system (get-nameSystem system)(get-drives system)(cons userName (get-users system))(get-user-log system)(get-rutaActual system) make-fechaCreacion (get-directory system)(get-files system)(get-trash system)))

;; Agregar usuario Logueado
;Descripción: Loguea a un usuario indicado
;Dom: sistema (list) X user (str)
;Rec: sistema (list)
(define (set-log system userName-log)
  (make-system (get-nameSystem system)(get-drives system)(get-users system) userName-log (get-rutaActual system)make-fechaCreacion (get-directory system)(get-files system)(get-trash system)))

;; Quitar usuario logueado
;Descripción: Desloquea al usuario
;Dom: sistema (list)
;Rec: sistema (list)
(define (set-logout system)
  (make-system (get-nameSystem system)(get-drives system)(get-users system) "" (get-rutaActual system) make-fechaCreacion (get-directory system)(get-files system)(get-trash system)))

;; Agregar unidad fijada
;Descripción: Agrega unidad a ruta actual
;Dom: sistema (list) X ruta (list)
;Rec: sistema (list)
(define (set-rutaActual system letter)
  (make-system (get-nameSystem system)(get-drives system)(get-users system) (get-user-log system) (list letter) make-fechaCreacion (get-directory system)(get-files system) (get-trash system)))

;; Crear nuevo directorio con el nombre
;Descripción: Agrega nuevo directorio
;Dom: sistema (list) X nombre directorio (str)
;Rec: sistema (list)
(define (set-directory system nameDirectory)
  (make-system (get-nameSystem system)(get-drives system)(get-users system) (get-user-log system) (get-rutaActual system) make-fechaCreacion (cons (make-directory nameDirectory (get-rutaActual system) (get-user-log system) make-fechaCreacion make-fechaCreacion) (get-directory system))(get-files system)(get-trash system)))

;; Copiar nuevo directorio
;Descripción: agrega nuevo directorio a listado directorios
;Dom: sistema (list) X directorio(list)
;Rec: sistema (list)
(define (set-neWdirectory system newDirectory)
  (make-system (get-nameSystem system)(get-drives system)(get-users system) (get-user-log system) (get-rutaActual system) make-fechaCreacion (cons newDirectory (get-directory system)) (get-files system)(get-trash system)))

;; Actualizar ruta
;Descripción: Agregar nueva ruta
;Dom: sistema (list) X ruta (list)
;Rec: sistema (list)
(define (set-newRutaActual system RutaAntigua newRuta)
  (make-system (get-nameSystem system)(get-drives system)(get-users system) (get-user-log system) (reverse (cons newRuta (reverse RutaAntigua))) make-fechaCreacion (get-directory system)(get-files system)(get-trash system)))


;; Crear nueva ruta desde cero
;Descripción: Crear nueva ruta
;Dom: sistema (list) X ruta (list)
;Rec: sistema (list)
(define (set-newRutaCompleta system newRutaCompleta )
  (make-system (get-nameSystem system)(get-drives system)(get-users system) (get-user-log system) newRutaCompleta make-fechaCreacion (get-directory system)(get-files system)(get-trash system)))

;; Agregar file al sistema
;Descripción: Agregar nuevo file al sistema
;Dom: sistema (list) X file (list)
;Rec: sistema (list)
(define (set-files system neWfile)
  (make-system (get-nameSystem system)(get-drives system)(get-users system) (get-user-log system)(get-rutaActual system) make-fechaCreacion (get-directory system)(cons neWfile ( get-files system))(get-trash system) ))

;; Agregar File al basurero
;Descripción: Agregar file a trash
;Dom: sistema (list) X file (list)
;Rec: sistema (list)
(define (set-trash system deletedFile)
  (make-system (get-nameSystem system)(get-drives system)(get-users system) (get-user-log system)(get-rutaActual system) make-fechaCreacion (get-directory system)(get-files system)(cons '(deletedFile) (get-trash system))))

;; Cambiar ruta de file
;Descripción: Cambiar ruta de File
;Dom: ruta (list) X file(list) X sistema (list)
;Rec: file (list)
(define (set-ruta-files ruta file system)
  (list (get-name-file file) (get-extension-file file) (get-texto-file file) (get-atributos-file file) ruta (get-user-log system)))

;; Cambiar ruta de directory
;Descripción: Cambiar ruta de directorio
;Dom: ruta (list) X directorio(list) X sistema (list)
;Rec: directorio (list)
(define (set-ruta-directory ruta directory system)
  (make-directory (get-name-Cadadirectory directory) ruta (get-user-log system) make-fechaCreacion make-fechaCreacion ))


;; CAPA PERTENENCIA

;; Nombre drive pertenece a listado de drives
;Descripción: indica si nombre de drive se encuentra en listado de drives
;Dom: nombre drive (str) X sistema (list)
;Rec: false | nombre drive (str)
(define (isNombreDrive? nombreDrive system)
  (member nombreDrive (map (lambda (listadoDrives) (get-name-drive listadoDrives))
                                 (get-drives system))))

;; Nombre de usuario pertenece a lista de usuarios
;Descripción: indica si nombre de usuario se encuentra en listado de ususarios
;Dom: nombre user (str) X sistema (list)
;Rec: false | nombre usuario (str)
(define (isUserList? nombreUser system)
  (member nombreUser (get-users system)))

;; Nombre de usuario se encuentra logueado?
;Descripción: indica nombre de usuario es igual al que se encuentra logueado
;Dom: nombre usuario (str) X sistema (list)
;Rec: false | true
(define (isUserLog? nombreUser system)
  (eq? nombreUser (get-user-log system)))

;; Nombre de directorio ya se encuentra ocupado?
;Descripción: indica si nombre de directorio se encuentra en listado de nombres directorios
;Dom: nombre directorio (str) X sistema (list)
;Rec: false | nombre directorio (str)
(define (isNameDirectory? nameDirectory system)
  (member nameDirectory (map (lambda (listNAmeDirectory) (get-name-directory listNAmeDirectory))
                             (get-directory system))))

;;Nombre de file ya se encuentra ocupado?
;Descripción: indica si nombre de file se encuentra en listado de nombres de file
;Dom: nombre file (str) X sistema (list)
;Rec: false | nombre file (str)
(define (isNameFile? nameFile system)
  (member nameFile (map (lambda (listNamesFiles) (get-listname-file listNamesFiles))
                             (get-files system))))


;; Existe nombre directory?
;Descripción: indica si nombre del directorio ya se encuentra en sistema
;Dom: nombre directorio (str) X listado directorios (list)
;Rec: false | true
(define (existNameDirectory? nameDirectory listDirectory)
  (cond
    [(null? listDirectory) #f]
    [ (equal? (get-name-directory (car listDirectory)) nameDirectory) #t]
    [else (existNameDirectory? nameDirectory (cdr listDirectory))]))

;; Existe nombre file?
;Descripción: indica si nombre del file ya se encuentra en sistema
;Dom: nombre file (str) X listado files (list)
;Rec: false | true
(define (existNamefile? nameFile listFile)
  (cond
    [(null? listFile) #f]
    [ (equal? (get-listname-file (car listFile)) nameFile) #t]
    [else (existNamefile? nameFile (cdr listFile))]))

;;CAPA : OTRAS FUNCIONES

;; redefinir file - trash
;Descripción: Actualiza los files y trash
;Dom: sistema (list) X files (list) X trash(list)
;Rec: sistema (list)
(define (redefine-files-trash system files trash)
  (make-system (get-nameSystem system)(get-drives system)(get-users system) (get-user-log system)(get-rutaActual system) make-fechaCreacion (get-directory system) files (cons trash (get-trash system))))


;; redefinir file
;Descripción: Actualiza los files
;Dom: sistema (list) X files (list)
;Rec: sistema (list)
(define (redefine-files system files)
  (make-system (get-nameSystem system)(get-drives system)(get-users system) (get-user-log system)(get-rutaActual system) make-fechaCreacion (get-directory system) files (get-trash system)))


;; redefinir directory - trash
;Descripción: Actualiza los directorios y trash
;Dom: sistema (list) X directorios (list) X trash(list)
;Rec: sistema (list)
(define (redefine-directory-trash system listDirectory trash)
  (make-system (get-nameSystem system)(get-drives system)(get-users system) (get-user-log system)(get-rutaActual system) make-fechaCreacion listDirectory (get-files system) (cons trash (get-trash system))))

;; redefinir directory
;Descripción: Actualiza los directorios
;Dom: sistema (list) X directorios (list)
;Rec: sistema (list)
(define (redefine-directory system listDirectory)
  (make-system (get-nameSystem system)(get-drives system)(get-users system) (get-user-log system)(get-rutaActual system) make-fechaCreacion listDirectory (get-files system) (get-trash system)))



;;Recorre lista de files en busca de un file
;Descripción: Busca el file segun su nombre y entrega file con toda la información
;Dom: files (list) X nombre file (str)
;Rec: file (list)
(define (buscar-file nombreArchivo listaFile)
      (cond
        [(null? listaFile) null]
        [(equal? (get-name-file listaFile) nombreArchivo) (cons (car listaFile)(buscar-file nombreArchivo (cdr listaFile)))]
        [else (buscar-file nombreArchivo (cdr listaFile))]))

;;Recorre lista de directorios en busca de un directorio
;Descripción: Busca el directorio segun su nombre y entrega directorio con toda la información
;Dom: directorios (list) X nombre directorio (str)
;Rec: directorio (list)
(define (buscar-directory nombreArchivo listaDirectory)
      (cond
        [(null? listaDirectory) null]
        [(equal? (get-name-Cadadirectory listaDirectory) nombreArchivo) (cons (car listaDirectory)(buscar-directory nombreArchivo (cdr listaDirectory)))]
        [else (buscar-directory nombreArchivo (cdr listaDirectory))]))
