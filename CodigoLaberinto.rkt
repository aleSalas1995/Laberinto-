#lang racket
;Luis Alejandro Salas Rojas
;2014010742

;packages requeridos 
(require graph math/matrix)
(require typed-stack)

(define-struct nodo (Nombre Sequence)#:mutable #:transparent);Struct de nodo
(define-struct arista (IdNodo1 IdNodo2 Peso));Struct de la aristas
(define Nodos (list));Lista nodos 
(define Aristas (list));Lista Aristas
(define pila (make-stack));Define la pila con su nombre "pila"


;Matriz usada de ejemplo 
;3 perro y 2 hueso
(define A(matrix ([ 0 1 1 1 0 1 1 2 0 1]
                  [ 0 0 1 0 0 1 0 0 0 1]
                  [ 0 1 1 0 1 1 1 1 1 1]
                  [ 0 0 0 0 0 1 0 0 0 0]
                  [ 1 1 1 0 1 1 1 1 1 1]
                  [ 1 0 0 0 1 0 0 0 1 0]
                  [ 1 1 1 1 1 0 1 0 1 1]
                  [ 0 0 0 0 1 0 1 0 1 0]
                  [ 1 1 1 1 3 1 1 0 1 1]
                  )))

;Ciclo de camino
(define (ciclo i j)
  (define raiz (string-append (~v i) (~v j)))
  (set! Nodos (list));Setea lo que esta en list a Nodos
  (define node(make-nodo (string-append (~v i) (~v j)) '()));Se crea un nodo con la posicion i j unidas en un string
  (set! Aristas (list));Setea lo que esta en list a Aristas
  (set! pila (make-stack (list i j)));Le mete a la pila el i j como lista
  (set! Nodos (append Nodos (list node)));Le setea a Nodos el nuevo node
  (cicloInicial)
  (define x (car(top pila)));Toma el x de el tope de la pila
  (define y (last(top pila)));Toma el y del tope de la pila

  (define s(string-append (~v  x) (~v y)));Define s como un string de xy
  
  (reverse (camino (string-append (~v  x) (~v y)) (list s) raiz) );Se devuelve en el camino buscando los padres hasta llegar al nodo raiz
  )


;Este if lo que hace es preguntar si el tope de la pila es 2, sino llama a validaciones 
(define (cicloInicial)
  (if (equal? 2 (matrix-ref A (car(top pila))(last(top pila)))) pila
      (validaciones))
  )

(define (validaciones)
  (define x (car(top pila)));Define el x como el primer numero del primer elemento de la pila
  (define y (last(top pila)));Define el y como el primer numero del segundo elemento de la pila
  (define pilaNueva (pop pila));Le asigna el tope de la pila a pilaNueva
  (set! pila pilaNueva);Setea la pila actualizada 
  (if (>= (-(length (matrix-cols A))1) (+ y 1))(hayConexion  x (+ y 1) x y)  #f);Valida que la columna no se salga al mover a la derecha y sino lo hace llama la funcion de hayConexion
  (if (<= 0 (- x 1)) (hayConexion (- x 1) y x y) #f);Valida que la fila no sea menor que 0 y si se cumple llama la funcion hayConexion
  (if (>= (-(length (matrix-rows A))1) (+ x 1)) (hayConexion (+ x 1) y x y) #f);Valida que al mover a la derecha no se salga y llama a hayConexion
  (if (<= 0 (- y 1))(hayConexion x (- y 1) x y) #f );Valida que la columna al mover a la izquierda no sea menor que 0
  (cicloInicial)
  )

;Recibe la posicion el x y actual y el x y que se va mover
;Verifica que si la posicion a la que se va mover es 1 o 2 y llama a la funcion agregar
(define (hayConexion nuevoI nuevoJ i j)
  (if (or (equal? 2 (matrix-ref A nuevoI nuevoJ))(equal? 1 (matrix-ref A nuevoI nuevoJ))) (agregar nuevoI nuevoJ i j)  
      #f) 
  )

;Recibe la posicion el x y actual y el x y que se va mover
;llama a buscarNodo con la posicion actual y llama la funcion de agregarAux
(define (agregar i j xP yP)
  (if (empty? (buscarNodo (string-append (~v i) (~v j)))) (agregarAux i j xP yP);si esta vacio llama a la funcion de agregarAux
      #f)
  )

;Recibe un i j de padre e i j del hijo
(define (agregarAux i j xP yP)
  (define node(make-nodo (string-append (~v i) (~v j)) '()));Crea el nodo con el i j y una lista de hijos vacia
  (define edge(make-arista (string-append (~v xP) (~v yP)) (string-append (~v i) (~v j)) (matrix-ref A i j)));Crea la arista con el ij padre, ij hijo, peso del hijo
  (set! Nodos (append Nodos (list node)));Setea a Nodos el node nuevo
  (set! Aristas (append Aristas (list edge)));Setea a Aristas el edge(arista) nuevo
  ;Actualiza la lista de hijos del nodo padre
  (set-nodo-Sequence! (car(buscarNodo (string-append (~v xP) (~v yP)))) (append (nodo-Sequence(car(buscarNodo (string-append (~v xP) (~v yP)))))(list(string-append (~v i) (~v j)))))
  (define pilaNueva (push pila (list i j)));le asigna a pilaNueva la pila con el elemento nuevo
  (set! pila pilaNueva);Asigna pilaNueva a pila
  )

;Busca un nodo determinado a partir del id del nodo.
(define (buscarNodo idNodo)
    (filter (lambda(x) (equal? (nodo-Nombre x) idNodo)) Nodos))

;Busca el nodo padre apartir del id del nodo
(define (buscarNodoPadreAux idNodo)
    (buscarNodo (arista-IdNodo1 (first(filter (lambda(x) (equal? (arista-IdNodo2 x) idNodo)) Aristas)))))

;Busca los nodos padres hasta devolverse al nodo raiz
(define (camino id lista raiz )
  (if (equal? raiz (nodo-Nombre(car(buscarNodoPadreAux id)))) (append lista (list raiz))  
      (camino (nodo-Nombre(car(buscarNodoPadreAux id))) (append lista (list(nodo-Nombre(car(buscarNodoPadreAux id)))) ) raiz))

  )

;Imprime la lista de nodos del camino con sus hijos
(define (arbol)
  Nodos)
