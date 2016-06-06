# huffman-code
La codificación de Huffman es un algoritmo para la compresión de datos.

No se pudo encontrar una manera de manipular bits para que comprimiera de
verdad el archivo.

En lugar de eso nuestro proyecto regresa un archivo con un string que 
representa los bits necesearios para la compresión del archivo

Requerimientos para compilar:
    *) Tener el modulo Std
    *) Tener el modulo Printf
    *) Tener instalado make

Compilar:
    Desde el carpeta scr en la terminal escribir el comando "make" 

Se genera un ejecutable llamada "huff", a continuación se indica el metodo de uso

$./huff [-c|-d] [archive|archive.hff] (archive.tree)

Se necesita incluir la bandera "-c" si se quiere comprimir o "-d" para descomprmir.
Para comprimir se requiere que se le pase el nombre del archivo a comprimir.
Para descomprimir se necesita que se pase un archivo .hff y un archivo .tree

Ejemplos:

Comprimir:
(Desde la carpeta src existe el archivo example)

[rtaboada@cpu]$./huff -c example

Descomprimir
(Desde la carpeta src existen los archivos example.hff y example.tree)

[rtaboada@cpu]$./huff -d example.hff example.tree

A notar: Cuando se descomprime se eliminan los archivos .hff y .tree utilizados.

Para cualquier duda contarme.
atte

Ricardo Taboada (taboada@ciencias.unam.mx)

