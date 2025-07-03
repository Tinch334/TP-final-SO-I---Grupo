# TP-final-SO-I---Grupo
Repositorio del grupo 4 y 20 para el trabajo practico final de Sistemas Operativos 1. Integrantes: Juan Ignacio Bertoni, Goñi Martín, Tomas Segovia, Lucas Wasserstrom.

## Instrucciones de uso

Para correr el projecto primero hacer `make` en el directorio con los archivos, luego `erl -noshell -s node init -s init stop`. Para poder interactuar con la consola se debe esperar a que el nodo consiga un nombre.
Las carpetas necesarias se crean automaticamente, no es necesario crearlas.

## Comandos CLI
* `HELP`:
   Muestra una lista de todos los comandos de la CLI
* `EXIT`:
   Cierra el programa
* `NODE_ID`:
   Muestra el ID del nodo
* `LIST_FILES`:
   Devuelve una lista con los nombres de los archivos en la carpeta compartida
* `SEARCH_REQUEST`:
  Recive un argumento que va a ser el archivo a buscar, para luego mostrar los nodos de la red que tengan ese archivo (admite wildcards)
* `DOWNLOAD_REQUEST`:
  Recive el nombre de un archivo y el nombre de un nodo, luego descarga ese archivo directamente de ese nodo
* `LIST_NODES`:
  Devuelve una lista con los nombres de todos los nodos registrados
