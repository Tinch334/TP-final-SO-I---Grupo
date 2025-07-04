# TP-final-SO-I---Grupo
Repositorio del grupo 4 y 20 para el trabajo practico final de Sistemas Operativos 1. Integrantes: Juan Ignacio Bertoni, Goñi Martín, Tomas Segovia, Lucas Wasserstrom.

## Instrucciones de uso

Para correr el projecto primero hacer `make` en el directorio con los archivos, luego `erl -noshell -s node init -s init stop`. Este comando indica que se ejecute erlang y sin abrir el entorno shell, se ejecute la función `init` del módulo `node`, y al terminar lo anterior, se provoque un apagado limpio de la VM llamando a `stop` del módulo `init` (nativo de Erlang).
 
Para poder interactuar con la consola se debe esperar a que el nodo consiga un nombre. Las carpetas y archivos necesarios son creados automaticamente, no es necesario crearlas.

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
  Recibe un argumento que va a ser el archivo a buscar, para luego mostrar los nodos de la red que tengan ese archivo (admite wildcards)
* `DOWNLOAD_REQUEST`:
  Recibe el nombre de un archivo y el nombre de un nodo, luego descarga ese archivo directamente de ese nodo
* `LIST_NODES`:
  Devuelve una lista con los nombres de todos los nodos registrados
* `REFRESH`:
  Recargar los contenidos del archivo de registro de nodos. Así evitamos tener registrados peers que ya se fueron de la red.
* `IP_MAP <IpAddr>`:
  Devuelve todos los nodos vinculados a una misma dirección IP. Útil si un mismo peer tiene muchas instancias del programa corriendo.
