module Main (main) where
import System.Environment
import System.IO
import System.Directory

{----------------------------------------------------------------------------------------------------------
    Programacion Funcional - PRACTICA 4: Compresion de Ficheros
    Fecha de entrega: 13/06/2022
    Alumno: Francisco Madrigal Puertas
    Matricula: b19M057

    Uso:
    La siguiente practica se compone de un main que lanza una pequena explicacion
    del funcionamiento del algoritmo y del programa y posteriormente lo pone en ejecucion
    hasta que se pulsa Ctrl+Z.
    El programa pide al usuario un fichero a comprimir o descomprimir. Posteriormente, le
    pregunta al usuario cual es la accion a tomar con el fichero (comprimirlo o descomprimirlo*).
    Finalmente, pregunta al usuario en que fichero se desea almacenar el resultado de la compresion
    y se realiza la misma.

    * ATENCION: Para descomprimir el fichero es necesario haberlo comprimido justo antes (sa que no
    es lo correcto pero al introducir la informacion de descompresion en el propio fichero aumentaba 
    mucho el tamano del archivo comprimido y la compresion del archivo era nulo o incluso pesaba mas 
    el fichero "comprimido" que el original).

    La estructura del codigo es la siguiente:
        - Boot: Aqui se encuentra el codigo de arranque de la interfaz del programa. Esta formada por las 
        funciones main, cabecera, programa y los fallos del programa.
        - Code: Aqui se encuentran las funciones que realizan el algoritmo de compresion. Para comprenderlo
        de forma mas sencilla lo he dividido en las siguientes subsecciones:
            + 
--------------------------------------------------------------------------------------------------------}

       ----------------------------------    BOOT    ----------------------------------------

main = do
    cabecera
    programa Vacio


cabecera = do
    putStrLn "\n\n---------------------------------------------------------------"
    putStrLn "  Programacion Funcional - PRaCTICA 4: Compresion de ficheros  "
    putStrLn "            Francisco Madrigal Puertas - b19M057               "
    putStrLn "                           2022                               "
    putStrLn "---------------------------------------------------------------\n"
    putStrLn "Bienvenido al sistema de compresion/descompresion realizado para\nla asignatura de Programacion Funcional. Este sistema funciona  \nmediante el algoritmo de Hoffman. El funcionamiento es el \nsiguiente: \n"
    putStrLn "   - Primero se genera un arbol binario donde las hojas mas     \n     cercanas a la raiz son las las hojas correspondientes a los\n    caracteres del texto a comprimir con mas iteraciones y las \n     mas lejanas corresponden a los caracteres con menos iteraciones.\n"
    putStrLn "   - Despuas se le asignan 1 a los hijos izquierdos y un 0 a los\n     hijos derechos. El codigo generado al recorrer el camino   \n"
    putStrLn "   - Por ultimo, se sustituyen los caracteres por sus codigos   \n     o los codigos por sus caracteres (segun la tarea a realizar).\n"
    putStrLn "El programa, mientras esta activo, guardara un arbol de traduccion\nusado para la posterior descompresion del fichero previamente   \ncomprimido. Por tanto, es necesario la compresion previa de un  \nfichero antes de descomprimirlo (ya que al comprimirlo, el propio \nfichero no almacen la informacion para la posterior traduccion). \n"
    putStrLn "Para salir del programa, escribir Ctrl+Z y posteriormente pulsar\nla tecla Enter. \n"
    

programa arbol = do
    putStrLn "---------------------------------------------------------------"
    putStrLn "Introduzca el fichero a comprimir/descomprimir"
    putStrLn "---------------------------------------------------------------"
    fileName <- getLine
    putStrLn "---------------------------------------------------------------"
    fileExists <- doesFileExist fileName

    if fileExists
        then do content <- readFile fileName     
                putStrLn "Introduzca la accion a realizar (comprimir/c, descomprimir/d)"
                putStrLn "---------------------------------------------------------------"
                option <- getLine
                putStrLn "---------------------------------------------------------------"
                (if option == "c"
                    then do
                        putStrLn "Introduzca el nombre que tendra el fichero comprimido."
                        putStrLn "---------------------------------------------------------------"
                        newFile <- getLine
                        putStrLn "---------------------------------------------------------------"
                        let newContents = comprimirHex content
                        writeFile newFile newContents
                        let arbol = treeList $ tree $ listaIteraciones content
                        putStrLn "Fichero comprimido con exito."
                        putStrLn "---------------------------------------------------------------"
                        programa arbol
                        
                    else (if option == "d"
                        then do (if arbol == Vacio
                                then do fallo3
                                else do 
                                putStrLn "Introduzca el nombre que tendra el fichero descomprimido."
                                putStrLn "---------------------------------------------------------------"
                                newFile <- getLine
                                putStrLn "---------------------------------------------------------------"
                                let newContents = descomprimeBin arbol $ obtenerOriginal $ hexToBitList content
                                writeFile newFile newContents
                                putStrLn "Fichero descomprimido con exito."
                                putStrLn "---------------------------------------------------------------"
                                programa Vacio)
                        else do fallo2)) 
        else fallo1


fallo1 = do putStrLn "ATENCIoN: El fichero introducido no existe o no se ha podido encontrar. \nIntroduzca un fichero valido o pulse Ctrl+Z + Enter para matar el proceso."
            programa Vacio


fallo2 = do putStrLn "ATENCIoN: Opcion no valida. \nIntroduzca de nuevo todos los datos. "
            programa Vacio


fallo3 = do putStrLn "ATENCIoN: No se ha comprimido el archivo anteriormente. \nPruebe a comprimir un archivo y seguidamente pruebe a descomprimirlo."
            programa Vacio



       ----------------------------------    CODE    ----------------------------------------
                    -----------------  ITERATION LIST  -------------------- 

{- 
    Funcion que dado una cadena nos devuelve una lista de tuplas de tipo [(Char,Int)] en la que
    cada Char es un caracter que aparece en la cadena dada como parametro y el entero es el 
    numero de iteraciones del caracter en la cadena. La lista esta ordenada de menor a mayor
    en funcion del segundo elemento de la tupla.

    PRE: Un String del que queremos obtener las iteraciones de cada caracter
    POST: Una lista de tuplas de la forma [(Char,Int)]
-}
listaIteraciones :: String -> [(Char, Int)]
listaIteraciones s = ordena [(x, ocurrencias x s) | x <- (letras s)]


{- 
    Funcion que dado una cadena nos devuelve una lista con todos los caracteres que aparecen
    en la cadena.

    PRE: Un String del que queremos obtener los caracteres que lo componen
    POST: Una lista de caracteres [Char]
-}
letras :: String -> [Char]
letras s = letras' s [] 


{- 
    Funcion que dado una cadena nos devuelve una lista con todos los caracteres que aparecen
    en la cadena (auxiliar).

    PRE: Un String del que queremos obtener los caracteres que lo componen y una lista auxiliar
    donde vamos introduciendo los caracteres
    POST: Una lista de caracteres [Char]
-}
letras' :: String -> [Char] -> [Char]
letras' [] r = r
letras' s@(x:xs) r = if x `elem` xs
                    then letras' xs r
                    else letras' xs [x]++r


{- 
    Funcion que dado un caracter y una cadena, nos dice las veces que aparece dicho caracter
    en la cadena.

    PRE: Un caracter y un String del que queremos saber las iteraciones del primero en el segundo
    POST: Un entero con las ocurrencias del caracter en la cadena 
-}
ocurrencias :: Char -> String -> Int
ocurrencias c s = length [a | a <- s, a == c]


{- 
    Funcion que dada una lista de tipo [(Char,Int)] devuelve la misma lista pero ordenada
    de menor a mayor.

    PRE: Una lista de tipo [(Char,Int)] a ordenar
    POST: una lista de tipo [(Char,Int)] ordenada de menor a mayor
-}
ordena:: [(Char, Int)] -> [(Char, Int)]
ordena [] = []
ordena (x:xs) = (ordena menores) ++ [x] ++ (ordena mayores)
                where
                mayores = [a | a <- xs, snd a > snd x]
                menores = [a | a <- xs, snd a <= snd x]



                    -----------------  TRANSLATION TREE  -------------------- 

-- Tipo arbol
data Tree a = Vacio | Nodo (Tree a) a (Tree a) deriving (Eq,Ord,Show)

{- 
    Funcion que dado un arbol de tuplas (Char,Int) devuelve el segundo elemento de la tupla
    cuando el nodo tiene hijos. En caso de ser una hoja devuelve 0.

    PRE: Un arbol de tipo (Char, Integer)
    POST:   0 si la raiz es vacia
            El segundo elemento de la tupla raiz si la raiz es de tipo (Char, Integer)
-}
getValueTree :: Tree (Char, Int) -> Int
getValueTree Vacio = 0
getValueTree (Nodo hi raiz hd) = snd raiz


{- 
    Funcion que dada una lista de tipo [(Char,Int)] con los caracteres y las ocurrencias
    de un texto devuelve una lista de tipo [Tree (Char,Int)] donde cada elemento de la lista
    inicial son raices del arbol.

    PRE: Una lista de iteraciones de tipo [(Char, Int)]
    POST: Una lista de arboles de unicamente un elemento de tipo 
    (Char, Int). Es decir, donde todos son raices
-}
tree :: [(Char, Int)] -> [Tree (Char, Int)]
tree [] = []
tree l@(x:xs) = (Nodo Vacio x Vacio) : tree xs


{- 
    Funcion que dada una lista de tipo [Tree (Char,Int)] devuelve la misma lista pero con 
    los elementos ordenados en funcion del valor del segundo elemento de la tupla de la raiz
    del arbol.

    PRE: Una lista de arboles de tipo (Char, Int)
    POST: Una lista de arboles ordenada en funcion del tamano de su raiz
-}
ordenaTree:: [Tree (Char, Int)] -> [Tree (Char, Int)]
ordenaTree [] = []
ordenaTree (x:xs) = (ordenaTree menores) ++ [x] ++ (ordenaTree mayores)
                where
                mayores = [a | a <- xs, getValueTree a > getValueTree x ]
                menores = [a | a <- xs, getValueTree a <= getValueTree x]


{- 
    Funcion que dada una lista de tipo [Tree (Char,Int)] une todos los elementos de esa lista
    en un unico arbol, generando asi un arbol de tipo Hoffman.

    PRE: Una lista de arboles de tipo (Char, Int)
    POST: Un arbol de compresion de Hoffman. Donde:
        - Las fusiones de dos nodos se representa con el caracter NULL
        - Se cumplen las propiedades para hacer un arbol de Hoffman (
            que todos los caracteres sean hojas)
-}
treeList :: [Tree (Char, Int)] -> Tree(Char, Int)
treeList [x] = x
treeList l@(x1:x2:xs) = treeList (ordenaTree ((Nodo x1 ('\0', getValueTree x1 + getValueTree x2) x2) : xs))


{- 
    Funcion que dada un arbol de Hoffman, de tipo Tree (Char, Int), modifica el segundo elemento
    de la tupla por un String donde se almacena el codigo por el que se traduce cada caracter en
    binario.

    PRE: Un arbol de tipo (Char, Int)
    POST: Un arbol de tipo (Char, String) donde String es el valor
    para el caracter Char en el algoritmo de Hoffman (e.j. ('c', "1001"))
-}
codigo :: Tree (Char, Int) -> Tree (Char, String)
codigo (Nodo hi raiz hd) = Nodo (codigo' hi "1") (fst raiz, "") (codigo' hd "0")


{- 
    Funcion que dada un arbol de Hoffman, de tipo Tree (Char, Int), modifica el segundo elemento
    de la tupla por un String donde se almacena el codigo por el que se traduce cada caracter en
    binario. Auxiliar.

    PRE: Un arbol de tipo (Char, Int) y un entero que sirve para almacenar el camino 
    POST: Un arbol de tipo (Char, String) donde String es el valor
    para el caracter Char en el algoritmo de Hoffman (e.j. ('c', "1001"))
-}
codigo' :: Tree (Char, Int) -> String -> Tree (Char, String)
codigo' Vacio _ = Vacio
codigo' (Nodo hi raiz hd) cadena = Nodo (codigo' hi $ cadena++"1") (fst raiz, cadena) (codigo' hd $ cadena++"0")


{- 
    Funcion que pasa un arbol de cualquier tipo a una lista con todos los nodos del arbol.

    PRE: Un arbol de cualquier tipo
    POST: Una lista con los elementos del arbol "ordenados" por su
    forma en el arbol
-}
arbolALista :: Tree a -> [a]
arbolALista Vacio = []
arbolALista (Nodo hi raiz hd) = (arbolALista hi) ++ [raiz] ++ (arbolALista hd)


                    -----------------  TRANSLATION LIST  -------------------- 


{- 
    Funcion que dada una lista de tipo [(Char,String)] con los caracteres y sus respectivos codigos
    de traduccion, devuelve la misma lista pero eliminando las posibles tuplas con caracter nulo, que 
    usabamos a la hora de realizar el arbol para representar las ramas que habian sido unidas.

    PRE: Una lista de tipo (Char, String)
    POST: La misma lista dada por parametro sin los elementos "fusion",
    ese decir, los elementos resultantes de fusionar dos ramas (que se 
    representaban con el caracter NULL)
-}
quitarNull :: [(Char, String)] -> [(Char, String)]
quitarNull l = filter (\(x,y) -> x /= '\0')  l


{- 
    Funcion que dado una cadena nos devuelve la lista de traduccion.

    PRE: Un arbol de cualquier tipo
    POST: Una lista con los elementos del arbol "ordenados" por su
    forma en el arbol
-}
listaCodigo :: String -> [(Char, String)]
listaCodigo =  quitarNull . arbolALista . codigo . treeList . tree . listaIteraciones 


                    -------------------  TRANSLATION   ---------------------- 


{- 
    Funcion que dada una cadena y una lista de traduccion devuelve una cadena traducida en binario

    PRE: Un string a traducir y una lista de traduccion (la lista con las conversiones a binario)
    POST: Un string en binario traducido
-}
translateString :: String -> [(Char, String)] -> String
translateString s l = concat [translateChar c l | c <- s]


{- 
    Funcion que dado un caracter y una lista de traduccion devuelve una cadena con el caracter traducido
    en binario.

    PRE: Un char a traducir y su lista de traduccion
    POST: Un string con el char traducido a binario con nuestro algoritmo de Huffman
-}
translateChar :: Char -> [(Char, String)] -> String
translateChar c ((x,y):xs) 
    | x == c = y
    | otherwise = translateChar c xs


{- 
    Funcion que dado un String en binario previamente traducido introduce la informacion
    de los bits que se han tenido que anadir para que el texto en binario se pueda escribir
    en hexadecimal.

    PRE: Un texto en binario
    POST: Un texto en binario donde se anaden bits para poder traducir de binario a byte
    donde su ultimo byte es un byte que expresa los bits anadidos:
        - Si es de la forma 1xxx1111 es una cadena donde se han anadido xxx bits (xxx es un
        numero binario entre 0-7)
        - Si es de la forma 00000000 no se ha anadido ningun bit
    La cadena se llena de 0 al final de la misma hasta llenar el byte (en caso necesario)
-}
shiftBits :: String -> String
shiftBits cadena
    | mod (length cadena) 8 == 1 = cadena ++ "000000011111111"
    | mod (length cadena) 8 == 2 = cadena ++ "00000011101111"
    | mod (length cadena) 8 == 3 = cadena ++ "0000011011111"
    | mod (length cadena) 8 == 4 = cadena ++ "000011001111"
    | mod (length cadena) 8 == 5 = cadena ++ "00010111111"
    | mod (length cadena) 8 == 6 = cadena ++ "0010101111"
    | mod (length cadena) 8 == 7 = cadena ++ "010011111"
    | mod (length cadena) 8 == 0 = cadena ++ "00000000"


{- 
    Funcion que dado un String que contiene el ultimo byte de un texto en binario previamente
    tratado (es decir, se le ha anadido ya el ultimo byte de informacion) devuelve la cantidad de 
    0s que han sido anadidos al texto original en binario.

    PRE: Un byte en binario
    POST: El numero de 0 anadidos para rellenar el ultimo byte
-}
shiftBitsInversa :: String -> Int
shiftBitsInversa cadena
    | cadena == "00000000" = 0
    | cadena == "10011111" = 1
    | cadena == "10101111" = 2
    | cadena == "10111111" = 3
    | cadena == "11001111" = 4
    | cadena == "11011111" = 5
    | cadena == "11101111" = 6
    | cadena == "11111111" = 7


{- 
    Funcion que dado cuatro bits devuelve el Nibble correspondiente en hexadecimal

    PRE: Una cadena de 4 bits
    POST: Un Nibble en hex, traduccion de los bits dados
-}
bitToNibble :: String -> String
bitToNibble bit 
    | bit == "0000" = "0"
    | bit == "0001" = "1"
    | bit == "0010" = "2"
    | bit == "0011" = "3"
    | bit == "0100" = "4"
    | bit == "0101" = "5"
    | bit == "0110" = "6"
    | bit == "0111" = "7"
    | bit == "1000" = "8"
    | bit == "1001" = "9"
    | bit == "1010" = "A"
    | bit == "1011" = "B"
    | bit == "1100" = "C"
    | bit == "1101" = "D"
    | bit == "1110" = "E"
    | bit == "1111" = "F"
    | otherwise = ""


{- 
    Funcion que dado un Nibble en hex devuelve una cadena de cuatro bits con la 
    traduccion del primero.

    PRE: Un Nibble
    POST: El Nibble dado por parametro en binario
-}
nibbleToBit :: Char -> String
nibbleToBit nibble 
    | nibble == '0' = "0000"
    | nibble == '1' = "0001"
    | nibble == '2' = "0010"
    | nibble == '3' = "0011"
    | nibble == '4' = "0100"
    | nibble == '5' = "0101"
    | nibble == '6' = "0110"
    | nibble == '7' = "0111"
    | nibble == '8' = "1000"
    | nibble == '9' = "1001"
    | nibble == 'A' = "1010"
    | nibble == 'B' = "1011"
    | nibble == 'C' = "1100"
    | nibble == 'D' = "1101"
    | nibble == 'E' = "1110"
    | nibble == 'F' = "1111"
    | otherwise = ""


{- 
    Funcion que dada una cadena traducida ya a binario la traduce a hexadecimal.

    PRE: Un string en binario a traducir en hexadecimal
    POST: El string dado en hexadecimal
-}
bitToHexList :: String -> String
bitToHexList "" = ""
bitToHexList s = (bitToNibble $ take 4 s) ++ (bitToHexList $ drop 4 s)


{- 
    Funcion que dada una cadena en hexadecimal la traduce a binario.

    PRE: Un string en hexadecimal a traducir en binario
    POST: El string dado en binario
-}
hexToBitList :: String -> String
hexToBitList "" = ""
hexToBitList s@(x:xs) = (nibbleToBit x) ++ hexToBitList xs


{- 
    Funcion que dada una cadena traducida ya a binario devuelve la cadena en binario original,
    es decir, sin el ultimo byte de informacion.

    PRE: Un string en binario con los bits de informacion finales
    POST: El string en binario sin el ultimo byte de informacion
-}
obtenerOriginal :: String -> String
obtenerOriginal s = take (length s - (8 + (shiftBitsInversa $ reverse $ take 8 $ reverse s))) s
 

{- 
    Funcion que dada una cadena, la traduce a binario

    PRE: Un string a traducir
    POST: El string en binario
-}
comprimirBinario :: String -> String
comprimirBinario s = shiftBits (translateString s (listaCodigo s))


{- 
    Funcion que dada una cadena, la traduce a hexadecimal

    PRE: Un string a traducir
    POST: El string en hexadecimal
-}
comprimirHex :: String -> String
comprimirHex s = bitToHexList (comprimirBinario s)


{- 
    Funcion que dada un arbol de traduccion de tipo Tree (Char,Int) y una cadena
    traducida a binario (sin el ultimo byte de informacion) devuelve el texto original

    PRE: Un arbol de traduccion de tipo Tree (Char,Int) y una cadena en binario (sin el byte
    de informacion)
    POST: La cadena original
-}
descomprimeBin :: Tree (Char,Int) -> String -> String
descomprimeBin a cadena =  if null cadena 
                        then [] 
                        else descomprimeBin' a a cadena

                        
{-
    Funcion auxiliar a descomprimeBin
-}
descomprimeBin' :: Tree(Char,Int) -> Tree(Char,Int) -> String -> String
descomprimeBin' a (Nodo Vacio (caracter,valor) Vacio) cadena = caracter:(descomprimeBin a cadena)
descomprimeBin' a (Nodo hi c hd) ('1':cadena) = descomprimeBin' a hi cadena
descomprimeBin' a (Nodo hi c hd) ('0':cadena) = descomprimeBin' a hd cadena











