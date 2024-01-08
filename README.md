# Haskell Entrega Final
Este repositorio contiene la entrega final realizada durante el mes 06/2022 para la asignatura Programación Funcional de la carrera Grado en Matemáticas e Informática de la Universidad Politécnica de Madrid

## Acerca del proyecto
El proyecto es un compresor y descompresor de archivos de texto utilizando el lenguaje de programación Haskell. El algoritmo utilizado es el algoritmo de Hoffman. Para más información consultar el enunciado 'Practica4.pdf'

## Información
Programacion Funcional - PRACTICA 4: Compresion de Ficheros

Fecha de entrega: 13/06/2022

Alumno: Francisco Madrigal Puertas

Matricula: b19M057

## Uso
La siguiente practica se compone de un main que lanza una pequena explicacion
del funcionamiento del algoritmo y del programa y posteriormente lo pone en ejecucion
hasta que se pulsa Ctrl+Z.

El programa pide al usuario un fichero a comprimir o descomprimir. Posteriormente, le
pregunta al usuario cual es la accion a tomar con el fichero (comprimirlo o descomprimirlo*).
Finalmente, pregunta al usuario en que fichero se desea almacenar el resultado de la compresion
y se realiza la misma.

**ATENCION:** Para descomprimir el fichero es necesario haberlo comprimido justo antes (sa que no
es lo correcto pero al introducir la informacion de descompresion en el propio fichero aumentaba 
mucho el tamano del archivo comprimido y la compresion del archivo era nulo o incluso pesaba mas 
el fichero "comprimido" que el original).

## Código
Boot: Aqui se encuentra el codigo de arranque de la interfaz del programa. Esta formada por las 
funciones main, cabecera, programa y los fallos del programa.

Code: Aqui se encuentran las funciones que realizan el algoritmo de compresion. Para comprenderlo
de forma mas sencilla lo he dividido en las siguientes subsecciones: Iteration list, Translation tree, Translation list, Translation

# English Version
## About the Project
The project is a text file compressor and decompressor using the Haskell programming language. The algorithm utilized is the Huffman algorithm.

## Information
Functional Programming - PRACTICE 4: File Compression

Deadline: 06/13/2022

Student: Francisco Madrigal Puertas

ID: b19M057

## Usage
This practice consists of a main function that launches a brief explanation of the algorithm's operation and the program itself. Subsequently, it runs until Ctrl+Z is pressed.

The program prompts the user for a file to compress or decompress. Afterward, it asks the user which action to take with the file (compress or decompress*). Finally, it asks the user in which file they want to store the result of the compression, and it performs the same.

**ATTENTION:** To decompress the file, it is necessary to have compressed it just before (knowing it is not the correct approach, but including the decompression information in the file itself significantly increased the size of the compressed file, rendering the file either uncompressed or sometimes even larger than the original "compressed" file).

## Code
Boot: Here lies the code for booting up the program's interface. It consists of the main, header, program, and program faults functions.

Code: This section encompasses functions responsible for the compression algorithm. For easier comprehension, I have divided it into the following subsections: Iteration list, Translation tree, Translation list, Translation.
