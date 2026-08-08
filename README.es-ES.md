

Esta es una implementación de Forth accesible escrita directamente en código máquina para la arquitectura RISC-V. Es una adaptación del excelente [SmithForth](https://dacvs.neocities.org/SF/) x86-64 de David Smith.

Ejecútelo con `make run`. Necesitarás ejecutar Linux ya sea en hardware RISC-V o en un entorno QEMU.

Este proyecto se desarrolló como parte de mi tesis de máster [tesis](https://github.com/AndreiDuma/SmithForth_RISC-V/releases/download/v1.0/From_x86-64_Forth_to_RISC-V_Andrei_Dorian_Duma_2024.pdf), en la que anoto exhaustivamente el código máquina de SmithForth antes de portarlo a RISC-V. El resumen de mi tesis se presenta a continuación:

> En esta tesis presentamos la implementación de un sistema Forth utilizable, construido utilizando únicamente código máquina RISC-V y el sistema operativo Linux como bases. Comenzamos justificando la necesidad de implementaciones de lenguajes de programación accesibles, discutiendo las características deseables en los compiladores educativos. Tras seleccionar Forth como nuestro lenguaje de elección para una implementación de lenguaje educativo, revisamos los sistemas Forth existentes y motivamos por qué crear un port para RISC-V es una tarea que vale la pena. A continuación, examinamos a fondo SmithForth, un sistema Forth de alta calidad para la arquitectura x86-64. Tras comprender sus principios, lo portamos a RISC-V, adaptándolo a nuestros fines. Finalmente, extendemos este sistema Forth en el propio Forth: escribimos un ensamblador RISC- V, proporcionamos operadores aritméticos y lógicos útiles, además de estructuras condicionales y de bucle. Completamos nuestra demostración con una implementación de FizzBuzz en Forth, mostrando la usabilidad del sistema.

Quienes estén interesados pueden [descargar](https://github.com/AndreiDuma/SmithForth_RISC-V/releases/download/v1.0/From_x86-64_Forth_to_RISC-V_Andrei_Dorian_Duma_2024.pdf) el documento de la tesis en formato PDF.
