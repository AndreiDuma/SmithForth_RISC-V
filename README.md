This is an accessible Forth implementation written directly in machine
code for the RISC-V architecture.  It is an adaptation of David
Smith's excellent x86-64 [SmithForth](https://dacvs.neocities.org/SF/).

Run with `make run`.  You will need to run Linux either on RISC-V
hardware or in a QEMU environment.

This project was developed as part of my Master's [thesis](https://github.com/AndreiDuma/SmithForth_RISC-V/releases/download/v1.0/From_x86-64_Forth_to_RISC-V_Andrei_Dorian_Duma_2024.pdf), in
which I thoroughly annotate SmithForth's machine code before porting
it to RISC-V.  The abstract of my thesis is given below:

> In this thesis we present the implementation of a usable Forth
> system, built using only RISC-V machine code and the Linux operating
> system as foundations. We begin by justifying the need for
> accessible programming language implementations, discussing
> desirable features in educational compilers. Having selected Forth
> as our language of choice for an educational language
> implementation, we review existing Forth systems and we motivate why
> creating a RISC-V port is a worthwhile task. Next we thoroughly
> examine SmithForth, a high-quality Forth system for the x86-64
> architecture. After understanding its principles, we port it to
> RISC-V, adapting it to our purposes. Finally, we extend this Forth
> system in Forth itself: we write a RISC- V assembler, we provide
> useful arithmetic and logic operators plus conditional and looping
> constructs. We complete our demonstration with a Forth
> implementation of FizzBuzz, showing the usability of the system.

Those interested can [download](https://github.com/AndreiDuma/SmithForth_RISC-V/releases/download/v1.0/From_x86-64_Forth_to_RISC-V_Andrei_Dorian_Duma_2024.pdf) the thesis document in PDF format.
