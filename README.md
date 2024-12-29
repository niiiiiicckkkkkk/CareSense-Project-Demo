#  3364Mach

Hi, I'm Sebastian. This is my project 3364Mach, a software simulation of the microprocessor I used in a class called Laboratory Electronics.


## Background

My last semester of college I took a Physics class spending 3 hours in lab twice a week building little circuits. The culmination of all our labs was a microprocessor with a custom instruction set specified in Verilog. As I worked through the course, I noticed many of my classmates without a CS background struggle with digital logic. Eventually I would like to provide this simulation software as a learning tool for the class allowing students to better understand how data flows through a clocked system.

** This project is also a sort of living embodiment of the work I've done since graduation exploring the functional programming paradigm through Haskell. I've found the journey to be a very fun one and hope to share some of that excitement through my code :)

## Module Breakdown

The project consists of 4 large components all available in the src directory

- module Binary defines the abstraction for physical wires and signals. 
- module Parser is responsible for reading in assembly files such as those in the "asm" directory and converting to the ASM datatype
- module Loader resolves labels and assigns memory locations to the parsed assembly directives
- module Simulator defines state transitions for the mock processor

The entry point is located in app/Main.hs.

## Running The Project
I added a short Dockerfile in case it's helpful to try out the program. On my machine that looks like

```
docker build -t caresense-demo .
docker run -it caresense-demo asm/prime.bill
```
This will stick you in the interactive simulation program for the prime program. For some commands to run try

```
step 20 # goes forward 20 clock ticks
xReg PC # peek at the PC Reg
xReg IR # peek at the IR Reg
xMem 0 255 # dump the memory bindings from 0-255
undo 5 # go back 5 clockticks
quit # get me out of here
```
Fair warning -- the UX is pretty bad. Eventually I hope to connect the simulation to a TUI "frontend" and display information that way.