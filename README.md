# Sense-VM (Work in progress)

Sense-VM is a bytecode-vm for microcontrollers such as STM32, and
NRF52. Sense-VM is based on the Categorical Abstract Machine but is augmented
to support Concurrent ML style concurrency.


## Building the compiler

The compiler is implemented in Haskell and uses the stack build
system.  So building the compiler requires [stack](https://docs.haskellstack.org/en/stable/README/)
Go to the `frontend/CamIoT` directory and issue the command:

```
stack build
```

The command above builds an executable called `camiotc` in somewhere
under the '.stack-work' directory that the `stack` tool generates.

To install the binary to `$HOME/.local/bin` issue the command:

```
stack install
```

## Using the camiotc compiler

`camiotc` takes one or more arguments one of which should be a source file.
For example, to compile one of the example programs under `testcases` do:

```
camiotc testcases/good11.cam 
```

The output of this command should be:

```
compiling file `testcases/good11.cam` to output `out.svm`
```

Alternatively a specific output file can be specified using the `-o` parameter:

```
camiotc testcases/good11.cam -o good11.svm
```

and the output should be:

```
compiling file `testcases/good11.cam` to output `good11.svm`
```

## Configuring and building the virtual machine

Coming soon.

## Flashing an STM32F4 discovery board

Coming soon.


# About

First commit 8 July 2020.
