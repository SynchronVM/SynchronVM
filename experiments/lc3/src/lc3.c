#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <signal.h>
/* unix */
#include <unistd.h>
#include <fcntl.h>

#include <sys/time.h>
#include <sys/types.h>
#include <sys/termios.h>
#include <sys/mman.h>

// memory locations
uint16_t memory[UINT16_MAX];

// registers
enum
  {
   R_R0 = 0,
   R_R1,
   R_R2,
   R_R3,
   R_R4,
   R_R5,
   R_R6,
   R_R7,
   R_PC, /* program counter */
   R_COND,
   R_COUNT
  };

uint16_t reg[R_COUNT];

// assembly instruction
enum
  {
   OP_BR = 0, /* branch */
   OP_ADD,    /* add  */
   OP_LD,     /* load */
   OP_ST,     /* store */
   OP_JSR,    /* jump register */
   OP_AND,    /* bitwise and */
   OP_LDR,    /* load register */
   OP_STR,    /* store register */
   OP_RTI,    /* unused */
   OP_NOT,    /* bitwise not */
   OP_LDI,    /* load indirect */
   OP_STI,    /* store indirect */
   OP_JMP,    /* jump */
   OP_RES,    /* reserved (unused) */
   OP_LEA,    /* load effective address */
   OP_TRAP    /* execute trap */
  };

enum
  {
   FL_POS = 1 << 0, /* P */
   FL_ZRO = 1 << 1, /* Z */
   FL_NEG = 1 << 2, /* N */
  };

uint16_t check_key()
{
  fd_set readfds;
  FD_ZERO(&readfds);
  FD_SET(STDIN_FILENO, &readfds);

  struct timeval timeout;
  timeout.tv_sec = 0;
  timeout.tv_usec = 0;
  return select(1, &readfds, NULL, NULL, &timeout) != 0;
}

struct termios original_tio;

void disable_input_buffering()
{
  tcgetattr(STDIN_FILENO, &original_tio);
  struct termios new_tio = original_tio;
  new_tio.c_lflag &= ~ICANON & ~ECHO;
  tcsetattr(STDIN_FILENO, TCSANOW, &new_tio);
}

void restore_input_buffering()
{
  tcsetattr(STDIN_FILENO, TCSANOW, &original_tio);
}

void handle_interrupt(int signal)
{
  restore_input_buffering();
  printf("\n");
  exit(-2);
}

int main(int argc, const char* argv[])
{
    if (argc < 2)
      {
        /* show usage string */
        printf("lc3 [image-file1] ...\n");
        exit(2);
      }

    for (int j = 1; j < argc; ++j)
      {
        if (!read_image(argv[j]))
          {
            printf("failed to load image: %s\n", argv[j]);
            exit(1);
          }
      }

    // SETUP
    signal(SIGINT, handle_interrupt);
    disable_input_buffering();

    /* set the PC to starting position */
    /* 0x3000 is the default */
    enum { PC_START = 0x3000 };
    reg[R_PC] = PC_START;

    int running = 1;
    while (running)
    {
        /* FETCH */
        uint16_t instr = mem_read(reg[R_PC]++);
        uint16_t op = instr >> 12;

        switch (op)
        {
            case OP_ADD:
                {ADD, 6}
                break;
            case OP_AND:
                {AND, 7}
                break;
            case OP_NOT:
                {NOT, 7}
                break;
            case OP_BR:
                {BR, 7}
                break;
            case OP_JMP:
                {JMP, 7}
                break;
            case OP_JSR:
                {JSR, 7}
                break;
            case OP_LD:
                {LD, 7}
                break;
            case OP_LDI:
                {LDI, 6}
                break;
            case OP_LDR:
                {LDR, 7}
                break;
            case OP_LEA:
                {LEA, 7}
                break;
            case OP_ST:
                {ST, 7}
                break;
            case OP_STI:
                {STI, 7}
                break;
            case OP_STR:
                {STR, 7}
                break;
            case OP_TRAP:
                {TRAP, 8}
                break;
            case OP_RES:
            case OP_RTI:
            default:
                {BAD OPCODE, 7}
                break;
        }
    }
    restore_input_buffering();
}
