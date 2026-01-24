#include <cstdlib>

#include <verilated.h>

#include "Vfir1_6.h"

int main(int argc, char **argv) {
  Verilated::commandArgs(argc, argv);

  Vfir1_6 *top = new Vfir1_6;

  while(!Verilated::gotFinish()) {
    top->eval();
  }

  top->final();

  delete top;

  return EXIT_SUCCESS;
}

