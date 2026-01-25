#include <cstdlib>

#include <verilated.h>

#include "Vmiir1.h"

int main(int argc, char **argv) {
  Verilated::commandArgs(argc, argv);

  Vmiir1 *top = new Vmiir1;

  while(!Verilated::gotFinish()) {
    top->eval();
  }

  top->final();

  delete top;

  return EXIT_SUCCESS;
}

