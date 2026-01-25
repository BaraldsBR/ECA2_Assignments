#include <cstdlib>

#include <verilated.h>

#include "Vmiir2.h"

int main(int argc, char **argv) {
  Verilated::commandArgs(argc, argv);

  Vmiir2 *top = new Vmiir2;

  while(!Verilated::gotFinish()) {
    top->eval();
  }

  top->final();

  delete top;

  return EXIT_SUCCESS;
}

