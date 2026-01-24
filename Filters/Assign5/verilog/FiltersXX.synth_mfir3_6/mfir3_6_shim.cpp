#include <cstdlib>

#include <verilated.h>

#include "Vmfir3_6.h"

int main(int argc, char **argv) {
  Verilated::commandArgs(argc, argv);

  Vmfir3_6 *top = new Vmfir3_6;

  while(!Verilated::gotFinish()) {
    top->eval();
  }

  top->final();

  delete top;

  return EXIT_SUCCESS;
}

