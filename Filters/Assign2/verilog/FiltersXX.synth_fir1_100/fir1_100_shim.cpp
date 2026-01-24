#include <cstdlib>

#include <verilated.h>

#include "Vfir1_100.h"

int main(int argc, char **argv) {
  Verilated::commandArgs(argc, argv);

  Vfir1_100 *top = new Vfir1_100;

  while(!Verilated::gotFinish()) {
    top->eval();
  }

  top->final();

  delete top;

  return EXIT_SUCCESS;
}

