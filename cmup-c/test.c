#include <libgen.h>
#include <stdio.h>

int main(int argc, char *argv[]) {
  printf("%s", dirname(argv[2]));
  return 0;
}
