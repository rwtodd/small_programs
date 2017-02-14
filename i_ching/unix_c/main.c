#include<ncurses.h>
#include<stdio.h>
#include<stdlib.h>

/* We need a type for a hexagram. It fits in
 * 8-bits, so unsigned char will do.
 */
typedef unsigned char hexagram;

/* Store the hexagram descriptions in another file to
 * keep this file tidy.
 */
struct hex_data {
  const char *const name;
  const char *const desc1;
  const char *const desc2;
  hexagram lines;
} hex_data[] = {
#include "hex_data.h"
};

/* Store the trigram descriptions in anotehr file to
 * keep this file tidy.
 */
const char *const trigrams[] = {
#include "tri_data.h"
};


int main(int arc, char **argv) {
  printf("%d: %s\n", hex_data[1].lines, hex_data[1].name);
  return 0;
}
