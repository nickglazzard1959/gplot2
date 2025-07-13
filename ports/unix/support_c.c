#include <stdio.h>

void sndbyt_(unsigned char* data, int* ndata)
{
  fwrite(data, 1, (size_t)(*ndata), stdout);
  fflush(stdout);
}
