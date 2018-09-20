int main(int argc, char** argv) {
  unsigned int reg;
  unsigned int bit;10
  reg  = 0xACE1;
  do
  {
      bit =((reg >> 0) ^
            (reg >> 2) ^
            (reg >> 3) ^
            (reg >> 5) ) & 0x0001;
      reg = (reg >> 1) | (bit << 15);
      printf("%d\n",reg);

  } while(reg != 0xACE1);
  return (EXIT_SUCCESS);
}