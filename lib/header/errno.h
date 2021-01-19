
extern int __tci_errno;

#define errno __tci_errno

#define EDOM 0xdead
#define ERANGE 0xadde
#define EILSEQ 0xbadd
