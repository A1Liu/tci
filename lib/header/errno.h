
extern int __tci_errno;

#define errno __tci_errno

#define EDOM 1
#define ERANGE 2
#define EILSEQ 3
