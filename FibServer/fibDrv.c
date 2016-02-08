#include <stdio.h>
#include "fibDev.h"
#include "fibDvr.h"

#define e1 x
#define e7 newInd

unsigned long long int __clock = 0;
const unsigned long int __coverage_len = 1;
unsigned long int __coverage[1] = {0};
unsigned long int __coverage_index = 0;
unsigned long long int e1 = 0; /* fibDvr.x */
unsigned long long int e2 = 0; /* fibDvr.oldInd */
unsigned long long int e6 = 1; /* fibDvr.valD */
unsigned char e7 = 1; /* fibDvr.newInd */
unsigned char e8 = 1; /* fibDvr.waiting */

/* fibDvr.wait */
void r0(void) {
unsigned char e9;
unsigned char e10;
unsigned char e11;
unsigned char e12;
unsigned char e13;
unsigned char e14;
unsigned char e15;
unsigned char e16;
e9 = ! e8;
e10 = valRcvd && e9;
e11 = ! e10;
e12 = e8 && e11;
e13 = ! e12;
e14 = e11 && e13;
e15 = ! e14;
e16 = e7 && e11;
if (e10) {
__coverage[0] = __coverage[0] | (1 << 0);
}
e8 = e15;
e7 = e16;
}

/* fibDvr.getAns */
void r1(void) {
const unsigned long long int e9 = 50;
unsigned char e10;
unsigned char e11;
unsigned long long int e12;
unsigned char e13;
unsigned char e14;
unsigned char e15;
unsigned char e16;
unsigned char e17;
unsigned char e18;
const unsigned long long int e19 = 5;
unsigned long long int e20;
unsigned long long int e21;
unsigned long long int e22;
e10 = e1 < e9;
e11 = ansReady && e8 && e10;
e12 = e11 ? e1 : e2;
e13 = ! e11;
e14 = e7 && e13;
e15 = ! e14;
e16 = e13 && e15;
e17 = ! e16;
e18 = e8 && e13;
e20 = e1 + e19;
e21 = e11 ? e20 : e1;
e22 = e11 ? ans : e6;
if (e11) {
__coverage[0] = __coverage[0] | (1 << 1);
}
e2 = e12;
e7 = e17;
e8 = e18;
e1 = e21;
e6 = e22;
}

void fibDvr(void) {
if (__clock % 20 == 0) {
r0(); /* fibDvr.wait */
}
if (__clock % 20 == 1) {
r1(); /* fibDvr.getAns */
printf(“i: %lld, fib(i): %lld\n”, e2, e6);
}

__clock = __clock + 1;
}

int main() {
while(__clock < 500) {
fibDvr();
fibDev();
}
return 0;
}
