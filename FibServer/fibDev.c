#include "fibDev.h"
#include "fibDvr.h"

#define e3 ans
#define e7 ansReady
#define e8 valRcvd

static unsigned long long int __clock = 0;
static const unsigned long int __coverage_len = 1;
static unsigned long int __coverage[1] = {0};
static unsigned long int __coverage_index = 0;
static unsigned long long int e1 = 1; /* fibDev.fst */
static unsigned long long int e2 = 1; /* fibDev.snd */
unsigned long long int e3 = 0; /* fibDev.ans */
static unsigned long long int e6 = 0; /* fibDev.i */
unsigned char e7 = 0; /* fibDev.ansReady */
unsigned char e8 = 0; /* fibDev.valRcvd */
static unsigned char e9 = 0; /* fibDev.runFib */

/* fibDev.getIndex */
static void r0(void) {
unsigned char e10;
unsigned char e11;
unsigned char e12;
unsigned char e13;
unsigned char e14;
unsigned char e15;
unsigned char e16;
unsigned char e17;
const unsigned long long int e18 = 1;
unsigned long long int e19;
unsigned long long int e20;
unsigned char e21;
unsigned char e22;
unsigned char e23;
unsigned char e24;
unsigned long long int e25;
e10 = ! e9;
e11 = newInd && e10;
e12 = ! e11;
e13 = e8 && e12;
e14 = ! e13;
e15 = e12 && e14;
e16 = ! e15;
e17 = e7 && e12;
e19 = e11 ? e18 : e2;
e20 = e11 ? e18 : e1;
e21 = e9 && e12;
e22 = ! e21;
e23 = e12 && e22;
e24 = ! e23;
e25 = e11 ? x : e6;
if (e11) {
__coverage[0] = __coverage[0] | (1 << 0);
}
e8 = e16;
e7 = e17;
e2 = e19;
e1 = e20;
e9 = e24;
e6 = e25;
}

/* fibDev.computeFib */
static void r1(void) {
const unsigned long long int e10 = 0;
unsigned char e11;
unsigned char e12;
unsigned long long int e13;
unsigned long long int e14;
unsigned long long int e15;
const unsigned long long int e16 = 1;
unsigned long long int e17;
unsigned long long int e18;
e11 = e10 < e6;
e12 = e9 && e11;
e13 = e12 ? e2 : e1;
e14 = e1 + e2;
e15 = e12 ? e14 : e2;
e17 = e6 – e16;
e18 = e12 ? e17 : e6;
if (e12) {
__coverage[0] = __coverage[0] | (1 << 1);
}
e1 = e13;
e2 = e15;
e6 = e18;
}

/* fibDev.sendVal */
void r2(void) {
const unsigned long long int e10 = 0;
unsigned char e11;
unsigned char e12;
unsigned char e13;
unsigned char e14;
unsigned char e15;
unsigned char e16;
unsigned char e17;
unsigned char e18;
unsigned long long int e19;
unsigned char e20;
e11 = e6 == e10;
e12 = e9 && e11;
e13 = ! e12;
e14 = e8 && e13;
e15 = e7 && e13;
e16 = ! e15;
e17 = e16 && e13;
e18 = ! e17;
e19 = e12 ? e1 : e3;
e20 = e9 && e13;
if (e12) {
__coverage[0] = __coverage[0] | (1 << 2);
}
e8 = e14;
e7 = e18;
e3 = e19;
e9 = e20;
}

void fibDev(void) {
if (__clock % 3 == 0) {
r0(); /* fibDev.getIndex */
}
if (__clock % 3 == 1) {
r1(); /* fibDev.computeFib */
}
if (__clock % 3 == 2) {
r2(); /* fibDev.sendVal */
}

__clock = __clock + 1;
}
