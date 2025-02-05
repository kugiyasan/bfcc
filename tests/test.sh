#!/usr/bin/env sh

RED='\033[0;31m'
GREEN='\033[0;32m'
PURPLE='\033[0;35m'
NC='\033[0m'

TESTS_DIR="$(dirname "$(realpath "$0")")"
REPO_ROOT="$(dirname "$TESTS_DIR")"
BFCC="$REPO_ROOT/target/debug/bfcc"

cd "$TESTS_DIR" || exit

assert() {
  expected="$1"
  input="$2"

  printf '%s' "$input" | "$BFCC" - > tmp.s
  cc -o tmp tmp.s foo.o
  printf '%b' "$PURPLE"
  ./tmp
  actual="$?"
  printf '%b' "$NC"

  if [ "$actual" = "$expected" ]; then
    printf "${GREEN}%s => %s${NC}\n" "$input" "$actual"
    rm tmp.s tmp
  else
    cat tmp.s
    printf "${RED}%s => %s expected, but got %s${NC}\n" "$input" "$expected" "$actual"
    exit 1
  fi
}

assertImplicitMain() {
  assert "$1" "int main() {$2}"
}

build() {
  cargo build || exit
  cc -c foo.o foo.c
}

build

# step 1
assertImplicitMain 0 '0;'
assertImplicitMain 42 '42;'
# step 2
assertImplicitMain 21 "5+20-4;"
# step 3
assertImplicitMain 41 " 12 + 34 - 5 ;"
# step 5
assertImplicitMain 47 '5+6*7;'
assertImplicitMain 15 '5*(9-6);'
assertImplicitMain 4 '(3+5)/2;'
# step 6
assertImplicitMain 15 '-(-3*+5);'
assertImplicitMain 10 '-10+ +20;'
# step 7
assertImplicitMain 1 '2 == 2;'
assertImplicitMain 0 '2 != 2;'
assertImplicitMain 1 '2 >= 2;'
assertImplicitMain 0 '2 < 2;'
assertImplicitMain 1 '2 <= 2 == 1;'
# step 9
assertImplicitMain 3 '0;1;2;3;'
assertImplicitMain 6 'int a; a = 6; a;'
assertImplicitMain 6 '
int a;
int b;
a = 1;
b = 2 + 3;
a + b;'
assertImplicitMain 6 '
int foo;
int bar;
foo = 1;
bar = 2 + 3;
foo + bar;'
assertImplicitMain 10 '
int a;
int b;
int c;
int d;
int foo;
int bar;
int baz;
a=b=c=d=foo=bar=baz=10;'
# step 11
assertImplicitMain 5 'return 5;return 8;'
assertImplicitMain 14 '
int a;
int b;
a = 3;
b = 5 * 6 - 8;
return a + b / 2;'
# step 12
assertImplicitMain 4 '
int c;
c = 3;
if (1)
  c = 4;
return c;
'
assertImplicitMain 1 '
return 1 < 2;
'
assertImplicitMain 4 '
int c;
c = 3;
if (1 < 2)
  c = 4;
return c;
'
assertImplicitMain 1 '
int a;
int b;
a = 1;
b = 2;
if (a < b)
  return a;
else
  return b;
'
assertImplicitMain 2 '
int a;
int b;
a = 1;
b = 2;
if (a > b)
  return a;
else
  return b;
'
assertImplicitMain 10 '
int i;
i = 0;
while (i < 10)
  i = i + 1;
return i;
'
assertImplicitMain 45 '
int tot;
int i;
tot = 0;
for (i = 0; i < 10; i = i + 1)
  tot = tot + i;
return tot;
'
# step 13
assertImplicitMain 45 '
int tot;
int i;

tot = 0;
i = 0;
while (i < 10) {
  tot = tot + i;
  i = i + 1;
}
return tot;
'
assertImplicitMain 45 '
int tot;
int i;
tot = 0;
for (i = 0; i < 10; i = i + 1) {
  tot = tot + i;
}
return tot;
'
# step 14
assert 0 '
int foo();

int main() {
  foo();
}
'
assert 0 '
int fooxy(int x, int);

int main() {
  fooxy(3, 4);
}
'
# step 15
assert 15 '
int ret() {
  return 15;
}

int main() {
  return ret();
}
'
assert 15 '
int id(int x) {
  return x;
}

int main() {
  return id(15);
}
'
assert 15 '
int sub(int a, int b) {
  return a - b;
}

int main() {
  return sub(20, 5);
}
'
assert 55 '
int fib(int n) {
  if (n <= 2)
    return 1;
  return fib(n - 1) + fib(n - 2);
}

int main() {
  return fib(10);
}
'
assert 55 '
int sum(int m, int n) {
  int acc;
  int i;
  acc = 0;
  for (i = m; i <= n; i = i + 1)
    acc = acc + i;
  return acc;
}

int main() {
  return sum(1, 10);
}
'
# step 16
assertImplicitMain 3 '
int x;
int *y;
x = 3;
y = &x;
return *y;
'
assertImplicitMain 3 '
int x;
int y;
int *z;
x = 3;
y = 5;
z = &y + 1;
return *z;
'
# step 17
assertImplicitMain 1 'return - - - - 1;'
# step 18
assertImplicitMain 3 '
int x;
int *y;
y = &x;
*y = 3;
return x;
'
# step 19
assert 8 '
int alloc4(int **p, int a, int b, int c, int d);

int main() {
  int *p;
  alloc4(&p, 1, 2, 4, 8);
  int *q;
  q = p + 2;
  *q;
  q = p + 3;
  return *q;
}
'
# step 20
assertImplicitMain 7 '
int x;
int *y;
int a;
int b;
int c;
int d;
int e;
int f;
int g;

a = sizeof(x) == 4;
b = sizeof(y) == 8;

c = sizeof(x + 3) == 4;
d = sizeof(y + 3) == 8;
e = sizeof(*y) == 4;

f = sizeof(1) == 4;
g = sizeof(sizeof(1)) == 4;
return a + b + c + d + e + f + g;
'
# step 21
assertImplicitMain 1 '
int a[2];
*a = 1;
return *a;
'
assertImplicitMain 3 '
int a[2];
*a = 1;
*(a + 1) = 2;
int *p;
p = a;
return *p + *(p + 1);
'
# step 22
assertImplicitMain 42 '
int arr[4];
arr[3] = 42;
return 3[arr];
'
assertImplicitMain 6 '
int arr[5];
int i;
for (i = 0; i < 5; i = i + 1)
  arr[i] = i;

return arr[2] + 4[arr];
'
# step 23
assert 23 '
int x;
int y[20];
int main() { x = 7; y[5] = 16; return x + y[5]; }
'
# step 24
assertImplicitMain 3 '
char x[3];
x[0] = -1;
x[1] = 2;
int y;
y = 4;
return x[0] + y;
'
# step 25
assertImplicitMain 97 '
char *x;
x = "abc";
return x[0];
'

assertImplicitMain 0 '
int arr[2];
int *p;
p = &arr[1];
return 0;
'
assertImplicitMain 0 '
struct Vec2 { int x, y; } v;
v.x = 0;
return v.x;
'
assertImplicitMain 0 '
return 5 - 3 - 2;
'
assertImplicitMain 8 '
return 32 / 2 / 2;
'
assertImplicitMain 11 '
int* arr[5];
int **p;
p = arr;
int a;
int b;
a = 5;
b = 6;
arr[3] = &a;
p[4] = &b;
return *arr[3] + *p[4];
'
assertImplicitMain 0 '
struct S { int x; int y; } s;
struct S *p;

p = &s;
s.x = 1;
p->y = 2;
return p->y + s.x - 3;
'
assertImplicitMain 3 '
struct S { int x; int y; } s;
struct S *p;
p = &s;
s.y = 3;
return p->y;
'
assert 0 '
void (*__func) (void);
int atexit(void (*__func) (void));
int main() { return 0; }
'
assert 0 '
typedef long unsigned int size_t;
extern size_t __ctype_get_mb_cur_max (void) ;
int main() { return 0; }
'
assert 0 '
#include <stdlib.h>
int main() { return 0; }
'
assert 0 '
#include <ctype.h>
int main() { return 0; }
'
assert 42 '
int true_fn() { return 42; }
int false_fn() { return 62; }
int main() { return 2 < 4 ? true_fn() : false_fn(); }
'
assertImplicitMain 1 '
return '"'abcdefgh'"' == 1633837924;
'
assertImplicitMain 45 '
int sum = 0;
for (int i = 0; i < 20; i++) {
  if (i == 10) break;
  sum += i;
}
return sum;
'
assertImplicitMain 100 '
int x = 3, y;
switch (x) {
  case 1:  { y = 5; break; }
  case 2:    y = 7; break;
  case 3:  { y = 9; }
  default: { y = 100; }
}
return y;
'

echo 'All tests passed!'
