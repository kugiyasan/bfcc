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

  "$BFCC" "$input" > tmp.s
  cc -o tmp tmp.s foo.o
  printf '%b' "$PURPLE"
  ./tmp
  actual="$?"
  printf '%b' "$NC"

  if [ "$actual" = "$expected" ]; then
    printf "${GREEN}%s => %s${NC}\n" "$input" "$actual"
  else
    cat tmp.s
    printf "${RED}%s => %s expected, but got %s${NC}\n" "$input" "$expected" "$actual"
    exit 1
  fi
}

assertImplicitMain() {
  assert "$1" "main() { $2 }"
}

build() {
  cargo build
  cc -c foo.o foo.c
}

build || exit

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
assertImplicitMain 10 '-10++20;'
# step 7
assertImplicitMain 1 '2 == 2;'
assertImplicitMain 0 '2 != 2;'
assertImplicitMain 1 '2 >= 2;'
assertImplicitMain 0 '2 < 2;'
assertImplicitMain 1 '2 <= 2 == 1;'
# step 9
assertImplicitMain 3 '0;1;2;3;'
assertImplicitMain 6 'a = 6; a;'
assertImplicitMain 6 '
a = 1;
b = 2 + 3;
a + b;'
assertImplicitMain 6 '
foo = 1;
bar = 2 + 3;
foo + bar;'
assertImplicitMain 10 '
a=b=c=d=foo=bar=baz=10;'
# step 11
assertImplicitMain 5 'return 5;return 8;'
assertImplicitMain 14 '
a = 3;
b = 5 * 6 - 8;
return a + b / 2;'
# step 12
assertImplicitMain 4 '
c = 3;
if (1)
  c = 4;
return c;
'
assertImplicitMain 1 '
return 1 < 2;
'
assertImplicitMain 4 '
c = 3;
if (1 < 2)
  c = 4;
return c;
'
assertImplicitMain 1 '
a = 1;
b = 2;
if (a < b)
  return a;
else
  return b;
'
assertImplicitMain 2 '
a = 1;
b = 2;
if (a > b)
  return a;
else
  return b;
'
assertImplicitMain 10 '
i = 0;
while (i < 10)
  i = i + 1;
return i;
'
assertImplicitMain 45 '
tot = 0;
for (i = 0; i < 10; i = i + 1)
  tot = tot + i;
return tot;
'
# step 13
assertImplicitMain 45 '
tot = 0;
i = 0;
while (i < 10) {
  tot = tot + i;
  i = i + 1;
}
return tot;
'
assertImplicitMain 45 '
tot = 0;
for (i = 0; i < 10; i = i + 1) {
  tot = tot + i;
}
return tot;
'
# step 14
assertImplicitMain 0 'foo();'
assertImplicitMain 0 'fooxy(3, 4);'
# step 15
assert 15 '
ret() {
  return 15;
}

main() {
  return ret();
}
'
assert 15 '
id(x) {
  return x;
}

main() {
  return id(15);
}
'
assert 15 '
sub(a, b) {
  return a - b;
}

main() {
  return sub(20, 5);
}
'
assert 55 '
fib(n) {
  if (n <= 2)
    return 1;
  return fib(n - 1) + fib(n - 2);
}

main() {
  return fib(10);
}
'
assert 55 '
sum(m, n) {
  acc = 0;
  for (i = m; i <= n; i = i + 1)
    acc = acc + i;
  return acc;
}

main() {
  return sum(1, 10);
}
'
# step 16
assertImplicitMain 3 '
x = 3;
y = &x;
return *y;
'
assertImplicitMain 3 '
x = 3;
y = 5;
z = &y + 8;
return *z;
'

echo 'All tests passed!'
