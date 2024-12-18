#!/usr/bin/env sh

RED='\033[0;31m'
GREEN='\033[0;32m'
NC='\033[0m'

assert() {
  expected="$1"
  input="$2"

  ./target/debug/bfcc "$input" > tmp.s
  cc -o tmp tmp.s
  ./tmp
  actual="$?"

  if [ "$actual" = "$expected" ]; then
    printf "${GREEN}%s => %s${NC}\n" "$input" "$actual"
  else
    cat tmp.s
    printf "${RED}%s => %s expected, but got %s${NC}\n" "$input" "$expected" "$actual"
    exit 1
  fi
}

cargo build

# step 1
assert 0 '0;'
assert 42 '42;'
# step 2
assert 21 "5+20-4;"
# step 3
assert 41 " 12 + 34 - 5 ;"
# step 5
assert 47 '5+6*7;'
assert 15 '5*(9-6);'
assert 4 '(3+5)/2;'
# step 6
assert 15 '-(-3*+5);'
assert 10 '-10++20;'
# step 7
assert 1 '2 == 2;'
assert 0 '2 != 2;'
assert 1 '2 >= 2;'
assert 0 '2 < 2;'
assert 1 '2 <= 2 == 1;'
# step 9
assert 3 '0;1;2;3;'
assert 6 '
a = 1;
b = 2 + 3;
a + b;'
assert 6 '
foo = 1;
bar = 2 + 3;
foo + bar;'
assert 10 '
a=b=c=d=foo=bar=baz=10;'
# step 11
assert 5 'return 5;return 8;'
assert 14 '
a = 3;
b = 5 * 6 - 8;
return a + b / 2;'

echo OK
