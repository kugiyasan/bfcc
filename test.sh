#!/usr/bin/env sh

assert() {
  expected="$1"
  input="$2"

  ./target/debug/bfcc "$input" > tmp.s
  cc -o tmp tmp.s
  ./tmp
  actual="$?"

  if [ "$actual" = "$expected" ]; then
    echo "$input => $actual"
  else
    echo "$input => $expected expected, but got $actual"
    exit 1
  fi
}

# step 1
assert 0 0
assert 42 42
# step 2
assert 21 "5+20-4"
# step 3
assert 41 " 12 + 34 - 5 "
# step 5
assert 47 '5+6*7'
assert 15 '5*(9-6)'
assert 4 '(3+5)/2'

echo OK
