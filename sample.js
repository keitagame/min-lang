function fact(n) {
  if (n <= 1) return 1;
  return n * fact(n - 1);
}

let x = fact(5);
x = x + 1;
print(x);
