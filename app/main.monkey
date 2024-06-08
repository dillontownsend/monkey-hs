return fn(x) {
  fn(y) {
    x + y;
  };
}(2);
