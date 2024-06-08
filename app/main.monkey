let perform = fn(f, x) {
  f(x);
};

let curried_add = fn(y) {
  fn (z) {
    y + z;
  };
};

let add_two = curried_add(2);

perform(add_two, 5);
