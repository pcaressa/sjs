let f = function(x) {
   let g = function(y) {
     return x + y;
  };
  return g;
};
console.log(f);
console.log(f(10));
console.log(f(10)(20));