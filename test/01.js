// Nested loops

let i = 0;
while (i < 10) {
  let j = 0;
  while (j < 10){
    console.log("(" + i, "," + j + ")");
    ++ j;
  }
  ++ i;
}
