//  01.js - gcd

let n = Number(prompt("n = ?"));
let m = Number(prompt("m = ?"));

if (n != Math.trunc(n) || n <= 0 || m != Math.trunc(m) || m <= 0) {
    alert("Insert positive integer numbers!");
} else {
    while (n != m) {
        if (n < m) {
            m -= n;
        } else {
            n -= m;
        }
    }
    alert("gcd = " + n);
}
