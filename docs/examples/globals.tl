var a int = c;
var b int = add(a, c);
var c int = 1;

fn main() {
    println(a);
    println(b);
    println(c);
}

fn add(x int, y int) int {
    return x + y;
}