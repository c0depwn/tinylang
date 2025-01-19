var x [2][2]int = [2][2]int{
	[2]int{1,2},
	[2]int{3,4},
};

fn main() {
	for var i int = 0; i < len(x); i += 1 {
		println(``);
		for var j int = 0; j < len(x[i]); j += 1 {
			print(x[i][j]);
			print(` `);
		};
	};
}

