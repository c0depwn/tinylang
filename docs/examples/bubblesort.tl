var x [3]int = [3]int{3,1,2};

fn main() {
	println(`before sort:`);
	for var i int = 0; i < len(x); i += 1 {
		println(x[i]);
	};

	sort(x);

	println(`after sort:`);
	for var i int = 0; i < len(x); i += 1 {
		println(x[i]);
	};
}

fn sort(arr [3]int) {
	for var i int = 0; i < len(x); i += 1 {
		for var j int = 0; j < len(x) - 1; j += 1 {
			if x[j] > x[j+1] {
				var temp int = x[j];
				x[j] = x[j+1];
				x[j+1] = temp;
			};
		};
	};
}
