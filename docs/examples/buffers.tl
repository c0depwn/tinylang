const STDIN int = 0;

fn main() {
	// allocate a 12 byte buffer
	var nameBuf [12]byte;

	println(`enter your name: `);

	// await user input from fd STDIN
    var readRes int = read(STDIN, nameBuf, len(nameBuf));
    if readRes == -1 {
    	println(`internal error`);
    	println(readRes);
    	return;
    };

    // convert the buffer to a string and print out what the user entered
    print(`your name is: `);
    println(string(nameBuf));

    // print out the ASCII codes one-by-one
    for var i int = 0; i < len(nameBuf); i += 1 {
        println(nameBuf[i]);
    };
}