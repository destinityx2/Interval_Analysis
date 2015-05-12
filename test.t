fun main (x, y, z)
{
	read n;

	x := 1;
	y := 1;

	answer := 0;
	counter := 2;

	m (x, y, z*2 + 1 * x + 3);

	while counter < n do
	{
		answer := x + y;
		x := y;
		y := answer;
		counter := counter + 1;
	}

	write answer;
}

fun main2 (x, y, z, k)
{
	answer := 0;
	counter := 2;

	m (x, y, z*2 + 1 * x + 3);

	while counter < n do
	{
		answer := x + y;
		x := y;
		y := answer;
		counter := counter + 1;
	}

	write answer;	
}

fun HelloWorld(x)
{
	x := 1;
	y := 2;
}