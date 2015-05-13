fun main (x, y, z)
{
	read n;

	x := 1;
	y := 1;

	answer := 0;
	counter := 2;

	answer := main2(x, 2 * 5 * x-3,   z) + 1;

	while counter < n do
	{
		answer := x + y;
		x := y;
		y := answer;
		counter := counter + 1;
	}

	return answer *2 - 1 + main2(x,y,t,z, 5*3);

	write answer;
}

fun main2 (x, y, z, k)
{
	answer := 0;
	counter := 2;

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