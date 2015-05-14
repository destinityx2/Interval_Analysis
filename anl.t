fun main ()
{
	x := ff(4);
	write x;
}

fun ff(n) 
{
	if n <= 3 then {
		return 3;
	} else {
		return 5;
	}
}