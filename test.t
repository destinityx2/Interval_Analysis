fun main ()
{
	x := fib(6);
	write x;
}

fun fib(n) 
{
	if n <= 1 then {
		return 1;
	} else {
		return fib(n-1) + fib(n-2);
	}
}