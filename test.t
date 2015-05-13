fun main ()
{
	read n;
	x := fib(n);
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