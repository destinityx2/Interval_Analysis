fun main() 
{
	x := tmp(4);
	y := 3;

	if y < x then {
		x := 1;
	} else {
		x := 5;
	}
	write x;
}

fun tmp(x) 
{
	if x < 1 then {
			return 2;
	} else {
			return 5;
	}
}
