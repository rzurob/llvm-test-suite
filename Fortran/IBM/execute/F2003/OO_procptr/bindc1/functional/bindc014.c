
int getfibonacci ( int n )
{
   if ( n == 0 || n == 1 )
      return n;
   else
      return (getfibonacci( n - 1 ) + getfibonacci( n - 2 ));
}