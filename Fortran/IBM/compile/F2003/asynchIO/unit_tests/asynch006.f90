module m
  contains
   subroutine sub(arg)
     integer, asynchronous :: arg(3)
     entry ent(arg2)
       real, asynchronous :: arg2(3)
   end subroutine
end module
use m
real :: r(10)
integer :: i(10)

call ent(r( (/2, 3, 4/)))
call sub(i( (/2, 3, 4/)))
end
