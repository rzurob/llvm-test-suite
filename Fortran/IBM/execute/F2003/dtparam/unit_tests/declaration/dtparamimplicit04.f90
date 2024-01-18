       ! implicit type and implicit class, with assumed length type parameters
       module m
         type dt(k,l)
           integer, kind :: k
           integer, len :: l
           integer(k) :: i(l)
         end type
       contains
         subroutine sub(a,d)
           implicit type(dt(2,*)) (a-b)
           implicit class(dt(4,*)) (c-d)

           if (a%k /= 2 .or. kind(a%i) /= 2) then
             print *, a%k, kind(a%i)
             error stop 1
           endif
           if (a%l /= 4 .or. ubound(a%i,1) /= 4) then
             print *, a%l, ubound(a%i,1)
             error stop 2
           endif

           if (d%k /= 4 .or. kind(d%i) /= 4) then
             print *, d%k, kind(d%i)
             error stop 3
           endif
           if (d%l /= 5 .or. ubound(d%i,1) /= 5) then
             print *, d%l, ubound(d%i,1)
             error stop 4
           endif
         end subroutine
       end module

       use m
       implicit type(dt(2,4)) (x), class(dt(4,5)) (y)
       pointer y
       allocate(y)
       call sub(x,y)
       end
