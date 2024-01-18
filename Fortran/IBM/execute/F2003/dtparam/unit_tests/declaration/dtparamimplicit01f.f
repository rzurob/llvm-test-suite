       ! implicittype / implicitclass as first clause in implicit
       ! fixed form
       module m
         type dt(k)
           integer, kind :: k
           integer(k) :: i
         end type
       end module

       @process fixed
       use m
       implicittype(dt(2)) (a-b)
       implicitclass(dt(4)) (c-d)

       allocatable :: d
       if (kind(a%i) /= 2 .or. a%k /= 2) then
         print *, kind(a%i)
         error stop 1
       endif

       allocate(d)
       if (kind(d%i) /= 4 .or. d%k /= 4) then
         print *, kind(d%i)
         error stop 2
       endif
       end
