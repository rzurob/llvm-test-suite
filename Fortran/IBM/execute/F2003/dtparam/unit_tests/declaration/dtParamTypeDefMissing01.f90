       ! Missing type parameter definitions.  The recovery is to assume
       ! default integer kind type parameters.
       type dt(k,l)     ! Errors 1,2
         integer(k) i
       end type

       type dt2(l)      ! Error 3
         integer i
       end type

       type(dt(4,2)) x
       type(dt2(2)) y
       print *, kind(x%i), x%k, x%l
       print *, y%l

       end
