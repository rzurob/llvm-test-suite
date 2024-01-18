       ! C478: We can't have more type specs that there are type
       !       parameters in the type.
       type dt(k)
         integer, kind :: k
         integer i
       end type

       type(dt(2,3)) x
       x%i = 3
       end
