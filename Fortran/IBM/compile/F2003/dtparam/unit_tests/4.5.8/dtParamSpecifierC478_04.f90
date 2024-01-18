       ! C478: Only one type param spec is allowed per type parameter.
       type dt(k)
         integer, kind :: k
         integer i
       end type

       type(dt(2,k=2)) x
       x%i = 3
       end
