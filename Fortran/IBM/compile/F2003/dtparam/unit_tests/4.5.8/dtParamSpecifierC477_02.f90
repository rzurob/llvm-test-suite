       ! type-param-spec-list must not be empty.
       type dt(k)
         integer, kind :: k = 0
         integer i
       end type

       type(dt()) x
       class(dt()), allocatable :: x1

       type dt2(k)
         integer, kind :: k
         integer(k) i
       end type

       type(dt2()) y
       class(dt2()), allocatable :: y1
       end
