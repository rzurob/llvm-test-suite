       ! C477 type-param-spec-list shall appear only if the type is parameterized.
       ! Also, from the grammar: type-param-spec-list must not be empty.
       type dt
         integer i
       end type

       type(dt(2)) x
       type(dt(k=2)) y
       type(dt()) z

       class(dt(2)), allocatable :: x1
       class(dt(k=2)), allocatable :: y1
       class(dt()), allocatable :: z1
       end
