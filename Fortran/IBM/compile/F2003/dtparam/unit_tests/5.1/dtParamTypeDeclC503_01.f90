       ! C503: TYPE(derived-type-spec) must not specify an abstract type.
       type, abstract :: dt(k,l)
         integer, kind :: k
         integer, len :: l
         integer(k) i(l)
       end type

       type(dt(2,:)), allocatable :: x
       end
