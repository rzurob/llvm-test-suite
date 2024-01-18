       ! C502: If CLASS(derived-type-spec) is used, derived-type-spec
       ! must be extensible.
       type dt(k,l)
         integer, kind :: k
         integer, len :: l
         sequence
         integer(k) i(l)
       end type

       type, bind(c) :: dt2(k,l)
         integer, kind :: k
         integer, len :: l
         integer(k) i(l)
       end type

       class(dt(2,:)), allocatable :: x
       class(dt2(4,:)), pointer :: x2
       end
