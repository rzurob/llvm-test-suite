       ! C481: An assumed type parameter value is only allowed for dummy args,
       !       type guards, and allocation of dummy args.
       type dt(k,l)
         integer, kind :: k
         integer, len :: l
         real(k) r(l)
       end type
       class(dt(l=3,k=4)), allocatable :: x

       allocate(x)
       select type (x)
         type is (dt(l=*, k=4))
           ! success
         class default
           error stop 1_4
       end select
       end
