       ! C481: An assumed type parameter value is only allowed for dummy args,
       !       type guards, and allocation of dummy args.
       type dt(k,l)
         integer, kind :: k
         integer, len :: l
         real(k) r(l)
       end type

       type(dt(l=*,k=4)) x
       end
