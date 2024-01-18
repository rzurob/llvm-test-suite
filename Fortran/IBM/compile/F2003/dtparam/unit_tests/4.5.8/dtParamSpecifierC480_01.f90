       ! C480: Each keyword must be the name of a type parameter
       !       of the type.
       type dt(k,l)
         integer, kind :: k =0 
         integer, len :: l
         integer i
       end type

       type(dt(i=2,l=3)) x
       x%i = 3
       end
