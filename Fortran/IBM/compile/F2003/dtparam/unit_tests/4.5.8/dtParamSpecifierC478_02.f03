       ! C478: We can't have more type specs that there are type
       !       parameters in the type.
       ! Check that inherited type parameters are counted properly.
       type at(k0)
         integer, kind :: k0
       end type

       type, extends(at) :: bt(k1)
         integer, kind :: k1
         integer j
       end type

       type, extends(bt) :: dt(k)
         integer, kind :: k
         integer i
       end type

       type(dt(2,4,8,3)) x
       x%i = 3
       end
