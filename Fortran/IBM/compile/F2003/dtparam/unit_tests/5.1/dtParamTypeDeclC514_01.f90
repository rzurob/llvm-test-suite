       ! C514: The PARAMETER attribute shall not be specified
       ! for a dummy argument, a pointer, an allocatable entity,
       ! a function, or an object in a common block.

       module m
         type dt(k,l)
           integer, kind :: k
           integer, len :: l
           integer(2*k) :: i(l-1)
         end type
       end module

       function foo(a)
         use m
         class(dt(2,2)), parameter :: foo = dt(2,2)((/ 0 /))

         class(dt(2,2)), parameter :: a = dt(2,2)((/ 1 /))

         allocatable x2
         class(dt(2,2)), parameter :: x2 = dt(2,2)((/ 2 /))

         pointer x3
         class(dt(2,2)), parameter :: x3 = dt(2,2)((/ 3 /))

         common /blk/ x4
         class(dt(2,2)), parameter :: x4 = dt(2,2)((/ 4 /))
       end function foo
