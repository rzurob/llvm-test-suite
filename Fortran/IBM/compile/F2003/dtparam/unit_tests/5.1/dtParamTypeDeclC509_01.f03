       ! C509: An entity declared with the CLASS keyword must be a
       ! dummy argument, or have teh ALLOCATABLE or POINTER attributes.
       module m
         type dt(k,l)
           integer, kind :: k
           integer, len :: l
           integer(k*2) :: i(l-1)
         end type

         class(dt(2,:)), allocatable :: d1  ! legal
         class(dt(4,2)), pointer :: d2      ! legal
       contains
         subroutine sub(a)
           class(dt(2,*)) a                 ! legal
         end subroutine
       end module

       subroutine sub2(n)
         use m, mt => dt
         integer n
         class(mt(4,n)) a1                  ! illegal
         class(mt(4,n)) a2                  ! legal
         class(mt(4,n)), allocatable :: a3  ! legal
         pointer :: a2
       end subroutine
