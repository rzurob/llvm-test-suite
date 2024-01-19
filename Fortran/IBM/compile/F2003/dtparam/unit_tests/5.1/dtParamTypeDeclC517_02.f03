       ! C517: The SAVE attribute must not be specified for an automatic
       ! object or a dummy argument.
       module m
         type dt(k,l)
           integer, kind :: k
           integer, len :: l
           integer(k*2) i(l-1)
         end type
       contains
         subroutine sub(a,n)
           integer n
           type(dt(2,*)), save :: a
           type(dt(2,n)), save :: b
         end subroutine
       end module
