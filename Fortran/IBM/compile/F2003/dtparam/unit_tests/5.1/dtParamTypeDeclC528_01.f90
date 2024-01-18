       ! C528: If the value attribute is specified, the length type parameter
       ! values must be omitted or specified by initialization exprssions.
       module m
         type dt(l)
           integer, len :: l = 2
           integer i(l-1)
         end type
       end module

       subroutine sub(a,b,c,d,e,n)
         use m
         integer n
         type(dt(2)), value :: a
         type(dt(:)), value :: b  ! illegal
         type(dt(*)), value :: c  ! illegal
         type(dt), value :: d
         type(dt(n)), value :: e  ! illegal
       end subroutine
