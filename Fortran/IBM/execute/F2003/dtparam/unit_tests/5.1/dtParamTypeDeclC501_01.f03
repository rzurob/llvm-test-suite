       ! C501: Every type parameter value that is not a colon or an asterisk
       !       must be a specification expression.
       module m
         integer :: useassoc = -1
       end module

       program main
         integer hostassoc
         hostassoc = 5
         call sub(10)
       contains
         subroutine sub(n)
           use m
           integer n, n_back
           integer, parameter :: x(2) = (/ 1, 2 /)
           integer common_obj
           common /blk/ common_obj
           type dt(l)
             integer, len :: l
           end type

           ! A constant or subobject of a constant
           type(dt(x(2))) a
           ! A dummy argument
           type(dt(n*2)) b
           ! An object from a common block
           type(dt(common_obj)) c
           ! A host-associated object
           type(dt(hostassoc)) d
           ! A use-associated object
           type(dt(useassoc)) e
           ! A restricted expression enclosed in parentheses
           type(dt((((5))))) f

           n_back = n
           n = 10 + n

           if (a%l /= x(2)) error stop 1
           if (b%l /= n_back*2) error stop 2
           if (c%l /= common_obj) error stop 3
           if (d%l /= hostassoc) error stop 4
           if (e%l /= useassoc) error stop 5
           if (f%l /= 5) error stop 6
         end subroutine
       end program

       block data
         integer common_obj
         common /blk/ common_obj
         data common_obj /9/
       end block data
