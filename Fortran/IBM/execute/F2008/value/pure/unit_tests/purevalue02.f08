       ! Test that we can pass a by-descriptor object by value
       ! to a pure routine
       module m
         implicit none

         type dt(k, l)
           integer, kind :: k
           integer, len :: l
           integer(k) :: i(l)
         contains
           procedure :: addone => dt_addone
         end type

         type, extends(dt) :: et(k2)
           integer, kind :: k2
           integer(k2) j(l)
         contains
           procedure :: addone => et_addone
         end type
       contains
         pure subroutine dt_addone(a)
           implicit none
           class(dt(4, *)), intent(inout) :: a
           a%i = a%i + 1
         end subroutine

         pure subroutine et_addone(a)
           implicit none
           class(et(4, *, 2)), intent(inout) :: a
           call a%dt%addone
           a%j = a%j + 1
         end subroutine

         pure subroutine puresub(a, b)
           implicit none
           type(et(4, 5, 2)), value :: a
           integer, intent(out) :: b
           call a%addone
           b = sum(a%i) + sum(a%j)
         end subroutine
       end module

       use m
       implicit none
       integer b
       class(et(4, :, 2)), allocatable :: x
       allocate(et(4, 5, 2) :: x)
       x%i = [1, 2, 3, 4, 5]
       x%j = [11, 12, 13, 14, 15]
       call puresub(x, b)
       if (b /= 90) then
         print *, b
         stop 1
       endif
       end
