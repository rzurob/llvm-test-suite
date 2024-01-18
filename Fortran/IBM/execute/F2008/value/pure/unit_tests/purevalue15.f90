!******************************************************************************
!*  ===========================================================================
!*
!*  TEST CASE NAME  : F2008/value/pure/unit_tests/purevalue15.f
!*  DATE            : 2010-12-01
!*  DRIVER STANZA   : xlf2003
!*
!*  DESCRIPTION
!*  - this is a copy of Rafik's purevalue02.f, extended to include pure
!*    functions as procedure components, with dummy arg having value attribute
!234567890123456789012345678901234567890123456789012345678901234567890123456789

       module m
         implicit none

         type dt(k, l)
           integer, kind :: k
           integer, len :: l
           integer(k) :: i(l)
         contains
           procedure :: addone => dt_addone
           procedure :: fadd => dt_fadd
         end type

         type, extends(dt) :: et(k2)
           integer, kind :: k2
           integer(k2) j(l)
         contains
           procedure :: addone => et_addone
           procedure :: fadd => et_fadd
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

         integer pure function dt_fadd(a,b)
           implicit none
           class(dt(4, *)), intent(in) :: a
           integer, value :: b
           dimension dt_fadd(5)
           dt_fadd = a%i + b
           b = -1
         end function

         integer pure function et_fadd(a,b)
           implicit none
           class(et(4, *, 2)), intent(in) :: a
           integer, value :: b
           dimension et_fadd(5)
           et_fadd = a%j + a%dt%fadd(b)
           et_fadd = et_fadd + b
           b = -1
         end function

         integer pure function purefunc(a, b)
           implicit none
           type(et(4, 5, 2)), value :: a
           integer, value :: b
           dimension purefunc(5)
           purefunc = a%fadd(b)
           purefunc = purefunc + b
           b = -1
         end function
       end module

       use m
       implicit none
       integer :: b,r(5)
       class(et(4, :, 2)), allocatable :: x
       allocate(et(4, 5, 2) :: x)
       x%i = [1, 2, 3, 4, 5]
       x%j = [11, 12, 13, 14, 15]
       call puresub(x, b)
       if (b /= 90) then
         print *, b
         stop 1
       endif
       b = 100
       r = purefunc(x, b)
       if (any(r /= [312,314,316,318,320])) then
         print *,b
         stop 2
       endif
       if (b /= 100) then
         print *, b
         stop 3
       endif
       end
