!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : Argument association with DTP
!*                             :
!*  PROGRAMMER                 : Huiwen Li
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  FUNCTIONAL TESTED          : Dummy argument contains assumed type
!*                               parameters
!*
!*  DRIVER STANZA              :
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890

       module m
         type dt(l)
           integer, len :: l
           integer i(l)
         end type
       contains
         subroutine sub(d)
           class(dt(*)) d   ! If type is used instead, no problem

           if (d%l /= 5 .or. ubound(d%i,1) /= 5) then
             print *, d%l, ubound(d%i,1)
             error stop 4
           endif
         end subroutine
       end module

       use m
       class(dt(5)), allocatable :: y
       allocate(y)
       call sub(y)
       end

