!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
!***********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f dtParamDummyValueC528_04.out
! %COMPOPTS:
! %GROUP: dtParamDummyValueC528_04.f
! %VERIFY: dtParamDummyValueC528_04.out:dtParamDummyValueC528_04.vf
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!***********************************************************************
!***********************************************************************
!* =================================================================== *
!* XL FORTRAN TEST CASE                         IBM INTERNAL USE ONLY  *
!* =================================================================== *
!*                                                                     *
!* TEST CASE TITLE : diagnostic test for DTP dummy argument with VALUE *
!*                                                                     *
!* TEST CASE NAME             : dtParamDummyValueC528_04.f             *
!*                                                                     *
!* PROGRAMMER                 : Rick Sawyer                            *
!*                                                                     *
!* DATE                       : May 13, 2008                           *
!*                                                                     *
!* DESCRIPTION                : C528 - length type parameter values    *
!*         shall be omitted or specified by initialization expressions.*
!*                                                                     *
!* STRUCTURE                  : MAIN                                   *
!*                                                                     *
!* EXECUTABLE                 : No                                     *
!*                                                                     *
!* DEPENDENCIES               : None                                   *
!*                                                                     *
!* REQUIRED COMPILER OPTIONS  : None                                   *
!*                                                                     *
!* NORMAL COMPLETION          : Return code = 1                        *
!*                                                                     *
!* ABNORMAL COMPLETION        : Return code = 0                        *
!*                                                                     *
!* RUN TIME ESTIMATE          : <60 SEC                                *
!*                                                                     *
!* CONDITIONS TESTED          : Listed below.                          *
!***********************************************************************

       ! C528: If the value attribute is specified, the length type parameter
       ! values must be omitted or specified by initialization exprssions.
       module m
         type dt(l)
           integer, len :: l = 2
           character(l) c
         end type
       end module

       subroutine sub(a1,a2,a3,b1,b2,b3,c1,c2,c3,d1,d2,d3,e1,e2,e3,f1,f2,f3,g1,g2,g3,h1,h2,h3,n)
         use m
         integer, intent(in) :: n
         integer :: i = n
         interface
           pure function foo(a)
             integer foo
             integer, intent(in) :: a
           end function
         end interface

         value :: a2, b2, d2, e2, f2, g2
         type(dt(2)) :: a1, a2, a3
         type(dt(:)) :: b1, b2, b3  ! illegal
         type(dt(*)), value :: c1, c2, c3  ! illegal
         type(dt) :: d1, d2, d3
         type(dt(n)) :: e1, e2, e3  ! illegal
         type(dt(i)) :: f1, f2, f3  ! illegal
         type(dt(foo(2))) :: g1, g2, g3  ! illegal
         value :: a3, b3, d3, e3, f3, g3

       end subroutine

       pure function foo(a)
         integer foo
         integer, intent(in) :: a

         foo=a*2
       end function
