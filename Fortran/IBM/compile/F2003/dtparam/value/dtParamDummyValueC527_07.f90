!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
!***********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f dtParamDummyValueC527_07.out
! %COMPOPTS:
! %GROUP: dtParamDummyValueC527_07.f
! %VERIFY: dtParamDummyValueC527_07.out:dtParamDummyValueC527_07.vf
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
!* TEST CASE NAME             : dtParamDummyValueC527_07.f             *
!*                                                                     *
!* PROGRAMMER                 : Rick Sawyer                            *
!*                                                                     *
!* DATE                       : May 13, 2008                           *
!*                                                                     *
!* DESCRIPTION                : C527: If the VALUE attribute is        *
!*         specified, the PARAMETER attribute shall not be specified.  *
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

      ! C527: If the VALUE attribute is specified, the PARAMETER, EXTERNAL, 
      ! POINTER, ALLOCATABLE, DIMENSION, VOLATILE, INTENT(INOUT), or 
      ! INTENT(OUT) attribute shall not be specified.
       module m
         type dt(l)
           integer, len :: l = 2
           integer i(l-1)
         end type
       end module

       subroutine sub(a,b,c1,c2)
         use m

         value :: b
         type(dt), value, parameter :: a = dt(4)(5)
         type(dt), parameter :: b = dt(3)(4)
         type(dt) :: c1, c2
         value :: c1
         parameter (c1 = dt(6)(7), c2 = dt(7)(8))
         value :: c2

       end subroutine
