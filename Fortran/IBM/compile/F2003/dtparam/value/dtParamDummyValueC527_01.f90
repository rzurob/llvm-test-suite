!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
!***********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f dtParamDummyValueC527_01.out
! %COMPOPTS:
! %GROUP: dtParamDummyValueC527_01.f
! %VERIFY: dtParamDummyValueC527_01.out:dtParamDummyValueC527_01.vf
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
!* TEST CASE NAME             : dtParamDummyValueC527_01.f             *
!*                                                                     *
!* PROGRAMMER                 : Rick Sawyer                            *
!*                                                                     *
!* DATE                       : May 13, 2008                           *
!*                                                                     *
!* DESCRIPTION                : C527: If the VALUE attribute is        *
!*         specified, the POINTER attribute shall not be specified.    *
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

       subroutine sub(a,b1,b2,b3,c1,c2,c3,d1,d2,d3,d4,d5)
         use m

         pointer :: b2, d2, d3
         value :: c2, d2, d4
         type(dt), value, pointer :: a
         type(dt), value :: b1, b2, b3
         type(dt), pointer :: c1, c2, c3
         type(dt) :: d1, d2, d3, d4, d5
         value :: c3, d3, d5
         pointer :: b3, d4, d5

       end subroutine
