!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
!***********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f dtParamDummyValueC529_01.out
! %COMPOPTS:
! %GROUP: dtParamDummyValueC529_01.f
! %VERIFY: dtParamDummyValueC529_01.out:dtParamDummyValueC529_01.vf
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
!* TEST CASE NAME             : dtParamDummyValueC529_01.f             *
!*                                                                     *
!* PROGRAMMER                 : Rick Sawyer                            *
!*                                                                     *
!* DATE                       : May 13, 2008                           *
!*                                                                     *
!* DESCRIPTION                : C529: The VALUE attribute shall not be *
!*                              specified for a dummy procedure.       *
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

      ! C529: The VALUE attribute shall not be specified for a dummy
      ! procedure.
       module m
         type dt(l)
           integer, len :: l = 2
           integer i(l-1)
         end type
       end module

       subroutine sub(a,b1,b2,b3,c1,c2,c3,d1,d2,d3,d4,d5,f,g)
         use m

         external :: b2, d2, d3
         value :: c2, d2, d4
         type(dt), value, external :: a
         type(dt), value :: b1, b2, b3
         type(dt), external :: c1, c2, c3
         type(dt) :: d1, d2, d3, d4, d5
         value :: c3, d3, d5
         external :: b3, d4, d5

         type(dt), value :: f,g
         integer i

         call f()
         i = g()

       end subroutine
