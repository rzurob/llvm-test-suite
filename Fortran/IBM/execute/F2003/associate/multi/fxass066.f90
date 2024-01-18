!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: fxass066.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : fxass066.f
!*
!*  DATE                       : Feb 5,2004
!*
!*  PRIMARY FUNCTIONS TESTED   : ASSOCIATE on INTRINSIC Data Types
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  KEYWORD(S)                 : ASSOCIATE, real
!*  TARGET(S)                  :
!*  NUMBER OF TESTS            : 1
!*  STATUS                     : done
!*
!*  STRUCTURE                  : Main program
!*  EXECUTABLE                 : Yes
!*
!*  INPUTS                     : None
!*  OUTPUTS                    : None
!*
!*  SETUP REQUIREMENTS         : N/A
!*  DEPENDENCIES               : External routine ZZRC
!*  REQUIRED COMPILER OPTIONS  : None
!*
!*  NORMAL COMPLETION          : Return code = 0
!*  ABNORMAL COMPLETION        : Return code ^= 0
!*
!*  RUN TIME ESTIMATE          : <60 SECS
!*
!*  CONDITIONS TESTED          : Listed below.
!*
!*  DESCRIPTION                : Test: MULTIPLE ASSOCIATE with expressions
!*                                     with real, real*4, real*8, real*16
!*                                     double precision data types with
!*                                     do loop.
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*                    -Initial Version
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890

      program fxass66
      implicit none

      real a / 1.9 /
      real b / 4.4 /
      real c

      real*4 a4 / 9.0 /
      real*4 b4 / 2.98 /
      real*4 c4

      real*8 a8 / 9.0d0 /
      real*8 b8 / 2.09 /
      real*8 c8

      real*16 a16 / 9.9 /
      real*16 b16 / 2.0q0 /
      real*16 c16

      double precision aa / 1.0d0 /
      double precision bb / 4.0d0 /
      double precision cc

      logical :: precision_r4, precision_r8, precision_r6

      integer count

!-----------   ASSOCIATE with REAL expressions ----------------

      c = a + (b + 1)*10
      do count = 1, 10

      associate ( arg => a , arg1 => b , arg2 => c )
         arg = arg + (arg1 + 1)*10
         if (.not.precision_r4(arg,arg2)) then
           error stop 1
         endif
      end associate
      a = 1.9

      end do

!-----------   ASSOCIATE with REAL*4 expressions ----------------

      c4 = a4 + (b4 + 1)*10
      do count = 1, 10

      associate ( ar => a4 , ar1 => b4 , ar2 => c4 )
         ar = ar + (ar1 + 1)*10
         if (.not.precision_r4(ar,a4)) then
           error stop 4
         endif
      end associate

      end do

!-----------   ASSOCIATE with REAL*8 expressions ----------------

      c8 = a8 + (b8 + 1)*10
      do count = 1, 10

      associate ( ag => a8 , ag1 => b8 , ag2 => c8 )
         ag = ag + (ag1 + 1)*10
         if (.not.precision_r8(ag,a8)) then
           error stop 5
         endif
      end associate

      end do

!-----------   ASSOCIATE with REAL*16 expressions ----------------

      c16 = a16 + (b16 + 1)*10
      do count = 1, 10

      associate ( rg => a16 , rg1 => b16 , rg2 => c16 )
         rg = rg + (rg1 + 1)*10
         if (.not.precision_r6(rg,a16)) then
           error stop 6
         endif
      end associate

      end do

!-----------   ASSOCIATE with DOUBLE PRECISION expressions ----------------

      cc = aa + (bb + 1)*10
      do count = 1, 10

      associate ( arg_1 => aa , arg1_1 => bb , arg2_1 => cc )
         arg_1 = arg_1 + (arg1_1 + 1)*10
         if (.not.precision_r6(arg_1,aa)) then
           error stop 7
         endif
      end associate

      end do

      end
