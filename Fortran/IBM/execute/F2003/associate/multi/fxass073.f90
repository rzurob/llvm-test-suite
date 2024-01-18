!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: fxass073.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : fxass073.f
!*
!*  DATE                       : Feb 5,2004
!*
!*  PRIMARY FUNCTIONS TESTED   : ASSOCIATE on INTRINSIC Data Types
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  KEYWORD(S)                 : ASSOCIATE,logical
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
!*                                     with logical, logical*(1,2,4,8) data
!*                                     types and do loop.
!*
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*                    -Initial Version
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890

      program fxass73a
      implicit none

      logical(4), parameter :: T = .true.
      logical(8), parameter :: F = .false.

      logical a / .true.  /
      logical b / .false. /
      logical c

      logical*1 a1 / .true. /
      logical*1 b1 / .false. /
      logical*1 c1

      logical*2 a2 / .true. /
      logical*2 b2 / .false. /
      logical*2 c2

      logical*4 a4 / .true. /
      logical*4 b4 / .false. /

      logical*8 a8 / .true. /
      logical*8 b8 / .false. /

      integer count

!-----------   ASSOCIATE with LOGICAL expressions ----------------

      c = .false.
      do count = 1, 10

      associate ( arg => a .eqv. b , arc => c)
         if(arg .neqv. arc)then
           error stop 1
         endif
      end associate

      end do

!-----------   ASSOCIATE with LOGICAL*1 expressions ----------------

      c1 = .true.
      do count = 1, 10

      associate ( arg1 => a1 .neqv. b1 , arc1 => c1)
         if(arg1 .neqv. arc1)then
           error stop 2
         endif
      end associate

      end do


!-----------   ASSOCIATE with LOGICAL*2 expressions ----------------

      c2 = .false.
      do count = 1, 10

      associate ( arg2 => a2 .eqv. b2 , arc2 => c2 )
         if(arg2 .neqv. arc2)then
           error stop 3
         endif
      end associate

      end do

!-----------   ASSOCIATE with LOGICAL*4 expressions ----------------

      do count = 1, 10

      associate ( arg4 => (a4 .neqv. b4), ar4 => .true. )
         if(arg4 .neqv. ar4)then
           error stop 4
         endif
      end associate

      end do

!-----------   ASSOCIATE with LOGICAL*8 expressions ----------------

      do count = 1, 10

      associate ( arg8 => a8 .eqv. b8 , ar8 => .false.)
         if(arg8 .neqv. ar8)then
           error stop 5
         endif
      end associate

      end do

      end
