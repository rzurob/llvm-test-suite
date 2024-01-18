!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90 -qintlog
! %GROUP: fxass086.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : fxass086.f
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
!*  DESCRIPTION                : Test: ASSOCIATE with expressions and with
!*                                     logical, logical*(1,2,4,8) data
!*                                     types and do loop. using -qintlog
!*                                     option.
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*                    -Initial Version
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890

      program fxass86a
      implicit none

      logical(4), parameter :: T = .true.
      logical(8), parameter :: F = .false.

      logical a / 1 /
      logical b / 0 /
      logical c

      logical*1 a1 / .true. /
      logical*1 b1 / .false. /
      logical*1 c1

      logical*2 a2 / T /
      logical*2 b2 / F /
      logical*2 c2

      logical*4 a4 / .true. /
      logical*4 b4 / .false. /

      logical*8 a8 / .true. /
      logical*8 b8 / .false. /

      integer count

!-----------   ASSOCIATE with LOGICAL expressions ----------------

      c = 0
      do count = 1, 10

      associate ( arg => a )
         arg = arg .eqv. b
         if(arg .neqv. a)then
           error stop 1
         endif
      end associate

      end do

!-----------   ASSOCIATE with LOGICAL*1 expressions ----------------

      c1 = .true.
      do count = 1, 10

      associate ( arg1 => a1 )
         arg1 = arg1 .neqv. b1
         if(arg1 .neqv. a1)then
           error stop 2
         endif
      end associate

      end do


!-----------   ASSOCIATE with LOGICAL*2 expressions ----------------

      c2 = F
      do count = 1, 10

      associate ( arg2 => a2 )
         arg2 = arg2 .eqv. b2
         if(arg2 .neqv. a2)then
           error stop 3
         endif
      end associate

      end do

!-----------   ASSOCIATE with LOGICAL*4 expressions ----------------

      do count = 1, 10

      associate ( arg4 => a4 )
         arg4 = arg4 .neqv. b4
         if(arg4 .neqv. a4)then
           error stop 4
         endif
      end associate

      end do

!-----------   ASSOCIATE with LOGICAL*8 expressions ----------------

      do count = 1, 10

      associate ( arg8 => a8 )
         arg8 = arg8 .eqv. b8
         if(arg8 .neqv. a8)then
           error stop 5
         endif
      end associate

      end do

      end
