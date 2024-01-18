!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: fxass117.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : fxass117.f
!*
!*  DATE                       : Feb 5,2004
!*
!*  PRIMARY FUNCTIONS TESTED   : ASSOCIATE on INTRINSIC Data Types
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  KEYWORD(S)                 : ASSOCIATE,complex
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
!*  DESCRIPTION                : Test: ASSOCIATE with POINTER
!*                                     with complex, complex(4)
!*                                     complex*8, complex*16
!*                                     data types.
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

      program fxass117
      implicit none

      complex, target :: a = (1.0e0, 2.0e0)
      complex, pointer :: b

      complex(4), target :: a4 = (2.0e0, 3.0e0)
      complex(4), pointer :: b4

      complex*8, target :: a8 = (1.0e0, 2.0e0)
      complex*8, pointer :: b8

      complex*16, target :: a16 = (1.0d0, 2.0d0)
      complex*16, pointer :: b16

      logical :: precision_x3, precision_x6, precision_x8

      b => a
      b4 => a4
      b8 => a8
      b16 => a16

      associate ( arg => b )
      if (.not.precision_x8(arg,a)) then
         error stop 1
      endif
      end associate

      associate ( arg3 => (b4 * 3) + 2, arg4 => b8 )
      if (.not.precision_x8(arg3,((a4 * 3) + 2))) then
         error stop 4
      endif

      if (.not.precision_x8(arg4,a8)) then
         error stop 5
      endif
      end associate

      associate ( arg5 => b16 )
      if (.not.precision_x6(arg5,a16)) then
         error stop 6
      endif
      end associate

      end

