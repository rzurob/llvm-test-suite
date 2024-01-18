!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: fxass118.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : fxass118.f
!*
!*  DATE                       : Feb 5,2004
!*
!*  PRIMARY FUNCTIONS TESTED   : ASSOCIATE on INTRINSIC Data Types
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  KEYWORD(S)                 : ASSOCIATE,logical,character
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
!*                                     with logical,logical(1,2,4,8)
!*                                     and character
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

@PROCESS CTYPLSS


      program fxass118
      implicit none

      logical, target :: a = .true.
      logical, pointer :: b

      logical*1, target :: a1 = .false.
      logical*1, pointer :: b1

      logical*2, target :: a2 = .true.
      logical*2, pointer :: b2

      logical*4, target :: a4 = .false.
      logical*4, pointer :: b4

      logical*8, target :: a8 = .true.
      logical*8, pointer :: b8

      character*4, target :: ab1 = 'abcd'
      character*4, pointer :: ab2

      b => a
      b1 => a1
      b2 => a2
      b4 => a4
      b8 => a8
      ab2 => ab1

      associate ( arg => b )
      if(arg .neqv. a)then
         error stop 1
      endif
      end associate

      associate ( arg1 => b1, arg2 => b2, arg3 => (.true. .eqv. b4), arg4 => b8 )
      if(arg1 .neqv. a1)then
         error stop 2
      endif

      if(arg2 .neqv. a2)then
         error stop 3
      endif

      if(arg3 .neqv. (.true. .eqv. b4))then
         error stop 4
      endif

      if(arg4 .neqv. a8)then
         error stop 5
      endif
      end associate

      associate ( arg5 => ab2 )
      if(arg5 .ne. ab1)then
         error stop 6
      endif
      end associate

      end

