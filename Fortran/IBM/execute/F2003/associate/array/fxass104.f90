!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: fxass104.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : fxass104.f
!*
!*  DATE                       : Feb 5,2004
!*
!*  PRIMARY FUNCTIONS TESTED   : ASSOCIATE on INTRINSIC Data Types
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  KEYWORD(S)                 : ASSOCIATE,dimension,character,byte
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
!*  DESCRIPTION                : Test: ASSOCIATE with expression and with
!*                                     single dimention array with character
!*                                     and byte data types.
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


      program fxass103
      implicit none

      character*5, dimension(3) :: char1
      character*6, dimension(2) :: char2
      character*12, dimension(3) :: res_c

      byte, dimension(2) :: byt1, byt2

      char1( 1 ) = 'white'
      char1( 2 ) = 'green'
      char1( 3 ) = 'brown'

      char2( 1 ) = 'flower'
      char2( 2 ) = 'cookie'

      byt1 = (/ 1,2 /)
      byt2 = (/ 2,4 /)

      res_c(1) = 'white flower'
      res_c(2) = 'brown cookie'
      res_c(3) = 'green flower'

      associate ( arg1 => (char1(1) //' '// char2(1)) )
           if(arg1 .ne. res_c(1))then
           error stop 1
           endif
      end associate

      associate ( arg2 => char1(2) //' '// char2(1) )
           if(arg2 .ne. res_c(3))then
           error stop 2
           endif
      end associate

      associate ( arg3 => char1(3) //' '// char2(2) )
           if(arg3 .ne. res_c(2))then
           error stop 3
           endif
      end associate

      associate ( arg5 => byt1(2) + byt2(1) )
           if(arg5 .ne. byt2(2))then
           error stop 5
           endif
      end associate

      end
