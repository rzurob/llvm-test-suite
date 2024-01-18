!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: fxass004.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : fxass004.f
!*
!*  DATE                       : Feb 5,2004
!*
!*  PRIMARY FUNCTIONS TESTED   : ASSOCIATE on INTRINSIC Data Types
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  DESCRIPTION                : ASSOCIATE with CHARACTER expressions
!*  KEYWORD(S)                 : ASSOCIATE, character
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
!*  DESCRIPTION                : Test: ASSOCIATE with expressions with
!*                                     character and do while.
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

      program fxass04
      implicit none

      character*4 a4 / 'good' /
      character*4 b4 / 'girl' /
      character*9 c4

      character*9 a8 / 'excellent' /
      character*9 b8 / '  weather' /
      character*19 c8

      character*5 a5 / 'white' /
      character*5 b5 / 'house' /
      character*11 c5

      integer count

!-----------   ASSOCIATE with CHARACTER expressions ----------------

      c4 = 'good girl'
      count = 1
      do while (count .LE. 10)

      associate ( arg4 =>  a4 //' '// b4 )
         if(arg4 .ne. c4)then
           error stop 4
         endif
      end associate
      count = count + 1

      end do

!-----------   ASSOCIATE with CHARACTER expressions ----------------
      count = 1
      c8 = 'excellent  weather'
      FIRST: do while (count .LE. 10)

      associate ( arg8 => a8 )
         if(arg8 .ne. a8)then
           error stop 5
         endif
      end associate
      count = count + 1

      end do FIRST

!-----------   ASSOCIATE with CHARACTER expressions ----------------

      c5 = 'white-house'
      do count = 1, 10

      associate ( arg5 =>  a5 //'-'// b5 )
         if(arg5 .ne. c5)then
           error stop 6
         endif
      end associate

      end do

      end
