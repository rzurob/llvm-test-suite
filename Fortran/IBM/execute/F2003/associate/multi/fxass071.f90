!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: fxass071.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : fxass071.f
!*
!*  DATE                       : Feb 5,2004
!*
!*  PRIMARY FUNCTIONS TESTED   : ASSOCIATE on INTRINSIC Data Types
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  KEYWORD(S)                 : ASSOCIATE,integer,byte
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
!*                                     with integer, integer*1, integer*2
!*                                     integer*4, integer*8 and byte
!*                                     data types and do loop.
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

      program fxass71a
      implicit none

      integer a / 1 /
      integer b / 4 /
      integer c

      integer*1 a1 / 2 /
      integer*1 b1 / 8 /
      integer*1 c1

      integer*2 a2 / 5 /
      integer*2 b2 / 4 /
      integer*2 c2

      integer*4 a4 / 9 /
      integer*4 b4 / 2 /
      integer*4 c4

      integer*8 a8 / 9 /
      integer*8 b8 / 2 /
      integer*8 c8

      byte ab1 / 1 /
      byte ab2 / 4 /
      byte cb

      integer count

!-----------   ASSOCIATE with INTEGER expressions ----------------

      c = (a + b)*10 + 1
      do count = 1, 10

      associate ( arg => (a + b)*10 + 1 , ar => c)
         if(arg .ne. ar)then
           error stop 1
         endif
      end associate

      end do

!-----------   ASSOCIATE with INTEGER*1 expressions ----------------

      c1 = (a1 + b1)*10_1 + 1_1
      do count = 1, 10

      associate ( arg1 => (a1 + b1)*10_1 + 1_1 , ar1 => c1 )
         if(arg1 .ne. ar1)then
           error stop 2
         endif
      end associate

      end do


!-----------   ASSOCIATE with INTEGER*2 expressions ----------------

      c2 = (a2 + b2)*10_2 + 1_2
      do count = 1, 10

      associate ( arg2 => (a2 + b2)*10_2 + 1_2 , arc2 => c2)
         if(arg2 .ne. arc2)then
           error stop 3
         endif
      end associate

      end do

!-----------   ASSOCIATE with INTEGER*4 expressions ----------------

      c4 = (a4 + b4)*10 + 1
      do count = 1, 10

      associate ( arg4 => (a4 + b4)*10 + 1 , arc4 => c4 )
         if(arg4 .ne. arc4)then
           error stop 4
         endif
      end associate

      end do

!-----------   ASSOCIATE with INTEGER*8 expressions ----------------

      c8 = (a8 + b8)*10 + 1
      do count = 1, 10

      associate ( arg8 => (a8 + b8)*10 + 1 , arc8 => c8 )
         if(arg8 .ne. arc8)then
           error stop 5
         endif
      end associate

      end do

!-----------   ASSOCIATE with BYTE expressions ----------------

      cb = (ab1 + ab2)*10_1 + 1_1
      do count = 1, 10

      associate ( arg_1 => (ab1 + ab2)*10_1 + 1_1 , arcb => cb )
         if(arg_1 .ne. arcb)then
           error stop 6
         endif
      end associate

      end do

      end
