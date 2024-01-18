!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Feb 5,2004
!*
!*  PRIMARY FUNCTIONS TESTED   : ASSOCIATE on INTRINSIC Data Types
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  KEYWORD(S)                 : ASSOCIATE,integer
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
!*                                     with integer, integer*1, integer*2
!*                                     integer*4, integer*8 and byte
!*                                     data types
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

      program fxass115
      implicit none

      integer, target :: a = 2
      integer, pointer :: b

      integer*4, target :: a4 = 8
      integer*4, pointer :: b4

      integer*8, target :: a8 = 10
      integer*8, pointer :: b8

      integer*1, target :: a1 = 10
      integer*1, pointer :: b1

      integer*2, target :: a2 = 10
      integer*2, pointer :: b2

      byte, target :: ab1 = 10
      byte, pointer :: ab2

      b => a
      b4 => a4
      b8 => a8
      b1 => a1
      b2 => a2
      ab2 => ab1

      associate ( arg => b )
      if(arg .ne. a)then
         error stop 1
      endif
      end associate

      associate ( arg1 => b2, arg2 => a2 )
      if(arg1 .ne. arg2)then
         error stop 2
      endif
      end associate

      associate ( arg3 => (b4 * 3) + 2 , arg4 => (a4 * 3) + 2)
      if(arg3 .ne. arg4)then
         error stop 4
      endif
      end associate

      associate ( arg5 => b1 )
      if(arg5 .ne. a1)then
         error stop 6
      endif
      end associate

      end

