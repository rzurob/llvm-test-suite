!**********************************************************************
!*  ===================================================================
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
!*  DESCRIPTION                : Test: ASSOCIATE
!*                                     with integer, integer*1, integer*2
!*                                     integer*4, integer*8 and byte
!*                                     data types with intrinsic
!*                                     function MOD and do loop.using
!*                                     associate result with expressions.
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*                    -Initial Version
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890

      program fxass01a
      implicit none

      integer a / 1 /
      integer b / 4 /

      integer*1 a1 / 2 /
      integer*1 b1 / 8 /

      integer*2 a2 / 5 /
      integer*2 b2 / 4 /

      integer*4 a4 / 9 /
      integer*4 b4 / 2 /

      integer*8 a8 / 9 /
      integer*8 b8 / 2 /

      byte ab1 / 1 /
      byte ab2 / 4 /

      integer count

!---- ASSOCIATE with INTEGER expressions & intrinsic function -----

      associate ( arg => MOD(a,b) + MOD(9,2) )
         if(arg .ne. (MOD(a,b) + MOD(9,2)))then
           error stop 11
         endif
      end associate

!-----------   ASSOCIATE with INTEGER expressions ----------------

      do count = 1, 10

      associate ( arg => a )
         arg = arg + (b + 1)*10
         if(arg .ne. a)then
           error stop 1
         endif
      end associate

      end do

!-----------   ASSOCIATE with INTEGER*1 expressions ----------------

      do count = 1, 10

      associate ( arg1 => a1 )
         arg1 = arg1 + (b1 + 1)*10
         if(arg1 .ne. a1)then
           error stop 2
         endif
      end associate

      end do


!-----------   ASSOCIATE with INTEGER*2 expressions ----------------

      do count = 1, 10

      associate ( arg2 => a2 )
         arg2 = arg2 + (b2 + 1)*10
         if(arg2 .ne. a2)then
           error stop 3
         endif
      end associate

      end do

!-----------   ASSOCIATE with INTEGER*4 expressions ----------------

      do count = 1, 10

      associate ( arg4 => a4 )
         arg4 = arg4 + (b4 + 1)*10
         if(arg4 .ne. a4)then
           error stop 4
         endif
      end associate

      end do

!-----------   ASSOCIATE with INTEGER*8 expressions ----------------

      do count = 1, 10

      associate ( arg8 => a8 )
         arg8 = arg8 + (b8 + 1)*10
         if(arg8 .ne. a8)then
           error stop 5
         endif
      end associate

      end do

!-----------   ASSOCIATE with BYTE expressions ----------------

      do count = 1, 10

      associate ( arg_1 => ab1 )
         arg_1 = arg_1 + (ab2 + 1)*10
         if(arg_1 .ne. ab1)then
           error stop 6
         endif
      end associate

      end do

      end
