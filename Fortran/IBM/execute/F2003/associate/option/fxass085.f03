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
!*  DESCRIPTION                : Test: ASSOCIATE with expressions and with
!*                                     with integer data types with do loop
!*                                     using different -qintsize options.
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

      program fxass85a
      implicit none

      integer a / 1 /
      integer b / 4 /
      integer c

      integer a1 / 2 /
      integer b1 / 8 /
      integer c1

      integer a2 / 5 /
      integer b2 / 4 /
      integer c2

      integer a4 / 9 /
      integer b4 / 2 /
      integer c4

      integer a8 / 9 /
      integer b8 / 2 /
      integer c8

      integer count

!-----------   ASSOCIATE with INTEGER expressions ----------------

      c = a + (b + 1)
      do count = 1, 10

      associate ( arg => a )
         arg = arg + (b + c)
         if(arg .ne. a)then
           error stop 1
         endif
      end associate

      end do

!-----------   ASSOCIATE with INTEGER expressions ----------------

      c1 = a1 + (b1 + 1)
      do count = 1, 10

      associate ( arg1 => a1 )
         arg1 = arg1 + (b1 + c1)
         if(arg1 .ne. a1)then
           error stop 2
         endif
      end associate

      end do


!-----------   ASSOCIATE with INTEGER expressions ----------------

      c2 = a2 + (b2 + 1)
      do count = 1, 10

      associate ( arg2 => a2 )
         arg2 = arg2 + (b2 + c2)
         if(arg2 .ne. a2)then
           error stop 3
         endif
      end associate

      end do

!-----------   ASSOCIATE with INTEGER expressions ----------------

      c4 = a4 + (b4 + 1)
      do count = 1, 10

      associate ( arg4 => a4 )
         arg4 = arg4 + (b4 + c4)
         if(arg4 .ne. a4)then
           error stop 4
         endif
      end associate

      end do

!-----------   ASSOCIATE with INTEGER expressions ----------------

      c8 = a8 + (b8 + 1)
      do count = 1, 10

      associate ( arg8 => a8 )
         arg8 = arg8 + (b8 + c8)
         if(arg8 .ne. a8)then
           error stop 5
         endif
      end associate

      end do

      end
