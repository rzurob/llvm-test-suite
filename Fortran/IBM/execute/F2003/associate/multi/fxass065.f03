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

      program fxass65a
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

      c = a + (b + 1)*10
      do count = 1, 10

      associate ( arg => a , arg1 => b , arg2 => c)
         arg = arg + (arg1 + 1)*10
         if(arg .ne. a)then
           error stop 1
         endif
      end associate

      end do

!-----------   ASSOCIATE with INTEGER*1 expressions ----------------

      c1 = a1 + (b1 + 1)*10
      do count = 1, 10

      associate ( ar => a1 , ar1 => b1 , ar2 => c1)
         ar = ar + (ar1 + 1)*10
         if(ar .ne. a1)then
           error stop 2
         endif
      end associate

      end do


!-----------   ASSOCIATE with INTEGER*2 expressions ----------------

      c2 = a2 + (b2 + 1)*10
      do count = 1, 10
      associate ( ag => a2 , ag1 => b2 , ag2 => c2)
         ag = ag + (ag1 + 1)*10
         if(ag .ne. a2)then
           error stop 3
         endif
      end associate

      end do

!-----------   ASSOCIATE with INTEGER*4 expressions ----------------

      c4 = a4 + (b4 + 1)*10
      do count = 1, 10
      associate ( rg => a4 , rg1 => b4 , rg2 => c4)
         rg = rg + (rg1 + 1)*10
         if(rg .ne. a4)then
           error stop 4
         endif
      end associate

      end do

!-----------   ASSOCIATE with INTEGER*8 expressions ----------------

      c8 = a8 + (b8 + 1)*10
      do count = 1, 10
      associate ( argo => a8 , argo1 => b8 , argo2 => c8)
         argo = argo + (argo1 + 1)*10
         if(argo .ne. a8)then
           error stop 5
         endif
      end associate

      end do

!-----------   ASSOCIATE with BYTE expressions ----------------

      cb = ab1 + (ab2 + 1)*10
      do count = 1, 10
      associate ( arg_1 => ab1 , arg1_1 => ab2 , arg2_1 => cb)
         arg_1 = arg_1 + (arg1_1 + 1)*10
         if(arg_1 .ne. ab1)then
           error stop 6
         endif
      end associate

      end do

      end
