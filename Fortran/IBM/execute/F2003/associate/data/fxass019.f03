!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Feb 5,2004
!*
!*  PRIMARY FUNCTIONS TESTED   : ASSOCIATE on INTRINSIC Data Types
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  DESCRIPTION                : ASSOCIATE with REAL expressions
!*                               with nested do loop and do while
!*                               with cos intrinsic function
!*  KEYWORD(S)                 : ASSOCIATE, real
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
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*                    -Initial Version
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890

      program fxass19
      implicit none

      real a / 1.9 /
      real b / 4.4 /
      real c

      real*4 a4 / 9.0 /
      real*4 b4 / 2.98 /
      real*4 c4

      real*8 a8 / 9.0d0 /
      real*8 b8 / 2.09 /
      real*8 c8

      real*16 a16 / 9.9 /
      real*16 b16 / 2.0q0 /
      real*16 c16

      double precision aa / 1.0d0 /
      double precision bb / 4.0d0 /
      double precision cc

      logical precision_r4, precision_r8, precision_r6
      integer count, k

!-----------   ASSOCIATE with REAL expressions ----------------

      k = 1
      c = (a + b)*10 + 10.0
      do count = 1, 10
         do while ( K .le. 5 )
      assoc1: associate ( arg => (a + b)*10 + 10.0 )
         if (.not. precision_r4(arg,c)) then
           error stop 1
         endif
      end associate assoc1
         k = k + 1
         end do
      end do

!-----------   ASSOCIATE with REAL*4 expressions ----------------
      k = 1
      c4 = cos(1.0) + 2
      do 15 count = 1, 10
      first: do while ( K .le. 5 )
      associate ( arg4 => cos(1.0) + 2 )
         if (.not. precision_r4(arg4,c4)) then
           error stop 4
         endif
      end associate
      k = k + 1
      end do first
 15   end do

!-----------   ASSOCIATE with REAL*8 expressions ----------------

      do count = 1, 10
         do k = 1, 5

      associate ( arg8 => cos(1.0) )
         if (.not. precision_r4(arg8,cos(1.0))) then
           error stop 5
         endif
      end associate

         end do
      end do

!-----------   ASSOCIATE with REAL*16 expressions ----------------

      c16 = (a16 + b16)*10 + 1
      loop1: do count = 1, 10
      loop2: do k = 1,5
      associate ( arg16 => (a16 + b16)*10 + 1 )
         if (.not. precision_r6(arg16,c16)) then
           error stop 6
         endif
      end associate

      end do loop2
      end do loop1

!-----------   ASSOCIATE with DOUBLE PRECISION expressions ----------------

      cc = (aa + bb)*10 + 1
      do count = 1, 10

      associate ( arg1 => (aa + bb)*10 + 1 )
         if (.not. precision_r8(arg1,cc)) then
           error stop 7
         endif
      end associate

      end do

      end
