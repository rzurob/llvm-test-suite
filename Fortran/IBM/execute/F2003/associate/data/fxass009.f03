!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Feb 5,2004
!*
!*  PRIMARY FUNCTIONS TESTED   : ASSOCIATE on INTRINSIC Data Types
!*  SECONDARY FUNCTIONS TESTED : None
!*
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
!*  DESCRIPTION                : Test: ASSOCIATE with expressions
!*                                     with real, real*4, real*8, real*16
!*                                     double precision data types with
!*                                     do loop.
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*                    -Initial Version
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890

      program fxass09
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
      integer count

!-----------   ASSOCIATE with REAL expressions ----------------

      c = (a + b)*10 + 10.0
      do count = 1, 10

      associate ( arg => (a + b)*10 + 10.0 )
      if (.not. precision_r4(arg,c)) then
           error stop 1
      endif
      end associate

      end do

!-----------   ASSOCIATE with REAL*4 expressions ----------------

      c4 = (a4 + b4)*10 + 1
      do count = 1, 10

      associate ( arg4 => (a4 + b4)*10 + 1 )
      if (.not. precision_r4(arg4,c4)) then
           error stop 4
      endif
      end associate

      end do

!-----------   ASSOCIATE with REAL*8 expressions ----------------

      c8 = (a8 + b8)*10 + 1
      do count = 1, 10

      associate ( arg8 => (a8 + b8)*10 + 1 )
      if (.not. precision_r8(arg8,c8)) then

           error stop 5
      endif
      end associate

      end do

!-----------   ASSOCIATE with REAL*16 expressions ----------------

      c16 = (a16 + b16)*10 + 1
      do count = 1, 10

      associate ( arg16 => (a16 + b16)*10 + 1 )
      if (.not. precision_r6(arg16,c16)) then
           error stop 6
      endif
      end associate

      end do

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
