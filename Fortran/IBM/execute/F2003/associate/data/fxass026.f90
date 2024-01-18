!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: fxass026.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : fxass026.f
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
!*  DESCRIPTION                : Test: ASSOCIATE
!*                                     with real, real*4, real*8, real*16
!*                                     double precision data types with
!*                                     intrinsic function MIN, MAX and do loop.
!*                                     using result of associate with
!*                                     expressions.
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*                    -Initial Version
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890

      program fxass26
      implicit none

      real a / 1.9 /
      real b / 4.4 /
      real c

      real*4 a4 / 9.0 /
      real*4 b4 / 2.98 /
      real*4 c4

      real*8 a8 / 9.0d0 /

      real*16 a16 / 9.9 /

      double precision aa / 1.0d0 /
      double precision bb / 4.0d0 /

      logical precision_r4, precision_r8, precision_r6
      integer count

!-----------   ASSOCIATE with REAL expressions ----------------

      c = a + (b + 1)*10
      associate ( arg => a )
         arg = arg + (b + 1)*10
      if (.not. precision_r4(arg,a)) then
           error stop 1
      endif
      end associate
      if (.not. precision_r4(a,c)) then
           error stop 2
      endif

!-----------   ASSOCIATE with REAL*4 expressions ----------------

      c4 = (b4 + 1)*10
      associate ( arg4 => a4 )
         arg4 = (b4 + 1)*10
      if (.not. precision_r4(arg4,a4)) then
           error stop 3
      endif
      end associate
      if (.not. precision_r4(a4,c4)) then
           error stop 4
      endif

!-----------   ASSOCIATE with REAL*8 expressions ----------------

      associate ( arg8 => a8 )
         arg8 = 10.0_8
      if (.not. precision_r8(arg8,a8)) then
           error stop 5
      endif
      end associate
      if (.not. precision_r8(a8,10.0_8)) then
           error stop 6
      endif

!-----------   ASSOCIATE with REAL*16 expressions ----------------

      associate ( arg16 => a16 )

      arg16 = arg16*0.0_16
      if (.not. precision_r6(arg16,a16)) then
           error stop 7
      endif

      end associate

      if (.not. precision_r6(a16,0.0_16)) then
           error stop 8
      endif

!-----------   ASSOCIATE with DOUBLE PRECISION expressions ----------------

      associate ( arg1 => aa )
         arg1 = bb
      if (.not. precision_r8(arg1,aa)) then
           error stop 9
      endif
      end associate
      if (.not. precision_r8(aa,bb)) then
           error stop 10
      endif

      end
