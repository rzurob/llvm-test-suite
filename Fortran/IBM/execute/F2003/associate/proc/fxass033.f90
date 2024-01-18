!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Feb 5,2004
!*
!*  PRIMARY FUNCTIONS TESTED   : ASSOCIATE on INTRINSIC Data Types
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  KEYWORD(S)                 : ASSOCIATE,real
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
!*  NORMAL COMPLETION          : Return code = 0.0
!*  ABNORMAL COMPLETION        : Return code ^= 0
!*
!*  RUN TIME ESTIMATE          : <60 SECS
!*
!*  CONDITIONS TESTED          : Listed below.
!*
!*  DESCRIPTION                : Test: ASSOCIATE with expressions with
!*                                     real data types. calling subrotine
!*                                     and using associate in inside
!*                                     the subroutine.
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

      program fxass33a
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

      logical :: precision_r4, precision_r6, precision_r8

      c = (a + b)*10 + 10
      call real_sub(a,b,c)
         if(c .eq. 0.0)then
           error stop 10
         endif

      c16 = (a16 + b16)*10 + 10
      call real16_sub(a16,b16,c16)
         if(c16 .eq. 0.0)then
           error stop 11
         endif

      cc = (aa + bb)*10 + 10
      call double_sub(aa,bb,cc)
         if(cc .eq. 0.0)then
           error stop 12
         endif

      c4 = (a4 + b4)*10 + 10
      call real4_sub(a4,b4,c4)
         if(c4 .eq. 0.0)then
           error stop 13
         endif

      c8 = (a8 + b8)*10 + 10
      call real8_sub(a8,b8,c8)
         if(c8 .eq. 0.0)then
           error stop 14
         endif

      contains

!-----------   ASSOCIATE with REAL expressions ----------------

      subroutine real_sub(a,b,c)
            real a,b
            real, intent(out) :: c
            integer count
            logical :: precision_r4

            c = (a + b)*10 + 10
            do count = 1, 10

            associate ( arg => a )
              arg = arg + (a + b)*10
              if (.not.precision_r4(arg,a)) then
              c = 0.0
              return
              endif
            end associate

            end do

            return
      end subroutine real_sub

!-----------   ASSOCIATE with REAL16 expressions ----------------

      subroutine real16_sub(a16,b16,c16)
            real*16 a16,b16
            real*16, intent(out) :: c16
            integer count
            logical :: precision_r6

            c16 = (a16 + b16)*10 + 10
            do count = 1, 10

            associate ( arg16 => a16 )
              arg16 = arg16 + (a16 + b16)*10
              if (.not.precision_r6(arg16,a16)) then
              c16 = 0.0
              return
              endif
            end associate

            end do
            return
      end subroutine real16_sub


!-----------   ASSOCIATE with DOUBLE PRECISION expressions ----------------

      subroutine double_sub(aa,bb,cc)
            double precision aa,bb
            double precision, intent(out) :: cc
            integer count
            logical ::  precision_r6

            cc = (aa + bb)*10 + 10
            do count = 1, 10

            associate ( arg2 => aa )
              arg2 = arg2 + (aa + bb)*10
              if (.not.precision_r6(arg2,aa)) then
              cc = 0.0
              return
              endif
            end associate

            end do
            return
      end subroutine double_sub

!-----------   ASSOCIATE with REAL*4 expressions ----------------

      subroutine real4_sub(a4,b4,c4)
            real*4 a4,b4
            real*4, intent(out) :: c4
            integer count
            logical :: precision_r4

            c4 = (a4 + b4)*10 + 10
            do count = 1, 10

            associate ( arg4 => a4 )
              arg4 = arg4 + (a4 + b4)*10
              if (.not.precision_r4(arg4,a4)) then
              c4 = 0.0
              return
              endif
            end associate

            end do
            return
      end subroutine real4_sub

!-----------   ASSOCIATE with REAL*8 expressions ----------------

      subroutine real8_sub(a8,b8,c8)
            real*8 a8,b8
            real*8, intent(out) :: c8
            integer count
            logical :: precision_r8

            c8 = (a8 + b8)*10 + 10
            do count = 1, 10

            associate ( arg8 => a8 )
              arg8 = arg8 + (a8 + b8)*10
              if (.not.precision_r8(arg8,a8)) then
              c8 = 0.0
              return
              endif
            end associate

            end do
            return
      end subroutine real8_sub

      end

