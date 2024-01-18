!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: fxass038.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : fxass038.f
!*
!*  DATE                       : Feb 5,2004
!*
!*  PRIMARY FUNCTIONS TESTED   : ASSOCIATE on INTRINSIC Data Types
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  KEYWORD(S)                 : ASSOCIATE,real,real*16, RECURSIVE
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
!*                                     real data types. calling recursive
!*                                     subrouitne .using associate in inside
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

      program fxass38a
      implicit none

      interface
      recursive subroutine real_sub(a,c)
            real a
            real, intent(out) :: c
      end subroutine

      recursive subroutine real4_sub(a4,c4)
            real*4 a4
            real*4, intent(out) :: c4
      end subroutine

      recursive subroutine real8_sub(a8,c8)
            real*8 a8
            real*8, intent(out) :: c8
      end subroutine

      recursive subroutine real16_sub(a16,c16)
            real*16 a16
            real*16, intent(out) :: c16
      end subroutine

      end interface

      real a / 3.0 /
      real c / 4.4 /

      real*4 a4 / 4.0 /
      real*4 c4 / 2.98 /

      real*8 a8 / 9.0d0 /
      real*8 c8 / 2.09 /

      real*16 a16 / 5.0 /
      real*16 c16 / 2.0q0 /

      logical :: precision_r4, precision_r6, precision_r8

      call real_sub(a,c)
         if(c .eq. 0.0)then
           error stop 10
         endif

      call real4_sub(a4,c4)
         if(c4 .eq. 0.0)then
           error stop 13
         endif

      call real8_sub(a8,c8)
         if(c8 .eq. 0.0_8)then
           error stop 14
         endif

      call real16_sub(a16,c16)
         if(c16 .eq. 0.0_16)then
           error stop 15
         endif

   end
!-----------   ASSOCIATE with REAL expressions ----------------

      recursive subroutine real_sub(a,c)
            real a
            real, intent(out) :: c
            logical :: precision_r4, precision_r6, precision_r8

            c = a*2 + 1.0
            if ( a .le. 1.0) then
              associate ( arg => a*2 + 1.0 )
              if (.not.precision_r4(arg,c)) then
              c = 0.0
              return
              endif
              end associate
              c = a
            else
              call real_sub( a - 1.0, c )
            end if

            return
      end subroutine real_sub

!-----------   ASSOCIATE with REAL*4 expressions ----------------

      recursive subroutine real4_sub(a4,c4)
            real*4 a4
            real*4, intent(out) :: c4
            logical :: precision_r4, precision_r6, precision_r8

            if ( a4 .le. 1.0 ) then
              associate ( arg4 => a4*2 )
              if (.not.precision_r4(arg4,a4*2)) then
              c4 = 0.0
              return
              endif
              end associate
              c4 = a4
            else
              call real4_sub( a4 - 1.0, c4 )
            end if
            return
      end subroutine real4_sub

!-----------   ASSOCIATE with REAL*8 expressions ----------------

      recursive subroutine real8_sub(a8,c8)
            real*8 a8
            real*8, intent(out) :: c8
            logical :: precision_r4, precision_r6, precision_r8

            if ( a8 .le. 1.0_8 ) then
              associate ( arg8 => a8*2 + c8 )
              if (.not.precision_r8(arg8,(a8*2 + c8))) then
              c8 = 0.0_8
              return
              endif
              end associate
              c8 = a8
            else
              call real8_sub( a8 - 1.0_8, c8 )
            end if
            return
      end subroutine real8_sub

!-----------   ASSOCIATE with REAL*16 expressions ----------------

      recursive subroutine real16_sub(ab1,cb)
            real*16 ab1
            real*16, intent(out) :: cb
            logical :: precision_r4, precision_r6, precision_r8

            if ( ab1 .le. 1.0_16 ) then
              associate ( arg_1 => ab1*2 + cb )
              if (.not.precision_r6(arg_1,(ab1*2 + cb))) then
              cb = 0.0_16
              return
              endif
              end associate
              cb = ab1
            else
              call real16_sub( ab1 - 1.0_16, cb )
            end if
            return
      end subroutine real16_sub
