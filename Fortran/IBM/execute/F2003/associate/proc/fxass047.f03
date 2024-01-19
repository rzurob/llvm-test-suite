!**********************************************************************
!*  ===================================================================
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
!*                                     real data types and module.
!*                                     recursive funtion with using associate.
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*                    -Initial Version
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890

      module act
      interface
      recursive function real_fun(a) result (c)
            real a
            real :: c
      end function

      recursive function real4_fun(a4) result (c4)
            real*4 a4
            real*4 :: c4
      end function

      recursive function real8_fun(a8) result (c8)
            real*8 a8
            real*8 :: c8
      end function

      recursive function real16_fun(a16) result (c16)
            real*16 a16
            real*16 :: c16
      end function

      end interface
      end module

      program fxass42a
      use act
      implicit none

      real a / 3.0 /
      real c / 4.4 /

      real*4 a4 / 4.0 /
      real*4 c4 / 2.98 /

      real*8 a8 / 4.0d0 /
      real*8 c8 / 2.09 /

      real*16 a16 / 5.0 /
      real*16 c16 / 2.0q0 /


      c = real_fun(a)
         if(c .eq. 0.0)then
           error stop 10
         endif

      c4 = real4_fun(a4)
         if(c4 .eq. 0.0)then
           error stop 13
         endif

      c8 = real8_fun(a8)
         if(c8 .eq. 0.0)then
           error stop 14
         endif

      c16 = real16_fun(a16)
         if(c16 .eq. 0.0)then
           error stop 15
         endif

   end
!-----------   ASSOCIATE with REAL expressions ----------------

      recursive function real_fun(a) result (c)
            real a,b
            real :: c
            logical :: precision_r4, precision_r6, precision_r8
            b = 4.1
            if ( a .le. 1.0) then
              associate ( arg => a*2 + b )
              if (.not.precision_r4(arg,(a*2 + b))) then
              c = 0.0
              return
              endif
              end associate
              c = a
            else
              c = real_fun( a - 1.0 )
            end if

            return
      end function real_fun

!-----------   ASSOCIATE with REAL*4 expressions ----------------

      recursive function real4_fun(a4) result (c4)
            real*4 a4,b4
            real*4 :: c4
            logical :: precision_r4, precision_r6, precision_r8
            b4 = 7.54
            if ( a4 .le. 1.0 ) then
              associate ( arg4 => a4*2 + b4 )
              if (.not.precision_r4(arg4,(a4*2 + b4))) then
              c4 = 0.0
              return
              endif
              end associate
              c4 = a4
            else
              c4 = real4_fun( a4 - 1.0 )
            end if
            return
      end function real4_fun

!-----------   ASSOCIATE with REAL*8 expressions ----------------

      recursive function real8_fun(a8) result (c8)
            real*8 a8,b8
            real*8 :: c8
            logical :: precision_r4, precision_r6, precision_r8
            b8 = 8.4
            if ( a8 .le. 1.0 ) then
              associate ( arg8 => a8*2 + b8 )
              if (.not.precision_r8(arg8,(a8*2 + b8))) then
              c8 = 0.0
              return
              endif
              end associate
              c8 = a8
            else
              c8 = real8_fun( a8 - 1.0 )
            end if
            return
      end function real8_fun

!-----------   ASSOCIATE with REAL*16 expressions ----------------

      recursive function real16_fun(ab1) result (cb)
            real*16 ab1,ab2
            real*16 cb
            logical :: precision_r4, precision_r6, precision_r8
            ab2 = 2.4
            if ( ab1 .le. 1.0 ) then
              associate ( arg_1 => ab1*2 + ab2 )
              if (.not.precision_r6(arg_1,(ab1*2 + ab2))) then
              cb = 0.0
              return
              endif
              end associate
              cb = ab1
            else
              cb = real16_fun( ab1 - 1.0 )
            end if
            return
      end function real16_fun
