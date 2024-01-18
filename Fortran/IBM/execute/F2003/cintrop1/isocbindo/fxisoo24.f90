!#######################################################################
!***********************************************************************
! %START
! %MAIN:
! %PRECMD: ${TR_SRC}/scrisoa01.presh fxisoo24
! %COMPOPTS:
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!***********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 4/23/2002
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : ISO_C_BINDING module
!*  SECONDARY FUNCTIONS TESTED : see below
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : C_LONG_DOUBLE_COMPLEX
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!*      - testing C_LONG_DOUBLE_COMPLEX
!*      - FORTRAN code only
!*      - passing scalar arguments by REFERENCE and by VALUE
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program fxisoo24
   use ISO_C_BINDING

   interface
      complex(C_LONG_DOUBLE_COMPLEX) function fnt1(a)
         use ISO_C_BINDING
         complex(C_LONG_DOUBLE_COMPLEX) :: a
      end function fnt1
      complex(C_LONG_DOUBLE_COMPLEX) function fnt2(a)
         use ISO_C_BINDING
         complex(C_LONG_DOUBLE_COMPLEX), value :: a
      end function fnt2
      complex(C_LONG_DOUBLE_COMPLEX) function fnt3(a)
         use ISO_C_BINDING
         complex(C_LONG_DOUBLE_COMPLEX), intent(in) :: a
      end function fnt3
      complex(C_LONG_DOUBLE_COMPLEX) function fnt4(a)
         use ISO_C_BINDING
         complex(C_LONG_DOUBLE_COMPLEX), intent(in), value :: a
      end function fnt4
   end interface

   complex(C_LONG_DOUBLE_COMPLEX) :: a, ret

!! Test 1

   a = (5.0d0,5.0d0)

   ret = fnt1(a)

   if ( a /= (10.0d0,10.0d0) ) error stop 20

!! Test 2

   a = (5.0d0,5.0d0)

   ret = fnt2(a)

   if ( a /= (5.0d0,5.0d0) ) error stop 22

!! Test 3

   a = (5.0d0,5.0d0)

   ret = fnt3(a)

   if ( a /= (5.0d0,5.0d0) ) error stop 24

!! Test 4

   a = (5.0d0,5.0d0)

   ret = fnt4(a)

   if ( a /= (5.0d0,5.0d0) ) error stop 26

end program fxisoo24

complex(C_LONG_DOUBLE_COMPLEX) function fnt1(a)
   use ISO_C_BINDING

   complex(C_LONG_DOUBLE_COMPLEX) :: a

   if ( a /= (5.0d0,5.0d0) ) error stop 28

   a = a + (5.0d0,5.0d0)

   fnt1 = 0
end function fnt1

complex(C_LONG_DOUBLE_COMPLEX) function fnt2(a)
   use ISO_C_BINDING

   complex(C_LONG_DOUBLE_COMPLEX), value :: a

   if ( a /= (5.0d0,5.0d0) ) error stop 30

   a = a + (5.0d0,5.0d0)

   fnt2 = 0
end function fnt2

complex(C_LONG_DOUBLE_COMPLEX) function fnt3(a)
   use ISO_C_BINDING

   complex(C_LONG_DOUBLE_COMPLEX), intent(in) :: a

   if ( a /= (5.0d0,5.0d0) ) error stop 32

   fnt3 = 0
end function fnt3

complex(C_LONG_DOUBLE_COMPLEX) function fnt4(a)
   use ISO_C_BINDING

   complex(C_LONG_DOUBLE_COMPLEX), intent(in), value :: a

   if ( a /= (5.0d0,5.0d0) ) error stop 34

   fnt4 = 0
end function fnt4
