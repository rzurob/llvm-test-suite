!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
!***********************************************************************
! %START
! %MAIN:
! %PRECMD: ${TR_SRC}/scrisoa01.presh fxisof24
! %COMPOPTS:
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!***********************************************************************
!***********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : Support for ISO_C_BINDING module
!*
!*  PROGRAMMER                 : Alberto Alvarez-Mesquide
!*  DATE                       : 4/23/2002
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*
!*  PRIMARY FUNCTIONS TESTED   : ISO_C_BINDING module
!*  SECONDARY FUNCTIONS TESTED : see below
!*
!*  DRIVER STANZA              :
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : C_INT_LEAST32_T, C_INT_LEAST64_T
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!*      - testing C_INT_LEAST32_T and C_INT_LEAST64_T
!*      - FORTRAN code only
!*      - passing scalar arguments by REFERENCE and by VALUE
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program fxisof24
   use ISO_C_BINDING

   interface
      integer(C_INT_LEAST32_T) function fnt1(a,b)
         use ISO_C_BINDING
         integer(C_INT_LEAST32_T) :: a
         integer(C_INT_LEAST64_T) :: b
      end function fnt1
      integer(C_INT_LEAST32_T) function fnt2(a,b)
         use ISO_C_BINDING
         integer(C_INT_LEAST32_T), value :: a
         integer(C_INT_LEAST64_T), value :: b
      end function fnt2
      integer(C_INT_LEAST32_T) function fnt3(a,b)
         use ISO_C_BINDING
         integer(C_INT_LEAST32_T), intent(in) :: a
         integer(C_INT_LEAST64_T), intent(in) :: b
      end function fnt3
      integer(C_INT_LEAST32_T) function fnt4(a,b)
         use ISO_C_BINDING
         integer(C_INT_LEAST32_T), intent(in), value :: a
         integer(C_INT_LEAST64_T), intent(in), value :: b
      end function fnt4
   end interface

   integer(C_INT_LEAST32_T) :: a, ret
   integer(C_INT_LEAST64_T) :: b

!! Test 1

   a = 5
   b = 10

   ret = fnt1(a,b)

   if ( a /= 10 ) error stop 20
   if ( b /= 20 ) error stop 22

!! Test 2

   a = 5
   b = 10

   ret = fnt2(a,b)

   if ( a /= 5 ) error stop 24
   if ( b /= 10 ) error stop 26

!! Test 3

   a = 5
   b = 10

   ret = fnt3(a,b)

   if ( a /= 5 ) error stop 28
   if ( b /= 10 ) error stop 30

!! Test 4

   a = 5
   b = 10

   ret = fnt4(a,b)

   if ( a /= 5 ) error stop 32
   if ( b /= 10 ) error stop 34

end program fxisof24

integer(C_INT_LEAST32_T) function fnt1(a,b)
   use ISO_C_BINDING

   integer(C_INT_LEAST32_T) :: a
   integer(C_INT_LEAST64_T) :: b
   
   if ( a /= 5 ) error stop 36
   if ( b /= 10 ) error stop 38

   a = a + 5
   b = b + 10

   fnt1 = a**2+b
end function fnt1

integer(C_INT_LEAST32_T) function fnt2(a,b)
   use ISO_C_BINDING

   integer(C_INT_LEAST32_T), value :: a
   integer(C_INT_LEAST64_T), value :: b
   
   if ( a /= 5 ) error stop 40
   if ( b /= 10 ) error stop 42

   a = a + 5
   b = b + 10

   fnt2 = a**2+b
end function fnt2

integer(C_INT_LEAST32_T) function fnt3(a,b)
   use ISO_C_BINDING

   integer(C_INT_LEAST32_T), intent(in) :: a
   integer(C_INT_LEAST64_T), intent(in) :: b
   
   if ( a /= 5 ) error stop 44
   if ( b /= 10 ) error stop 46

   fnt3 = a**2+b
end function fnt3

integer(C_INT_LEAST32_T) function fnt4(a,b)
   use ISO_C_BINDING

   integer(C_INT_LEAST32_T), intent(in), value :: a
   integer(C_INT_LEAST64_T), intent(in), value :: b
   
   if ( a /= 5 ) error stop 48
   if ( b /= 10 ) error stop 50

   fnt4 = a**2+b
end function fnt4
