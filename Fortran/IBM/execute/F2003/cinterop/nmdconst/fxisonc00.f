!***********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 4/23/2002
!*
!*  PRIMARY FUNCTIONS TESTED   : ISO_C_BINDING module
!*  SECONDARY FUNCTIONS TESTED : see below
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : ISO_C_BINDING module defined named constants
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!*       - testing values for the ISO_C_BINDING module defined named constants:
!*         C_NULL_CHAR, C_ALERT, C_BACKSPACE, C_FORM_FEED, C_NEW_LINE
!*         C_CARRIAGE_RETURN, C_HORIZONTAL_TAB, C_VERTICAL_TAB
!*         C_NULL_PTR
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program fxisonc00
   use ISO_C_BINDING

   interface
      integer function fun1(x,y)
         character :: x, y
      end function fun1
      integer function fun2(x,y)
         character :: x, y
      end function fun2
      integer function fun3(x,y)
         character :: x, y
      end function fun3
      integer function fun4(x,y)
         character :: x, y
      end function fun4
      integer function fun5(x,y)
         character :: x, y
      end function fun5
      integer function fun6(x,y)
         character :: x, y
      end function fun6
      integer function fun7(x,y)
         character :: x, y
      end function fun7
      integer function fun8(x,y)
         character :: x, y
      end function fun8
      integer function fun9(x)
         character :: x(7)
      end function fun9
      integer function fun10(x)
         use ISO_C_BINDING, ONLY : C_PTR
         type(C_PTR) :: x
      end function fun10
   end interface

   character(C_CHAR) :: a, b
   character(C_CHAR) :: c(7)
   type(C_PTR) :: p
   integer ret

   a = C_NULL_CHAR
   ret = fun1(a,b)
   if ( b /= C_NULL_CHAR ) error stop 20

   a = C_ALERT
   ret = fun2(a,b)
   if ( b /= C_ALERT ) error stop 22

   a = C_BACKSPACE
   ret = fun3(a,b)
   if ( b /= C_BACKSPACE ) error stop 24

   a = C_FORM_FEED
   ret = fun4(a,b)
   if ( b /= C_FORM_FEED ) error stop 26

   a = C_NEW_LINE
   ret = fun5(a,b)
   if ( b /= C_NEW_LINE ) error stop 28

   a = C_CARRIAGE_RETURN
   ret = fun6(a,b)
   if ( b /= C_CARRIAGE_RETURN ) error stop 30

   a = C_HORIZONTAL_TAB
   ret = fun7(a,b)
   if ( b /= C_HORIZONTAL_TAB ) error stop 32

   a = C_VERTICAL_TAB
   ret = fun8(a,b)
   if ( b /= C_VERTICAL_TAB ) error stop 34

   c(1) = 's'
   c(2) = 't'
   c(3) = 'r'
   c(4) = 'i'
   c(5) = 'n'
   c(6) = 'g'
   c(7) = '\0'
   ret = fun9(c)

   p = C_NULL_PTR
   ret = fun10(p)

end program fxisonc00
