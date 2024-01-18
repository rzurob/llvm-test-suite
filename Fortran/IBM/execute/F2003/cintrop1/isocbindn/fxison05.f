!#######################################################################
!***********************************************************************
! %START
! %MAIN:
! %PRECMD: ${TR_SRC}/scrison00.presh fxison05 cxison04
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
!*  KEYWORD(S)                 : C_FLOAT_COMPLEX, C_DOUBLE_COMPLEX
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!*      - testing C_FLOAT_COMPLEX and C_DOUBLE_COMPLEX
!*      - using C functions with interface to FORTRAN subroutines
!*      - subroutines interfaces defined in module
!*      - passing scalar arguments by REFERENCE and by VALUE
!*      - main written in C
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module mxisob05
   interface
      subroutine sub1(a,b)
         use ISO_C_BINDING
         complex(C_FLOAT_COMPLEX) :: a
         complex(C_DOUBLE_COMPLEX) :: b
      end subroutine sub1
      subroutine sub2(a,b)
         use ISO_C_BINDING
         complex(C_FLOAT_COMPLEX), value :: a
         complex(C_DOUBLE_COMPLEX), value :: b
      end subroutine sub2
      subroutine sub3(a,b)
         use ISO_C_BINDING
         complex(C_FLOAT_COMPLEX), intent(in) :: a
         complex(C_DOUBLE_COMPLEX), intent(in) :: b
      end subroutine sub3
      subroutine sub4(a,b)
         use ISO_C_BINDING
         complex(C_FLOAT_COMPLEX), intent(in), value :: a
         complex(C_DOUBLE_COMPLEX), intent(in), value :: b
      end subroutine sub4
   end interface
end module mxisob05

program fxison05
   use ISO_C_BINDING
   use mxisob05

   complex(C_FLOAT_COMPLEX) :: a, ret
   complex(C_DOUBLE_COMPLEX) :: b

!! Test 1

   a = (5.0e0,5.0e0)
   b = (10.0d0,10.0d0)

   call sub1(a,b)

   if ( a /= (10.0e0,10.0e0) ) error stop 20
   if ( b /= (20.0d0,20.0d0) ) error stop 22

!! Test 2

   a = (5.0e0,5.0e0)
   b = (10.0d0,10.0d0)

   call sub2(a,b)

   if ( a /= (5.0e0,5.0e0) ) error stop 24
   if ( b /= (10.0d0,10.0d0) ) error stop 26

!! Test 3

   a = (5.0e0,5.0e0)
   b = (10.0d0,10.0d0)

   call sub3(a,b)

   if ( a /= (5.0e0,5.0e0) ) error stop 28
   if ( b /= (10.0d0,10.0d0) ) error stop 30

!! Test 4

   a = (5.0e0,5.0e0)
   b = (10.0d0,10.0d0)

   call sub4(a,b)

   if ( a /= (5.0e0,5.0e0) ) error stop 32
   if ( b /= (10.0d0,10.0d0) ) error stop 34

end program fxison05
