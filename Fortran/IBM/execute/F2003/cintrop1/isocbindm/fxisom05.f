!#######################################################################
!***********************************************************************
! %START
! %MAIN:
! %PRECMD: ${TR_SRC}/scrisoa00.presh fxisom05 cxisom04
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
!*  KEYWORD(S)                 : C_LONG_DOUBLE
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!*      - testing C_LONG_DOUBLE
!*      - using C functions with interface to FORTRAN subroutines
!*      - subroutines interfaces defined in module
!*      - passing scalar arguments by REFERENCE and by VALUE
!*      - main written in C
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module mxisob05
   interface
      subroutine sub1(a)
         use ISO_C_BINDING
         real(C_LONG_DOUBLE) :: a
      end subroutine sub1
      subroutine sub2(a)
         use ISO_C_BINDING
         real(C_LONG_DOUBLE), value :: a
      end subroutine sub2
      subroutine sub3(a)
         use ISO_C_BINDING
         real(C_LONG_DOUBLE), intent(in) :: a
      end subroutine sub3
      subroutine sub4(a)
         use ISO_C_BINDING
         real(C_LONG_DOUBLE), intent(in), value :: a
      end subroutine sub4
   end interface
end module mxisob05

program fxisom05
   use ISO_C_BINDING
   use mxisob05

   real(C_LONG_DOUBLE) :: a, ret

!! Test 1

   a = 5.0d0

   call sub1(a)

   if ( a /= 10.0d0 ) error stop 20

!! Test 2

   a = 5.0d0

   call sub2(a)

   if ( a /= 5.0d0 ) error stop 22

!! Test 3

   a = 5.0d0

   call sub3(a)

   if ( a /= 5.0d0 ) error stop 24

!! Test 4

   a = 5.0d0

   call sub4(a)

   if ( a /= 5.0d0 ) error stop 26

end program fxisom05
