!#######################################################################
!***********************************************************************
! %START
! %MAIN:
! %PRECMD: ${TR_SRC}/scrisor00.presh fxisor05 cxisor04
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
!*  KEYWORD(S)                 : C_BOOL
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!*      - testing C_BOOL
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
         logical(C_BOOL) :: a
      end subroutine sub1
      subroutine sub2(a)
         use ISO_C_BINDING
         logical(C_BOOL), value :: a
      end subroutine sub2
      subroutine sub3(a)
         use ISO_C_BINDING
         logical(C_BOOL), intent(in) :: a
      end subroutine sub3
      subroutine sub4(a)
         use ISO_C_BINDING
         logical(C_BOOL), intent(in), value :: a
      end subroutine sub4
   end interface
end module mxisob05

program fxisor05
   use ISO_C_BINDING
   use mxisob05

   logical(C_BOOL) :: a, ret

!! Test 1

   a = .true.

   call sub1(a)

   if ( a .eqv. .true. ) error stop 20

!! Test 2

   a = .true.

   call sub2(a)

   if ( a .neqv. .true. ) error stop 22

!! Test 3

   a = .true.

   call sub3(a)

   if ( a .neqv. .true. ) error stop 24

!! Test 4

   a = .true.

   call sub4(a)

   if ( a .neqv. .true. ) error stop 26

end program fxisor05
