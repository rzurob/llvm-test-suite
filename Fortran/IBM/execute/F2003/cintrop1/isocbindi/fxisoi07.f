!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
!***********************************************************************
! %START
! %MAIN:
! %PRECMD: ${TR_SRC}/scrisoi00.presh fxisoi07 cxisoi07
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
!*  KEYWORD(S)                 : C_INT_FAST16_T
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                : 
!*
!*	- testing C_INT_FAST16_T
!*	- using external FORTRAN subroutines
!*	- passing 1-dim and 2-dim array arguments
!*	- main written in C
!*
!234567890123456789012345678901234567890123456789012345678901234567890

subroutine sub1(a)
   use ISO_C_BINDING

   integer(C_INT_FAST16_T) :: a(5)
   
   do i = 1, 5
      if ( a(i) /= i ) error stop 20
      a(i) = i+1
   end do

end subroutine sub1

subroutine sub2(a)
   use ISO_C_BINDING

   integer(C_INT_FAST16_T), intent(in) :: a(5)

   do i = 1, 5
      if ( a(i) /= i ) error stop 22
   end do

end subroutine sub2

subroutine sub2a(a)
   use ISO_C_BINDING

   integer(C_INT_FAST16_T), intent(in) :: a(5)

   do i = 1, 5
      if ( a(i) /= i ) error stop 24
   end do

end subroutine sub2a

subroutine sub3(a)
   use ISO_C_BINDING

   integer(C_INT_FAST16_T), intent(inout) :: a(5)

   do i = 1, 5
      if ( a(i) /= i ) error stop 26
      a(i) = i+1
   end do

end subroutine sub3

subroutine sub4(a)
   use ISO_C_BINDING

   integer(C_INT_FAST16_T), intent(out) :: a(5)

   do i = 1, 5
      a(i) = i+1
   end do

end subroutine sub4

subroutine sub5(aa)
   use ISO_C_BINDING

   integer(C_INT_FAST16_T) :: aa(10,5)
   
   do i = 1, 5
      do j = 1, 10
         if ( aa(j,i) /= i+j-1 ) error stop 28
         aa(j,i) = i+j
      end do
   end do

end subroutine sub5

subroutine sub6(aa)
   use ISO_C_BINDING

   integer(C_INT_FAST16_T), intent(in) :: aa(10,5)

   do i = 1, 5
      do j = 1, 10
         if ( aa(j,i) /= i+j-1 ) error stop 30
      end do
   end do

end subroutine sub6

subroutine sub6a(aa)
   use ISO_C_BINDING

   integer(C_INT_FAST16_T), intent(in) :: aa(10,5)

   do i = 1, 5
      do j = 1, 10
         if ( aa(j,i) /= i+j-1 ) error stop 32
      end do
   end do

end subroutine sub6a

subroutine sub7(aa)
   use ISO_C_BINDING

   integer(C_INT_FAST16_T), intent(inout) :: aa(10,5)
   
   do i = 1, 5
      do j = 1, 10
         if ( aa(j,i) /= i+j-1 ) error stop 34
         aa(j,i) = i+j
      end do
   end do

end subroutine sub7

subroutine sub8(aa)
   use ISO_C_BINDING

   integer(C_INT_FAST16_T), intent(out) :: aa(10,5)
   
   do i = 1, 5
      do j = 1, 10
         aa(j,i) = i+j
      end do
   end do

end subroutine sub8
