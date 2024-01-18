!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
!***********************************************************************
! %START
! %MAIN:
! %PRECMD: ${TR_SRC}/scrisoa00.presh fxisod07 cxisod07
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
!*  KEYWORD(S)                 : C_INT32_T, C_INT64_T
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                : 
!*
!*	- testing C_INT32_T and C_INT64_T
!*	- using external FORTRAN subroutines
!*	- passing 1-dim and 2-dim array arguments
!*	- main written in C
!*
!234567890123456789012345678901234567890123456789012345678901234567890

subroutine sub1(a,b)
   use ISO_C_BINDING

   integer(C_INT32_T) :: a(5)
   integer(C_INT64_T) :: b(5)
   
   do i = 1, 5
      if ( a(i) /= i ) error stop 20
      a(i) = i+1
      if ( b(i) /= i ) error stop 22
      b(i) = i+1
   end do

end subroutine sub1

subroutine sub2(a,b)
   use ISO_C_BINDING

   integer(C_INT32_T), intent(in) :: a(5)
   integer(C_INT64_T), intent(in) :: b(5)

   do i = 1, 5
      if ( a(i) /= i ) error stop 24
      if ( b(i) /= i ) error stop 26
   end do

end subroutine sub2

subroutine sub2a(a,b)
   use ISO_C_BINDING

   integer(C_INT32_T), intent(in) :: a(5)
   integer(C_INT64_T), intent(in) :: b(5)

   do i = 1, 5
      if ( a(i) /= i ) error stop 28
      if ( b(i) /= i ) error stop 30
   end do

end subroutine sub2a

subroutine sub3(a,b)
   use ISO_C_BINDING

   integer(C_INT32_T), intent(inout) :: a(5)
   integer(C_INT64_T), intent(inout) :: b(5)

   do i = 1, 5
      if ( a(i) /= i ) error stop 32
      a(i) = i+1
      if ( b(i) /= i ) error stop 34
      b(i) = i+1
   end do

end subroutine sub3

subroutine sub4(a,b)
   use ISO_C_BINDING

   integer(C_INT32_T), intent(out) :: a(5)
   integer(C_INT64_T), intent(out) :: b(5)

   do i = 1, 5
      a(i) = i+1
      b(i) = i+1
   end do

end subroutine sub4

subroutine sub5(aa,bb)
   use ISO_C_BINDING

   integer(C_INT32_T) :: aa(10,5)
   integer(C_INT64_T) :: bb(10,5)
   
   do i = 1, 5
      do j = 1, 10
         if ( aa(j,i) /= i+j-1 ) error stop 36
         aa(j,i) = i+j
         if ( bb(j,i) /= i+j-1 ) error stop 38
         bb(j,i) = i+j
      end do
   end do

end subroutine sub5

subroutine sub6(aa,bb)
   use ISO_C_BINDING

   integer(C_INT32_T), intent(in) :: aa(10,5)
   integer(C_INT64_T), intent(in) :: bb(10,5)

   do i = 1, 5
      do j = 1, 10
         if ( aa(j,i) /= i+j-1 ) error stop 40
         if ( bb(j,i) /= i+j-1 ) error stop 42
      end do
   end do

end subroutine sub6

subroutine sub6a(aa,bb)
   use ISO_C_BINDING

   integer(C_INT32_T), intent(in) :: aa(10,5)
   integer(C_INT64_T), intent(in) :: bb(10,5)

   do i = 1, 5
      do j = 1, 10
         if ( aa(j,i) /= i+j-1 ) error stop 44
         if ( bb(j,i) /= i+j-1 ) error stop 46
      end do
   end do

end subroutine sub6a

subroutine sub7(aa,bb)
   use ISO_C_BINDING

   integer(C_INT32_T), intent(inout) :: aa(10,5)
   integer(C_INT64_T), intent(inout) :: bb(10,5)
   
   do i = 1, 5
      do j = 1, 10
         if ( aa(j,i) /= i+j-1 ) error stop 48
         aa(j,i) = i+j
         if ( bb(j,i) /= i+j-1 ) error stop 50
         bb(j,i) = i+j
      end do
   end do

end subroutine sub7

subroutine sub8(aa,bb)
   use ISO_C_BINDING

   integer(C_INT32_T), intent(out) :: aa(10,5)
   integer(C_INT64_T), intent(out) :: bb(10,5)
   
   do i = 1, 5
      do j = 1, 10
         aa(j,i) = i+j
         bb(j,i) = i+j
      end do
   end do

end subroutine sub8
