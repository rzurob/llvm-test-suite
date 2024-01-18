!#######################################################################
!***********************************************************************
! %START
! %MAIN:
! %PRECMD: ${TR_SRC}/scrisoa00.presh fxisok07 cxisok07
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
!*  KEYWORD(S)                 : C_CHAR, C_SIGNED_CHAR
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!*	- testing C_CHAR and C_SIGNED_CHAR
!*	- using external FORTRAN subroutines
!*	- passing 1-dim and 2-dim array arguments
!*	- main written in C
!*
!234567890123456789012345678901234567890123456789012345678901234567890

subroutine sub1(a,b)
   use ISO_C_BINDING

   character(C_CHAR) :: a(4)
   integer(C_SIGNED_CHAR) :: b(4)

   do i = 1, 4
      if ( a(i) /= achar(iachar('A')+i-1) ) error stop 20
      a(i) = achar(iachar('A')+i+3)
      if ( b(i) /= iachar('A')+i-1 ) error stop 22
      b(i) = iachar('A')+i+3
   end do

end subroutine sub1

subroutine sub2(a,b)
   use ISO_C_BINDING

   character(C_CHAR), intent(in) :: a(4)
   integer(C_SIGNED_CHAR), intent(in) :: b(4)

   do i = 1, 4
      if ( a(i) /= achar(iachar('A')+i-1) ) error stop 24
      if ( b(i) /= iachar('A')+i-1 ) error stop 26
   end do

end subroutine sub2

subroutine sub2a(a,b)
   use ISO_C_BINDING

   character(C_CHAR), intent(in) :: a(4)
   integer(C_SIGNED_CHAR), intent(in) :: b(4)

   do i = 1, 4
      if ( a(i) /= achar(iachar('A')+i-1) ) error stop 28
      if ( b(i) /= iachar('A')+i-1 ) error stop 30
   end do

end subroutine sub2a

subroutine sub3(a,b)
   use ISO_C_BINDING

   character(C_CHAR), intent(inout) :: a(4)
   integer(C_SIGNED_CHAR), intent(inout) :: b(4)

   do i = 1, 4
      if ( a(i) /= achar(iachar('A')+i-1) ) error stop 32
      a(i) = achar(iachar('A')+i+3)
      if ( b(i) /= iachar('A')+i-1 ) error stop 34
      b(i) = iachar('A')+i+3
   end do

end subroutine sub3

subroutine sub4(a,b)
   use ISO_C_BINDING

   character(C_CHAR), intent(out) :: a(4)
   integer(C_SIGNED_CHAR), intent(out) :: b(4)

   do i = 1, 4
      a(i) = achar(iachar('A')+i+3)
      b(i) = iachar('A')+i+3
   end do

end subroutine sub4

subroutine sub5(aa,bb)
   use ISO_C_BINDING

   character(C_CHAR) :: aa(6,4)
   integer(C_SIGNED_CHAR) :: bb(6,4)

   do i = 1, 4
      do j = 1, 6
         if ( aa(j,i) /= achar(iachar('A')+(i-1)*6+(j-1)) ) error stop 36
         aa(j,i) = achar(iachar('A')+(i-1)*6+j)
         if ( bb(j,i) /= iachar('A')+(i-1)*6+(j-1) ) error stop 38
         bb(j,i) = iachar('A')+(i-1)*6+j
      end do
   end do

end subroutine sub5

subroutine sub6(aa,bb)
   use ISO_C_BINDING

   character(C_CHAR), intent(in) :: aa(6,4)
   integer(C_SIGNED_CHAR), intent(in) :: bb(6,4)

   do i = 1, 4
      do j = 1, 6
         if ( aa(j,i) /= achar(iachar('A')+(i-1)*6+(j-1)) ) error stop 40
         if ( bb(j,i) /= iachar('A')+(i-1)*6+(j-1) ) error stop 42
      end do
   end do

end subroutine sub6

subroutine sub6a(aa,bb)
   use ISO_C_BINDING

   character(C_CHAR), intent(in) :: aa(6,4)
   integer(C_SIGNED_CHAR), intent(in) :: bb(6,4)

   do i = 1, 4
      do j = 1, 6
         if ( aa(j,i) /= achar(iachar('A')+(i-1)*6+(j-1)) ) error stop 44
         if ( bb(j,i) /= iachar('A')+(i-1)*6+(j-1) ) error stop 46
      end do
   end do

end subroutine sub6a

subroutine sub7(aa,bb)
   use ISO_C_BINDING

   character(C_CHAR), intent(inout) :: aa(6,4)
   integer(C_SIGNED_CHAR), intent(inout) :: bb(6,4)

   do i = 1, 4
      do j = 1, 6
         if ( aa(j,i) /= achar(iachar('A')+(i-1)*6+(j-1)) ) error stop 48
         aa(j,i) = achar(iachar('A')+(i-1)*6+j)
         if ( bb(j,i) /= iachar('A')+(i-1)*6+(j-1) ) error stop 50
         bb(j,i) = iachar('A')+(i-1)*6+j
      end do
   end do

end subroutine sub7

subroutine sub8(aa,bb)
   use ISO_C_BINDING

   character(C_CHAR), intent(out) :: aa(6,4)
   integer(C_SIGNED_CHAR), intent(out) :: bb(6,4)

   do i = 1, 4
      do j = 1, 6
         aa(j,i) = achar(iachar('A')+(i-1)*6+j)
         bb(j,i) = iachar('A')+(i-1)*6+j
      end do
   end do

end subroutine sub8
