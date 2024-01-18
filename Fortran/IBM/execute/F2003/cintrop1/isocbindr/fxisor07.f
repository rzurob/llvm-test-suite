!#######################################################################
!***********************************************************************
! %START
! %MAIN:
! %PRECMD: ${TR_SRC}/scrisor00.presh fxisor07 cxisor07
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
!*	- testing C_BOOL
!*	- using external FORTRAN subroutines
!*	- passing 1-dim and 2-dim array arguments
!*	- main written in C
!*
!234567890123456789012345678901234567890123456789012345678901234567890

subroutine sub1(a)
   use ISO_C_BINDING

   logical(C_BOOL) :: a(5)

   do i = 1, 5
      if ( a(i) .neqv. .true. ) error stop 20
      a(i) = .not. a(i)
   end do

end subroutine sub1

subroutine sub2(a)
   use ISO_C_BINDING

   logical(C_BOOL), intent(in) :: a(5)

   do i = 1, 5
      if ( a(i) .neqv. .true. ) error stop 22
   end do

end subroutine sub2

subroutine sub2a(a)
   use ISO_C_BINDING

   logical(C_BOOL), intent(in) :: a(5)

   do i = 1, 5
      if ( a(i) .neqv. .true. ) error stop 24
   end do

end subroutine sub2a

subroutine sub3(a)
   use ISO_C_BINDING

   logical(C_BOOL), intent(inout) :: a(5)

   do i = 1, 5
      if ( a(i) .neqv. .true. ) error stop 26
      a(i) = .not. a(i)
   end do

end subroutine sub3

subroutine sub4(a)
   use ISO_C_BINDING

   logical(C_BOOL), intent(out) :: a(5)

   do i = 1, 5
      a(i) = .not. a(i)
   end do

end subroutine sub4

subroutine sub5(aa)
   use ISO_C_BINDING

   logical(C_BOOL) :: aa(10,5)

   do i = 1, 5
      do j = 1, 10
         if ( aa(j,i) .neqv. .true. ) error stop 28
         aa(j,i) = .not. aa(j,i)
      end do
   end do

end subroutine sub5

subroutine sub6(aa)
   use ISO_C_BINDING

   logical(C_BOOL), intent(in) :: aa(10,5)

   do i = 1, 5
      do j = 1, 10
         if ( aa(j,i) .neqv. .true. ) error stop 30
      end do
   end do

end subroutine sub6

subroutine sub6a(aa)
   use ISO_C_BINDING

   logical(C_BOOL), intent(in) :: aa(10,5)

   do i = 1, 5
      do j = 1, 10
         if ( aa(j,i) .neqv. .true. ) error stop 32
      end do
   end do

end subroutine sub6a

subroutine sub7(aa)
   use ISO_C_BINDING

   logical(C_BOOL), intent(inout) :: aa(10,5)

   do i = 1, 5
      do j = 1, 10
         if ( aa(j,i) .neqv. .true. ) error stop 34
         aa(j,i) = .not. aa(j,i)
      end do
   end do

end subroutine sub7

subroutine sub8(aa)
   use ISO_C_BINDING

   logical(C_BOOL), intent(out) :: aa(10,5)

   do i = 1, 5
      do j = 1, 10
         aa(j,i) = .not. aa(j,i)
      end do
   end do

end subroutine sub8
