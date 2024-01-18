!#######################################################################
!***********************************************************************
! %START
! %MAIN:
! %PRECMD: ${TR_SRC}/scrisoi00.presh fxisoi06 cxisoi06
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
!*  KEYWORD(S)                 : C_INT_FAST16_T
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!*	- testing C_INT_FAST16_T
!*	- using external FORTRAN functions
!*	- passing 1-dim and 2-dim array arguments
!*	- main written in C
!*
!234567890123456789012345678901234567890123456789012345678901234567890

integer(C_INT_FAST16_T) function fnt1(a)
   use ISO_C_BINDING

   integer(C_INT_FAST16_T) :: a(5)

   do i = 1, 5
      if ( a(i) /= i ) error stop 20
      a(i) = i+1
   end do

   fnt1 = 0
end function fnt1

integer(C_INT_FAST16_T) function fnt2(a)
   use ISO_C_BINDING

   integer(C_INT_FAST16_T), intent(in) :: a(5)

   do i = 1, 5
      if ( a(i) /= i ) error stop 22
   end do

   fnt2 = 0
end function fnt2

integer(C_INT_FAST16_T) function fnt2a(a)
   use ISO_C_BINDING

   integer(C_INT_FAST16_T), intent(in) :: a(5)

   do i = 1, 5
      if ( a(i) /= i ) error stop 24
   end do

   fnt2a = 0
end function fnt2a

integer(C_INT_FAST16_T) function fnt3(a)
   use ISO_C_BINDING

   integer(C_INT_FAST16_T), intent(inout) :: a(5)

   do i = 1, 5
      if ( a(i) /= i ) error stop 26
      a(i) = i+1
   end do

   fnt3 = 0
end function fnt3

integer(C_INT_FAST16_T) function fnt4(a)
   use ISO_C_BINDING

   integer(C_INT_FAST16_T), intent(out) :: a(5)

   do i = 1, 5
      a(i) = i+1
   end do

   fnt4 = 0
end function fnt4

integer(C_INT_FAST16_T) function fnt5(aa)
   use ISO_C_BINDING

   integer(C_INT_FAST16_T) :: aa(10,5)

   do i = 1, 5
      do j = 1, 10
         if ( aa(j,i) /= i+j-1 ) error stop 28
         aa(j,i) = i+j
      end do
   end do

   fnt5 = 0
end function fnt5

integer(C_INT_FAST16_T) function fnt6(aa)
   use ISO_C_BINDING

   integer(C_INT_FAST16_T), intent(in) :: aa(10,5)

   do i = 1, 5
      do j = 1, 10
         if ( aa(j,i) /= i+j-1 ) error stop 30
      end do
   end do

   fnt6 = 0
end function fnt6

integer(C_INT_FAST16_T) function fnt6a(aa)
   use ISO_C_BINDING

   integer(C_INT_FAST16_T), intent(in) :: aa(10,5)

   do i = 1, 5
      do j = 1, 10
         if ( aa(j,i) /= i+j-1 ) error stop 32
      end do
   end do

   fnt6a = 0
end function fnt6a

integer(C_INT_FAST16_T) function fnt7(aa)
   use ISO_C_BINDING

   integer(C_INT_FAST16_T), intent(inout) :: aa(10,5)

   do i = 1, 5
      do j = 1, 10
         if ( aa(j,i) /= i+j-1 ) error stop 34
         aa(j,i) = i+j
      end do
   end do

   fnt7 = 0
end function fnt7

integer(C_INT_FAST16_T) function fnt8(aa)
   use ISO_C_BINDING

   integer(C_INT_FAST16_T), intent(out) :: aa(10,5)

   do i = 1, 5
      do j = 1, 10
         aa(j,i) = i+j
      end do
   end do

   fnt8 = 0
end function fnt8
