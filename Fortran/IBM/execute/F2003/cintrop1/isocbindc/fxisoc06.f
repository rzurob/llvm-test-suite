!#######################################################################
!***********************************************************************
! %START
! %MAIN:
! %PRECMD: ${TR_SRC}/scrisoa00.presh fxisoc06 cxisoc06
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
!*  KEYWORD(S)                 : C_INT8_T, C_INT16_T
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!*	- testing C_INT8_T and C_INT16_T
!*	- using external FORTRAN functions
!*	- passing 1-dim and 2-dim array arguments
!*	- main written in C
!*
!234567890123456789012345678901234567890123456789012345678901234567890

integer(C_INT8_T) function fnt1(a,b)
   use ISO_C_BINDING

   integer(C_INT8_T) :: a(5)
   integer(C_INT16_T) :: b(5)

   do i = 1, 5
      if ( a(i) /= i ) error stop 20
      a(i) = i+1
      if ( b(i) /= i ) error stop 22
      b(i) = i+1
   end do

   fnt1 = 0
end function fnt1

integer(C_INT8_T) function fnt2(a,b)
   use ISO_C_BINDING

   integer(C_INT8_T), intent(in) :: a(5)
   integer(C_INT16_T), intent(in) :: b(5)

   do i = 1, 5
      if ( a(i) /= i ) error stop 24
      if ( b(i) /= i ) error stop 26
   end do

   fnt2 = 0
end function fnt2

integer(C_INT8_T) function fnt2a(a,b)
   use ISO_C_BINDING

   integer(C_INT8_T), intent(in) :: a(5)
   integer(C_INT16_T), intent(in) :: b(5)

   do i = 1, 5
      if ( a(i) /= i ) error stop 28
      if ( b(i) /= i ) error stop 30
   end do

   fnt2a = 0
end function fnt2a

integer(C_INT8_T) function fnt3(a,b)
   use ISO_C_BINDING

   integer(C_INT8_T), intent(inout) :: a(5)
   integer(C_INT16_T), intent(inout) :: b(5)

   do i = 1, 5
      if ( a(i) /= i ) error stop 32
      a(i) = i+1
      if ( b(i) /= i ) error stop 34
      b(i) = i+1
   end do

   fnt3 = 0
end function fnt3

integer(C_INT8_T) function fnt4(a,b)
   use ISO_C_BINDING

   integer(C_INT8_T), intent(out) :: a(5)
   integer(C_INT16_T), intent(out) :: b(5)

   do i = 1, 5
      a(i) = i+1
      b(i) = i+1
   end do

   fnt4 = 0
end function fnt4

integer(C_INT8_T) function fnt5(aa,bb)
   use ISO_C_BINDING

   integer(C_INT8_T) :: aa(10,5)
   integer(C_INT16_T) :: bb(10,5)

   do i = 1, 5
      do j = 1, 10
         if ( aa(j,i) /= i+j-1 ) error stop 36
         aa(j,i) = i+j
         if ( bb(j,i) /= i+j-1 ) error stop 38
         bb(j,i) = i+j
      end do
   end do

   fnt5 = 0
end function fnt5

integer(C_INT8_T) function fnt6(aa,bb)
   use ISO_C_BINDING

   integer(C_INT8_T), intent(in) :: aa(10,5)
   integer(C_INT16_T), intent(in) :: bb(10,5)

   do i = 1, 5
      do j = 1, 10
         if ( aa(j,i) /= i+j-1 ) error stop 40
         if ( bb(j,i) /= i+j-1 ) error stop 42
      end do
   end do

   fnt6 = 0
end function fnt6

integer(C_INT8_T) function fnt6a(aa,bb)
   use ISO_C_BINDING

   integer(C_INT8_T), intent(in) :: aa(10,5)
   integer(C_INT16_T), intent(in) :: bb(10,5)

   do i = 1, 5
      do j = 1, 10
         if ( aa(j,i) /= i+j-1 ) error stop 44
         if ( bb(j,i) /= i+j-1 ) error stop 46
      end do
   end do

   fnt6a = 0
end function fnt6a

integer(C_INT8_T) function fnt7(aa,bb)
   use ISO_C_BINDING

   integer(C_INT8_T), intent(inout) :: aa(10,5)
   integer(C_INT16_T), intent(inout) :: bb(10,5)

   do i = 1, 5
      do j = 1, 10
         if ( aa(j,i) /= i+j-1 ) error stop 48
         aa(j,i) = i+j
         if ( bb(j,i) /= i+j-1 ) error stop 50
         bb(j,i) = i+j
      end do
   end do

   fnt7 = 0
end function fnt7

integer(C_INT8_T) function fnt8(aa,bb)
   use ISO_C_BINDING

   integer(C_INT8_T), intent(out) :: aa(10,5)
   integer(C_INT16_T), intent(out) :: bb(10,5)

   do i = 1, 5
      do j = 1, 10
         aa(j,i) = i+j
         bb(j,i) = i+j
      end do
   end do

   fnt8 = 0
end function fnt8
