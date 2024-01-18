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
!*  KEYWORD(S)                 : 16
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!*	- testing 16
!*	- using external FORTRAN functions
!*	- passing 1-dim and 2-dim array arguments
!*	- main written in C
!*
!234567890123456789012345678901234567890123456789012345678901234567890

real(16) function fnt1(a)
   use ISO_C_BINDING

   real(16) :: a(5)

   do i = 1, 5
      if ( a(i) /= real(i,16) ) error stop 20
      a(i) = real(i+1,16)
   end do

   fnt1 = 0
end function fnt1

real(16) function fnt2(a)
   use ISO_C_BINDING

   real(16), intent(in) :: a(5)

   do i = 1, 5
      if ( a(i) /= real(i,16) ) error stop 22
   end do

   fnt2 = 0
end function fnt2

real(16) function fnt2a(a)
   use ISO_C_BINDING

   real(16), intent(in) :: a(5)

   do i = 1, 5
      if ( a(i) /= real(i,16) ) error stop 24
   end do

   fnt2a = 0
end function fnt2a

real(16) function fnt3(a)
   use ISO_C_BINDING

   real(16), intent(inout) :: a(5)

   do i = 1, 5
      if ( a(i) /= real(i,16) ) error stop 26
      a(i) = real(i+1,16)
   end do

   fnt3 = 0
end function fnt3

real(16) function fnt4(a)
   use ISO_C_BINDING

   real(16), intent(out) :: a(5)

   do i = 1, 5
      a(i) = real(i+1,16)
   end do

   fnt4 = 0
end function fnt4

real(16) function fnt5(aa)
   use ISO_C_BINDING

   real(16) :: aa(10,5)

   do i = 1, 5
      do j = 1, 10
         if ( aa(j,i) /= real(i+j-1,16) ) error stop 28
         aa(j,i) = real(i+j,16)
      end do
   end do

   fnt5 = 0
end function fnt5

real(16) function fnt6(aa)
   use ISO_C_BINDING

   real(16), intent(in) :: aa(10,5)

   do i = 1, 5
      do j = 1, 10
         if ( aa(j,i) /= real(i+j-1,16) ) error stop 30
      end do
   end do

   fnt6 = 0
end function fnt6

real(16) function fnt6a(aa)
   use ISO_C_BINDING

   real(16), intent(in) :: aa(10,5)

   do i = 1, 5
      do j = 1, 10
         if ( aa(j,i) /= real(i+j-1,16) ) error stop 32
      end do
   end do

   fnt6a = 0
end function fnt6a

real(16) function fnt7(aa)
   use ISO_C_BINDING

   real(16), intent(inout) :: aa(10,5)

   do i = 1, 5
      do j = 1, 10
         if ( aa(j,i) /= real(i+j-1,16) ) error stop 34
         aa(j,i) = real(i+j,16)
      end do
   end do

   fnt7 = 0
end function fnt7

real(16) function fnt8(aa)
   use ISO_C_BINDING

   real(16), intent(out) :: aa(10,5)

   do i = 1, 5
      do j = 1, 10
         aa(j,i) = real(i+j,16)
      end do
   end do

   fnt8 = 0
end function fnt8
