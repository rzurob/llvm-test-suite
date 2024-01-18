!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
!***********************************************************************
! %START
! %MAIN:
! %PRECMD: ${TR_SRC}/scrisoa00.presh fxisok06 cxisok06
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
!*  KEYWORD(S)                 : C_CHAR, C_SIGNED_CHAR
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                : 
!*
!*	- testing C_CHAR and C_SIGNED_CHAR
!*	- using external FORTRAN functions
!*	- passing 1-dim and 2-dim array arguments
!*	- main written in C
!*
!234567890123456789012345678901234567890123456789012345678901234567890

integer(C_SIGNED_CHAR) function fnt1(a,b)
   use ISO_C_BINDING

   character(C_CHAR) :: a(4)
   integer(C_SIGNED_CHAR) :: b(4)
   
   do i = 1, 4
      if ( a(i) /= achar(iachar('A')+i-1) ) error stop 20
      a(i) = achar(iachar('A')+i+3)
      if ( b(i) /= iachar('A')+i-1 ) error stop 22
      b(i) = iachar('A')+i+3
   end do

   fnt1 = 0
end function fnt1

integer(C_SIGNED_CHAR) function fnt2(a,b)
   use ISO_C_BINDING

   character(C_CHAR), intent(in) :: a(4)
   integer(C_SIGNED_CHAR), intent(in) :: b(4)

   do i = 1, 4
      if ( a(i) /= achar(iachar('A')+i-1) ) error stop 24
      if ( b(i) /= iachar('A')+i-1 ) error stop 26
   end do

   fnt2 = 0
end function fnt2

integer(C_SIGNED_CHAR) function fnt2a(a,b)
   use ISO_C_BINDING

   character(C_CHAR), intent(in) :: a(4)
   integer(C_SIGNED_CHAR), intent(in) :: b(4)

   do i = 1, 4
      if ( a(i) /= achar(iachar('A')+i-1) ) error stop 28
      if ( b(i) /= iachar('A')+i-1 ) error stop 30
   end do

   fnt2a = 0
end function fnt2a

integer(C_SIGNED_CHAR) function fnt3(a,b)
   use ISO_C_BINDING

   character(C_CHAR), intent(inout) :: a(4)
   integer(C_SIGNED_CHAR), intent(inout) :: b(4)

   do i = 1, 4
      if ( a(i) /= achar(iachar('A')+i-1) ) error stop 32
      a(i) = achar(iachar('A')+i+3)
      if ( b(i) /= iachar('A')+i-1 ) error stop 34
      b(i) = iachar('A')+i+3
   end do

   fnt3 = 0
end function fnt3

integer(C_SIGNED_CHAR) function fnt4(a,b)
   use ISO_C_BINDING

   character(C_CHAR), intent(out) :: a(4)
   integer(C_SIGNED_CHAR), intent(out) :: b(4)

   do i = 1, 4
      a(i) = achar(iachar('A')+i+3)
      b(i) = iachar('A')+i+3
   end do

   fnt4 = 0
end function fnt4

integer(C_SIGNED_CHAR) function fnt5(aa,bb)
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

   fnt5 = 0
end function fnt5

integer(C_SIGNED_CHAR) function fnt6(aa,bb)
   use ISO_C_BINDING

   character(C_CHAR), intent(in) :: aa(6,4)
   integer(C_SIGNED_CHAR), intent(in) :: bb(6,4)

   do i = 1, 4
      do j = 1, 6
         if ( aa(j,i) /= achar(iachar('A')+(i-1)*6+(j-1)) ) error stop 40
         if ( bb(j,i) /= iachar('A')+(i-1)*6+(j-1) ) error stop 42
      end do
   end do

   fnt6 = 0
end function fnt6

integer(C_SIGNED_CHAR) function fnt6a(aa,bb)
   use ISO_C_BINDING

   character(C_CHAR), intent(in) :: aa(6,4)
   integer(C_SIGNED_CHAR), intent(in) :: bb(6,4)

   do i = 1, 4
      do j = 1, 6
         if ( aa(j,i) /= achar(iachar('A')+(i-1)*6+(j-1)) ) error stop 44
         if ( bb(j,i) /= iachar('A')+(i-1)*6+(j-1) ) error stop 46
      end do
   end do

   fnt6a = 0
end function fnt6a

integer(C_SIGNED_CHAR) function fnt7(aa,bb)
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

   fnt7 = 0
end function fnt7

integer(C_SIGNED_CHAR) function fnt8(aa,bb)
   use ISO_C_BINDING

   character(C_CHAR), intent(out) :: aa(6,4)
   integer(C_SIGNED_CHAR), intent(out) :: bb(6,4)
   
   do i = 1, 4
      do j = 1, 6
         aa(j,i) = achar(iachar('A')+(i-1)*6+j)
         bb(j,i) = iachar('A')+(i-1)*6+j
      end do
   end do

   fnt8 = 0
end function fnt8
