!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
!***********************************************************************
! %START
! %MAIN:
! %PRECMD: ${TR_SRC}/scrisor00.presh fxisor06 cxisor06
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
!*  KEYWORD(S)                 : C_BOOL
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                : 
!*
!*	- testing C_BOOL
!*	- using external FORTRAN functions
!*	- passing 1-dim and 2-dim array arguments
!*	- main written in C
!*
!234567890123456789012345678901234567890123456789012345678901234567890

logical(C_BOOL) function fnt1(a)
   use ISO_C_BINDING

   logical(C_BOOL) :: a(5)
   
   do i = 1, 5
      if ( a(i) .neqv. .true. ) error stop 20
      a(i) = .not. a(i)
   end do

   fnt1 = .false.
end function fnt1

logical(C_BOOL) function fnt2(a)
   use ISO_C_BINDING

   logical(C_BOOL), intent(in) :: a(5)

   do i = 1, 5
      if ( a(i) .neqv. .true. ) error stop 22
   end do

   fnt2 = .false.
end function fnt2

logical(C_BOOL) function fnt2a(a)
   use ISO_C_BINDING

   logical(C_BOOL), intent(in) :: a(5)

   do i = 1, 5
      if ( a(i) .neqv. .true. ) error stop 24
   end do

   fnt2a = .false.
end function fnt2a

logical(C_BOOL) function fnt3(a)
   use ISO_C_BINDING

   logical(C_BOOL), intent(inout) :: a(5)

   do i = 1, 5
      if ( a(i) .neqv. .true. ) error stop 26
      a(i) = .not. a(i)
   end do

   fnt3 = .false.
end function fnt3

logical(C_BOOL) function fnt4(a)
   use ISO_C_BINDING

   logical(C_BOOL), intent(out) :: a(5)

   do i = 1, 5
      a(i) = .not. a(i)
   end do

   fnt4 = .false.
end function fnt4

logical(C_BOOL) function fnt5(aa)
   use ISO_C_BINDING

   logical(C_BOOL) :: aa(10,5)
   
   do i = 1, 5
      do j = 1, 10
         if ( aa(j,i) .neqv. .true. ) error stop 28
         aa(j,i) = .not. aa(j,i)
      end do
   end do

   fnt5 = .false.
end function fnt5

logical(C_BOOL) function fnt6(aa)
   use ISO_C_BINDING

   logical(C_BOOL), intent(in) :: aa(10,5)

   do i = 1, 5
      do j = 1, 10
         if ( aa(j,i) .neqv. .true. ) error stop 30
      end do
   end do

   fnt6 = .false.
end function fnt6

logical(C_BOOL) function fnt6a(aa)
   use ISO_C_BINDING

   logical(C_BOOL), intent(in) :: aa(10,5)

   do i = 1, 5
      do j = 1, 10
         if ( aa(j,i) .neqv. .true. ) error stop 32
      end do
   end do

   fnt6a = .false.
end function fnt6a

logical(C_BOOL) function fnt7(aa)
   use ISO_C_BINDING

   logical(C_BOOL), intent(inout) :: aa(10,5)
   
   do i = 1, 5
      do j = 1, 10
         if ( aa(j,i) .neqv. .true. ) error stop 34
         aa(j,i) = .not. aa(j,i)
      end do
   end do

   fnt7 = .false.
end function fnt7

logical(C_BOOL) function fnt8(aa)
   use ISO_C_BINDING

   logical(C_BOOL), intent(out) :: aa(10,5)
   
   do i = 1, 5
      do j = 1, 10
         aa(j,i) = .not. aa(j,i)
      end do
   end do

   fnt8 = .false.
end function fnt8
