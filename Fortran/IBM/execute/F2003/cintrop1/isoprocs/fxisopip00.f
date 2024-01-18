!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
!***********************************************************************
! %START
! %MAIN:
! %PRECMD: ${TR_SRC}/scrisoa00.presh fxisopip00 cxisopip00
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
!*  KEYWORD(S)                 : C_INT_FAST64_T
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                : 
!*
!*	- testing interoperable kind type parameter C_INT_FAST64_T
!*      - testing procedures C_LOC, C_ASSOCIATED and C_F_POINTER
!*      - testing scalar/arrays
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program fxisopip00
   use ISO_C_BINDING, ONLY : C_PTR, C_INT_FAST64_T, C_LOC, C_ASSOCIATED, C_F_POINTER

   interface
      subroutine sub1(x)
         use ISO_C_BINDING, ONLY : C_PTR
         type(C_PTR) :: x
      end subroutine sub1
      subroutine sub2(x)
         use ISO_C_BINDING, ONLY : C_PTR
         type(C_PTR) :: x
      end subroutine sub2
      subroutine sub3(x)
         use ISO_C_BINDING, ONLY : C_PTR
         type(C_PTR) :: x
      end subroutine sub3
   end interface

   integer, parameter :: DIMB = 4, DIMC = 3

   integer(C_INT_FAST64_T), target :: a, b(DIMB), c(DIMC,DIMC)
   integer(C_INT_FAST64_T), allocatable, target :: al, bl(:), cl(:,:)
   integer(C_INT_FAST64_T), pointer :: pa, pb(:), pc(:,:)
   type(C_PTR) :: cp
   
!! Test 1

   a = 5
   pa => a

   cp = C_LOC(a)
   if ( .not. C_ASSOCIATED(cp) ) error stop 20
   if ( .not. C_ASSOCIATED(cp,C_LOC(a)) ) error stop 22

   call sub1(cp)
   if ( .not. C_ASSOCIATED(cp) ) error stop 24
   if ( C_ASSOCIATED(cp,C_LOC(a)) ) error stop 26

   call C_F_POINTER(cp,pa)
   if ( ASSOCIATED(pa,a) ) error stop 28
   if ( pa /= 10 ) error stop 30

   pa => a

   cp = C_LOC(pa)
   if ( .not. C_ASSOCIATED(cp) ) error stop 32
   if ( .not. C_ASSOCIATED(cp,C_LOC(pa)) ) error stop 34

   call sub1(cp)
   if ( .not. C_ASSOCIATED(cp) ) error stop 36
   if ( C_ASSOCIATED(cp,C_LOC(pa)) ) error stop 38

   call C_F_POINTER(cp,pa)
   if ( ASSOCIATED(pa,a) ) error stop 40
   if ( pa /= 10 ) error stop 42

!! Test 2

   do i = 1, DIMB
      b(i) = i
   end do
   pb => b

   cp = C_LOC(b)
   if ( .not. C_ASSOCIATED(cp) ) error stop 44
   if ( .not. C_ASSOCIATED(cp,C_LOC(b)) ) error stop 46

   call sub2(cp)
   if ( .not. C_ASSOCIATED(cp) ) error stop 48
   if ( C_ASSOCIATED(cp,C_LOC(b)) ) error stop 50

   call C_F_POINTER(cp,pb,(/DIMB/))
   if ( ASSOCIATED(pb,b) ) error stop 52
   do i = 1, DIMB
      if ( pb(i) /= i+1 ) error stop 54
   end do

!! Test 3

   do i = 1, DIMC
      do j = 1, DIMC
         c(i,j) = i+j
      end do
   end do
   pc => c

   cp = C_LOC(c)
   if ( .not. C_ASSOCIATED(cp) ) error stop 56
   if ( .not. C_ASSOCIATED(cp,C_LOC(c)) ) error stop 58

   call sub3(cp)
   if ( .not. C_ASSOCIATED(cp) ) error stop 60
   if ( C_ASSOCIATED(cp,C_LOC(c)) ) error stop 62

   call C_F_POINTER(cp,pc,(/DIMC,DIMC/))
   if ( ASSOCIATED(pc,c) ) error stop 64
   do i = 1, DIMC
      do j = 1, DIMC
         if ( pc(i,j) /= i+j+1 ) error stop 66
      end do
   end do

!! Test 4

   allocate(al)

   al = 5
   pa => al

   cp = C_LOC(al)
   if ( .not. C_ASSOCIATED(cp) ) error stop 68
   if ( .not. C_ASSOCIATED(cp,C_LOC(al)) ) error stop 70

   call sub1(cp)
   if ( .not. C_ASSOCIATED(cp) ) error stop 72
   if ( C_ASSOCIATED(cp,C_LOC(al)) ) error stop 74

   call C_F_POINTER(cp,pa)
   if ( ASSOCIATED(pa,al) ) error stop 76
   if ( pa /= 10 ) error stop 78

   pa => al

   cp = C_LOC(pa)
   if ( .not. C_ASSOCIATED(cp) ) error stop 80
   if ( .not. C_ASSOCIATED(cp,C_LOC(pa)) ) error stop 82

   call sub1(cp)
   if ( .not. C_ASSOCIATED(cp) ) error stop 84
   if ( C_ASSOCIATED(cp,C_LOC(pa)) ) error stop 86

   call C_F_POINTER(cp,pa)
   if ( ASSOCIATED(pa,a) ) error stop 88
   if ( pa /= 10 ) error stop 90

   deallocate(al)

!! Test 5

   allocate(bl(DIMB))
 
   do i = 1, DIMB
      bl(i) = i
   end do
   pb => bl

   cp = C_LOC(bl)
   if ( .not. C_ASSOCIATED(cp) ) error stop 92
   if ( .not. C_ASSOCIATED(cp,C_LOC(bl)) ) error stop 94

   call sub2(cp)
   if ( .not. C_ASSOCIATED(cp) ) error stop 96
   if ( C_ASSOCIATED(cp,C_LOC(bl)) ) error stop 98

   call C_F_POINTER(cp,pb,(/DIMB/))
   if ( ASSOCIATED(pb,bl) ) error stop 100
   do i = 1, DIMB
      if ( pb(i) /= i+1 ) error stop 102
   end do

   deallocate(bl)

!! Test 6

   allocate(cl(DIMC,DIMC))

   do i = 1, DIMC
      do j = 1, DIMC
         cl(i,j) = i+j
      end do
   end do
   pc => cl

   cp = C_LOC(cl)
   if ( .not. C_ASSOCIATED(cp) ) error stop 104
   if ( .not. C_ASSOCIATED(cp,C_LOC(cl)) ) error stop 106

   call sub3(cp)
   if ( .not. C_ASSOCIATED(cp) ) error stop 108
   if ( C_ASSOCIATED(cp,C_LOC(cl)) ) error stop 110

   call C_F_POINTER(cp,pc,(/DIMC,DIMC/))
   if ( ASSOCIATED(pc,cl) ) error stop 112
   do i = 1, DIMC
      do j = 1, DIMC
         if ( pc(i,j) /= i+j+1 ) error stop 114
      end do
   end do

   deallocate(cl)

end program fxisopip00
