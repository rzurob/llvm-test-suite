!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
!***********************************************************************
! %START
! %MAIN:
! %PRECMD: ${TR_SRC}/scrisoq00.presh fxisopxd02 cxisopxd02
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
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                : 
!*
!*	- testing interoperable kind type parameter 16
!*      - testing procedures C_LOC, C_ASSOCIATED and C_F_POINTER
!*      - testing scalar/arrays as fields in derived-types
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program fxisopxd02
   use ISO_C_BINDING, ONLY : C_PTR, C_LOC, C_ASSOCIATED, C_F_POINTER

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

   integer, parameter :: DIMB = 4, DIMC = 3, DIMT = 2

   type, bind(c) :: dts0
      complex(16) :: a
   end type

   type, bind(c) :: dts1
      complex(16) :: b(DIMB)
   end type

   type, bind(c) :: dts2
      complex(16) :: c(DIMC,DIMC)
   end type

   type(dts0), target :: dta(DIMT)
   type(dts0), pointer :: pa(:)
   type(dts1), target :: dtb(DIMT)
   type(dts1), pointer :: pb(:)
   type(dts2), target :: dtc(DIMT,DIMT)
   type(dts2), pointer :: pc(:,:)

   type(dts0), allocatable, target :: dtal(:)
   type(dts1), allocatable, target :: dtbl(:)
   type(dts2), allocatable, target :: dtcl(:,:)

   type(C_PTR) :: cp
   
!! Test 1

   do i = 1, DIMT
      dta(i)%a = cmplx(5.0q0,5.0q0)
   end do
   pa => dta

   cp = C_LOC(dta)
   if ( .not. C_ASSOCIATED(cp) ) error stop 20
   if ( .not. C_ASSOCIATED(cp,C_LOC(dta)) ) error stop 22

   call sub1(cp)
   if ( .not. C_ASSOCIATED(cp) ) error stop 24
   if ( C_ASSOCIATED(cp,C_LOC(dta)) ) error stop 26

   call C_F_POINTER(cp,pa,(/DIMT/))
   if ( ASSOCIATED(pa,dta) ) error stop 28
   do i = 1, DIMT
      if ( pa(i)%a /= cmplx(10.0q0,10.0q0) ) error stop 30
   end do

!! Test 2

   do i = 1, DIMT
      do j = 1, DIMB
         dtb(i)%b(j) = cmplx(j,j,16)
      end do
   end do
   pb => dtb

   cp = C_LOC(dtb)
   if ( .not. C_ASSOCIATED(cp) ) error stop 32
   if ( .not. C_ASSOCIATED(cp,C_LOC(dtb)) ) error stop 34

   call sub2(cp)
   if ( .not. C_ASSOCIATED(cp) ) error stop 36
   if ( C_ASSOCIATED(cp,C_LOC(dtb)) ) error stop 38

   call C_F_POINTER(cp,pb,(/DIMT/))
   if ( ASSOCIATED(pb,dtb) ) error stop 40
   do i = 1, DIMT
      do j = 1, DIMB
         if ( pb(i)%b(j) /= cmplx(j+1,j+1,16) ) error stop 42
      end do
   end do

!! Test 3

   do i = 1, DIMT
      do j = 1, DIMT
         do k = 1, DIMC
            do l = 1, DIMC
               dtc(i,j)%c(k,l) = cmplx(k+l,k+l,16)
            end do
         end do
      end do
   end do
   pc => dtc

   cp = C_LOC(dtc)
   if ( .not. C_ASSOCIATED(cp) ) error stop 44
   if ( .not. C_ASSOCIATED(cp,C_LOC(dtc)) ) error stop 46

   call sub3(cp)
   if ( .not. C_ASSOCIATED(cp) ) error stop 48
   if ( C_ASSOCIATED(cp,C_LOC(dtc)) ) error stop 50

   call C_F_POINTER(cp,pc,(/DIMT,DIMT/))
   if ( ASSOCIATED(pc,dtc) ) error stop 52
   do i = 1, DIMT
      do j = 1, DIMT
         do k = 1, DIMC
            do l = 1, DIMC
               if ( pc(i,j)%c(k,l) /= cmplx(k+l+1,k+l+1,16) ) error stop 54
            end do
         end do
      end do
   end do

!! Test 4

   allocate(dtal(DIMT))

   do i = 1, DIMT
      dtal(i)%a = cmplx(5.0q0,5.0q0)
   end do
   pa => dtal

   cp = C_LOC(dtal)
   if ( .not. C_ASSOCIATED(cp) ) error stop 20
   if ( .not. C_ASSOCIATED(cp,C_LOC(dtal)) ) error stop 22

   call sub1(cp)
   if ( .not. C_ASSOCIATED(cp) ) error stop 24
   if ( C_ASSOCIATED(cp,C_LOC(dtal)) ) error stop 26

   call C_F_POINTER(cp,pa,(/DIMT/))
   if ( ASSOCIATED(pa,dtal) ) error stop 28
   do i = 1, DIMT
      if ( pa(i)%a /= cmplx(10.0q0,10.0q0) ) error stop 30
   end do

!! Test 5

   allocate(dtbl(DIMT))

   do i = 1, DIMT
      do j = 1, DIMB
         dtbl(i)%b(j) = cmplx(j,j,16)
      end do
   end do
   pb => dtbl

   cp = C_LOC(dtbl)
   if ( .not. C_ASSOCIATED(cp) ) error stop 32
   if ( .not. C_ASSOCIATED(cp,C_LOC(dtbl)) ) error stop 34

   call sub2(cp)
   if ( .not. C_ASSOCIATED(cp) ) error stop 36
   if ( C_ASSOCIATED(cp,C_LOC(dtbl)) ) error stop 38

   call C_F_POINTER(cp,pb,(/DIMT/))
   if ( ASSOCIATED(pb,dtbl) ) error stop 40
   do i = 1, DIMT
      do j = 1, DIMB
         if ( pb(i)%b(j) /= cmplx(j+1,j+1,16) ) error stop 42
      end do
   end do

!! Test 6

   allocate(dtcl(DIMT,DIMT))

   do i = 1, DIMT
      do j = 1, DIMT
         do k = 1, DIMC
            do l = 1, DIMC
               dtcl(i,j)%c(k,l) = cmplx(k+l,k+l,16)
            end do
         end do
      end do
   end do
   pc => dtcl

   cp = C_LOC(dtcl)
   if ( .not. C_ASSOCIATED(cp) ) error stop 44
   if ( .not. C_ASSOCIATED(cp,C_LOC(dtcl)) ) error stop 46

   call sub3(cp)
   if ( .not. C_ASSOCIATED(cp) ) error stop 48
   if ( C_ASSOCIATED(cp,C_LOC(dtcl)) ) error stop 50

   call C_F_POINTER(cp,pc,(/DIMT,DIMT/))
   if ( ASSOCIATED(pc,dtcl) ) error stop 52
   do i = 1, DIMT
      do j = 1, DIMT
         do k = 1, DIMC
            do l = 1, DIMC
               if ( pc(i,j)%c(k,l) /= cmplx(k+l+1,k+l+1,16) ) error stop 54
            end do
         end do
      end do
   end do

end program fxisopxd02
