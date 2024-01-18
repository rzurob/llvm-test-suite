!#######################################################################
!***********************************************************************
! %START
! %MAIN:
! %PRECMD: ${TR_SRC}/scrisoa00.presh fxisopih01 cxisopih01
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
!*  KEYWORD(S)                 : C_INT64_T
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!*	- testing interoperable kind type parameter C_INT64_T
!*      - testing procedures C_LOC, C_ASSOCIATED and C_F_POINTER
!*      - testing scalar/arrays as fields in derived-types
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program fxisopih01
   use ISO_C_BINDING, ONLY : C_PTR, C_INT64_T, C_LOC, C_ASSOCIATED, C_F_POINTER

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

   type, bind(c) :: dts0
      integer(C_INT64_T) :: a
   end type

   type, bind(c) :: dts1
      integer(C_INT64_T) :: b(DIMB)
   end type

   type, bind(c) :: dts2
      integer(C_INT64_T) :: c(DIMC,DIMC)
   end type

   type(dts0), target :: dta
   type(dts0), pointer :: pa
   type(dts1), target :: dtb
   type(dts1), pointer :: pb
   type(dts2), target :: dtc
   type(dts2), pointer :: pc

   type(dts0), allocatable, target :: dtal
   type(dts1), allocatable, target :: dtbl
   type(dts2), allocatable, target :: dtcl

   type(C_PTR) :: cp

!! Test 1

   dta%a = 5
   pa => dta

   cp = C_LOC(dta)
   if ( .not. C_ASSOCIATED(cp) ) error stop 20
   if ( .not. C_ASSOCIATED(cp,C_LOC(dta)) ) error stop 22

   call sub1(cp)
   if ( .not. C_ASSOCIATED(cp) ) error stop 24
   if ( C_ASSOCIATED(cp,C_LOC(dta)) ) error stop 26

   call C_F_POINTER(cp,pa)
   if ( ASSOCIATED(pa,dta) ) error stop 28
   if ( pa%a /= 10 ) error stop 30

!! Test 2

   do i = 1, DIMB
      dtb%b(i) = i
   end do
   pb => dtb

   cp = C_LOC(dtb)
   if ( .not. C_ASSOCIATED(cp) ) error stop 32
   if ( .not. C_ASSOCIATED(cp,C_LOC(dtb)) ) error stop 34

   call sub2(cp)
   if ( .not. C_ASSOCIATED(cp) ) error stop 36
   if ( C_ASSOCIATED(cp,C_LOC(dtb)) ) error stop 38

   call C_F_POINTER(cp,pb)
   if ( ASSOCIATED(pb,dtb) ) error stop 40
   do i = 1, DIMB
      if ( pb%b(i) /= i+1 ) error stop 42
   end do

!! Test 3

   do i = 1, DIMC
      do j = 1, DIMC
         dtc%c(i,j) = i+j
      end do
   end do
   pc => dtc

   cp = C_LOC(dtc)
   if ( .not. C_ASSOCIATED(cp) ) error stop 44
   if ( .not. C_ASSOCIATED(cp,C_LOC(dtc)) ) error stop 46

   call sub3(cp)
   if ( .not. C_ASSOCIATED(cp) ) error stop 48
   if ( C_ASSOCIATED(cp,C_LOC(dtc)) ) error stop 50

   call C_F_POINTER(cp,pc)
   if ( ASSOCIATED(pc,dtc) ) error stop 52
   do i = 1, DIMC
      do j = 1, DIMC
         if ( pc%c(i,j) /= i+j+1 ) error stop 54
      end do
   end do

!! Test 4

   allocate(dtal)

   dtal%a = 5
   pa => dtal

   cp = C_LOC(dtal)
   if ( .not. C_ASSOCIATED(cp) ) error stop 56
   if ( .not. C_ASSOCIATED(cp,C_LOC(dtal)) ) error stop 58

   call sub1(cp)
   if ( .not. C_ASSOCIATED(cp) ) error stop 60
   if ( C_ASSOCIATED(cp,C_LOC(dtal)) ) error stop 62

   call C_F_POINTER(cp,pa)
   if ( ASSOCIATED(pa,dtal) ) error stop 64
   if ( pa%a /= 10 ) error stop 66

!! Test 5

   allocate(dtbl)

   do i = 1, DIMB
      dtbl%b(i) = i
   end do
   pb => dtbl

   cp = C_LOC(dtbl)
   if ( .not. C_ASSOCIATED(cp) ) error stop 68
   if ( .not. C_ASSOCIATED(cp,C_LOC(dtbl)) ) error stop 70

   call sub2(cp)
   if ( .not. C_ASSOCIATED(cp) ) error stop 72
   if ( C_ASSOCIATED(cp,C_LOC(dtbl)) ) error stop 74

   call C_F_POINTER(cp,pb)
   if ( ASSOCIATED(pb,dtbl) ) error stop 76
   do i = 1, DIMB
      if ( pb%b(i) /= i+1 ) error stop 78
   end do

!! Test 6

   allocate(dtcl)

   do i = 1, DIMC
      do j = 1, DIMC
         dtcl%c(i,j) = i+j
      end do
   end do
   pc => dtcl

   cp = C_LOC(dtcl)
   if ( .not. C_ASSOCIATED(cp) ) error stop 80
   if ( .not. C_ASSOCIATED(cp,C_LOC(dtcl)) ) error stop 82

   call sub3(cp)
   if ( .not. C_ASSOCIATED(cp) ) error stop 84
   if ( C_ASSOCIATED(cp,C_LOC(dtcl)) ) error stop 86

   call C_F_POINTER(cp,pc)
   if ( ASSOCIATED(pc,dtcl) ) error stop 88
   do i = 1, DIMC
      do j = 1, DIMC
         if ( pc%c(i,j) /= i+j+1 ) error stop 90
      end do
   end do

end program fxisopih01
