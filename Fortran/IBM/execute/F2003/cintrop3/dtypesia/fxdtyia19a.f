!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
!***********************************************************************
! %START
! %MAIN:
! %PRECMD: ${TR_SRC}/scrdtya00.presh fxdtyia19a cxdtyia19
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
!*  TEST CASE TITLE            : Support for derived types with BIND(C) attr.
!*
!*  PROGRAMMER                 : Alberto Alvarez-Mesquide
!*  DATE                       : 4/23/2002
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived types with BIND(C) attribute
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
!*      - Testing allocatable arrays of single derived types with BIND(C) attribute
!*      - Testing allocatable arrays of single derived types with INTENT attributes
!*      - Testing allocatable arrays of single derived types with integer components
!*      - Testing FORTRAN functions and C void functions
!*      - Main written in C
!*
!234567890123456789012345678901234567890123456789012345678901234567890

function fun1(dt) bind(c)
   use ISO_C_BINDING
   integer, parameter :: DIM1 = 3, DIM2 = 2

   type, bind(c) :: dt1
      integer(1) var_a(DIM1,DIM2)
      integer(C_INT) var_b(DIM1,DIM2)
      integer(C_SHORT) var_c(DIM1,DIM2)
      integer(8) var_d(DIM1,DIM2)
      integer(C_LONG) var_e(DIM1,DIM2)
      integer(2) var_f(DIM1,DIM2)
      integer(C_LONG_LONG) var_g(DIM1,DIM2)
      integer(4) var_h(DIM1,DIM2)
   end type

   type(dt1), dimension(DIM2,DIM1), target :: dt
   type(C_PTR) :: fun1

   do k = 1, DIM1
    do l = 1, DIM2
     do j = 1, DIM2
      do i = 1, DIM1
       if ( dt(l,k)%var_a(i,j) /= i+j+(j-1)*DIM2-1 ) error stop 20
       if ( dt(l,k)%var_b(i,j) /= i+j+(j-1)*DIM2 ) error stop 22
       if ( dt(l,k)%var_c(i,j) /= i+j+(j-1)*DIM2+1 ) error stop 24
       if ( dt(l,k)%var_d(i,j) /= i+j+(j-1)*DIM2+2 ) error stop 26
       if ( dt(l,k)%var_e(i,j) /= i+j+(j-1)*DIM2+3 ) error stop 28
       if ( dt(l,k)%var_f(i,j) /= i+j+(j-1)*DIM2+4 ) error stop 30
       if ( dt(l,k)%var_g(i,j) /= i+j+(j-1)*DIM2+5 ) error stop 32
       if ( dt(l,k)%var_h(i,j) /= i+j+(j-1)*DIM2+6 ) error stop 34
      end do
     end do
    end do
   end do

   do k = 1, DIM1
    do l = 1, DIM2
     do j = 1, DIM2
      do i = 1, DIM1
       dt(l,k)%var_a(i,j) = i+j+(j-1)*DIM2
       dt(l,k)%var_b(i,j) = i+j+(j-1)*DIM2+1
       dt(l,k)%var_c(i,j) = i+j+(j-1)*DIM2+2
       dt(l,k)%var_d(i,j) = i+j+(j-1)*DIM2+3
       dt(l,k)%var_e(i,j) = i+j+(j-1)*DIM2+4
       dt(l,k)%var_f(i,j) = i+j+(j-1)*DIM2+5
       dt(l,k)%var_g(i,j) = i+j+(j-1)*DIM2+6
       dt(l,k)%var_h(i,j) = i+j+(j-1)*DIM2+7
      end do
     end do
    end do
   end do

   fun1 = C_LOC(dt)

end function fun1

function fun2(dtx,dty) bind(c)
   use ISO_C_BINDING
   integer, parameter :: DIM1 = 3, DIM2 = 2

   type, bind(c) :: dt1
      integer(1) var_a(DIM1,DIM2)
      integer(C_INT) var_b(DIM1,DIM2)
      integer(C_SHORT) var_c(DIM1,DIM2)
      integer(8) var_d(DIM1,DIM2)
      integer(C_LONG) var_e(DIM1,DIM2)
      integer(2) var_f(DIM1,DIM2)
      integer(C_LONG_LONG) var_g(DIM1,DIM2)
      integer(4) var_h(DIM1,DIM2)
   end type

   type(dt1), dimension(DIM2,DIM1), intent(in) :: dtx
   type(dt1), dimension(DIM2,DIM1), intent(out), target :: dty
   type(C_PTR) :: fun2

   do k = 1, DIM1
    do l = 1, DIM2
     do j = 1, DIM2
      do i = 1, DIM1
       if ( dtx(l,k)%var_a(i,j) /= 2*(i+j+(j-1)*DIM2-1) ) error stop 36
       if ( dtx(l,k)%var_b(i,j) /= 2*(i+j+(j-1)*DIM2) ) error stop 38
       if ( dtx(l,k)%var_c(i,j) /= 2*(i+j+(j-1)*DIM2+1) ) error stop 40
       if ( dtx(l,k)%var_d(i,j) /= 2*(i+j+(j-1)*DIM2+2) ) error stop 42
       if ( dtx(l,k)%var_e(i,j) /= 2*(i+j+(j-1)*DIM2+3) ) error stop 44
       if ( dtx(l,k)%var_f(i,j) /= 2*(i+j+(j-1)*DIM2+4) ) error stop 46
       if ( dtx(l,k)%var_g(i,j) /= 2*(i+j+(j-1)*DIM2+5) ) error stop 48
       if ( dtx(l,k)%var_h(i,j) /= 2*(i+j+(j-1)*DIM2+6) ) error stop 50
      end do
     end do
    end do
   end do

   do k = 1, DIM1
    do l = 1, DIM2
     do j = 1, DIM2
      do i = 1, DIM1
       dty(l,k)%var_a(i,j) = 2*(i+j+(j-1)*DIM2-1)+1
       dty(l,k)%var_b(i,j) = 2*(i+j+(j-1)*DIM2)+1
       dty(l,k)%var_c(i,j) = 2*(i+j+(j-1)*DIM2+1)+1
       dty(l,k)%var_d(i,j) = 2*(i+j+(j-1)*DIM2+2)+1
       dty(l,k)%var_e(i,j) = 2*(i+j+(j-1)*DIM2+3)+1
       dty(l,k)%var_f(i,j) = 2*(i+j+(j-1)*DIM2+4)+1
       dty(l,k)%var_g(i,j) = 2*(i+j+(j-1)*DIM2+5)+1
       dty(l,k)%var_h(i,j) = 2*(i+j+(j-1)*DIM2+6)+1
      end do
     end do
    end do
   end do

   fun2 = C_LOC(dty)

end function fun2
