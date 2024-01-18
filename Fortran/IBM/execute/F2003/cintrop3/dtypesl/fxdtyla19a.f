!#######################################################################
!***********************************************************************
! %START
! %MAIN:
! %PRECMD: ${TR_SRC}/scrdtye00.presh fxdtyla19a cxdtyla19
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
!*  PRIMARY FUNCTIONS TESTED   : Derived types with BIND(C) attribute
!*  SECONDARY FUNCTIONS TESTED : see below
!*
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
!*      - Testing allocatable arrays of single derived types with logical, character and real components
!*      - Testing FORTRAN functions and C void functions
!*      - Main written in C
!*
!234567890123456789012345678901234567890123456789012345678901234567890

!! Test 1 Fun
function fun1(dt) bind(c)
   use ISO_C_BINDING
   integer, parameter :: DIM1 = 3, DIM2 = 2

   type, bind(c) :: dt1
      real(C_FLOAT) var_a(DIM1,DIM2)
      logical(C_BOOL) var_b(DIM1,DIM2)
      real(C_LONG_DOUBLE) var_c(DIM1,DIM2)
      real(8) var_d(DIM1,DIM2)
      real(4) var_e(DIM1,DIM2)
      logical(C_BOOL) var_f(DIM1,DIM2)
      real(C_DOUBLE) var_g(DIM1,DIM2)
      character(C_CHAR) var_h(DIM1,DIM2)
   end type

   type(dt1), dimension(DIM2,DIM1), target :: dt
   type(C_PTR) :: fun1

   do k = 1, DIM1
    do l = 1, DIM2
     do j = 1, DIM2
      do i = 1, DIM1
       if ( dt(l,k)%var_a(i,j) /= real(i+j+(j-1)*DIM2-1) ) error stop 20
       if ( (dt(l,k)%var_b(i,j) .neqv. .false.) ) error stop 22
       if ( dt(l,k)%var_c(i,j) /= dble(i+j+(j-1)*DIM2+1) ) error stop 24
       if ( dt(l,k)%var_d(i,j) /= dble(i+j+(j-1)*DIM2+2) ) error stop 26
       if ( dt(l,k)%var_e(i,j) /= real(i+j+(j-1)*DIM2+3) ) error stop 28
       if ( (dt(l,k)%var_f(i,j) .neqv. .false.) ) error stop 30
       if ( dt(l,k)%var_g(i,j) /= dble(i+j+(j-1)*DIM2+5) ) error stop 32
       if ( (dt(l,k)%var_h(i,j) /= 'A') ) error stop 34
      end do
     end do
    end do
   end do

   do k = 1, DIM1
    do l = 1, DIM2
     do j = 1, DIM2
      do i = 1, DIM1
       dt(l,k)%var_a(i,j) = real(i+j+(j-1)*DIM2)
       dt(l,k)%var_b(i,j) = .not. dt(l,k)%var_b(i,j)
       dt(l,k)%var_c(i,j) = dble(i+j+(j-1)*DIM2+2)
       dt(l,k)%var_d(i,j) = dble(i+j+(j-1)*DIM2+3)
       dt(l,k)%var_e(i,j) = real(i+j+(j-1)*DIM2+4)
       dt(l,k)%var_f(i,j) = .not. dt(l,k)%var_f(i,j)
       dt(l,k)%var_g(i,j) = dble(i+j+(j-1)*DIM2+6)
       dt(l,k)%var_h(i,j) = 'B'
      end do
     end do
    end do
   end do

   fun1 = C_LOC(dt)

end function fun1

!! Test 2 Fun
function fun2(dtx,dty) bind(c)
   use ISO_C_BINDING
   integer, parameter :: DIM1 = 3, DIM2 = 2

   type, bind(c) :: dt1
      real(C_FLOAT) var_a(DIM1,DIM2)
      logical(C_BOOL) var_b(DIM1,DIM2)
      real(C_LONG_DOUBLE) var_c(DIM1,DIM2)
      real(8) var_d(DIM1,DIM2)
      real(4) var_e(DIM1,DIM2)
      logical(C_BOOL) var_f(DIM1,DIM2)
      real(C_DOUBLE) var_g(DIM1,DIM2)
      character(C_CHAR) var_h(DIM1,DIM2)
   end type

   type(dt1), dimension(DIM2,DIM1), intent(in) :: dtx
   type(dt1), dimension(DIM2,DIM1), intent(out), target :: dty
   type(C_PTR) :: fun2

   do k = 1, DIM1
    do l = 1, DIM2
     do j = 1, DIM2
      do i = 1, DIM1
       if ( dtx(l,k)%var_a(i,j) /= real(2*(i+j+(j-1)*DIM2-1)) ) error stop 36
       if ( (dtx(l,k)%var_b(i,j) .neqv. .false.) ) error stop 38
       if ( dtx(l,k)%var_c(i,j) /= dble(2*(i+j+(j-1)*DIM2+1)) ) error stop 40
       if ( dtx(l,k)%var_d(i,j) /= dble(2*(i+j+(j-1)*DIM2+2)) ) error stop 42
       if ( dtx(l,k)%var_e(i,j) /= real(2*(i+j+(j-1)*DIM2+3)) ) error stop 44
       if ( (dtx(l,k)%var_f(i,j) .neqv. .false.) ) error stop 46
       if ( dtx(l,k)%var_g(i,j) /= dble(2*(i+j+(j-1)*DIM2+5)) ) error stop 48
       if ( (dtx(l,k)%var_h(i,j) /= 'A') ) error stop 50
      end do
     end do
    end do
   end do

   do k = 1, DIM1
    do l = 1, DIM2
     do j = 1, DIM2
      do i = 1, DIM1
       dty(l,k)%var_a(i,j) = real(2*(i+j+(j-1)*DIM2-1)+1)
       dty(l,k)%var_b(i,j) = .not. dtx(l,k)%var_b(i,j)
       dty(l,k)%var_c(i,j) = dble(2*(i+j+(j-1)*DIM2+1)+1)
       dty(l,k)%var_d(i,j) = dble(2*(i+j+(j-1)*DIM2+2)+1)
       dty(l,k)%var_e(i,j) = real(2*(i+j+(j-1)*DIM2+3)+1)
       dty(l,k)%var_f(i,j) = .not. dtx(l,k)%var_f(i,j)
       dty(l,k)%var_g(i,j) = dble(2*(i+j+(j-1)*DIM2+5)+1)
       dty(l,k)%var_h(i,j) = 'B'
      end do
     end do
    end do
   end do

   fun2 = C_LOC(dty)

end function fun2
