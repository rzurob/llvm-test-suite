!#######################################################################
!***********************************************************************
! %START
! %MAIN:
! %PRECMD: ${TR_SRC}/scrdtye00.presh fxdtyla26a cxdtyla26
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
!*      - Testing FORTRAN subroutines and C void functions
!*      - Main written in C
!*
!234567890123456789012345678901234567890123456789012345678901234567890

!! Test 1 Sub
subroutine sub1(dt) bind(c)
   use ISO_C_BINDING
   integer, parameter :: DIM1 = 3, DIM2 = 2

   type, bind(c) :: dt1
      logical(C_BOOL) var_a(DIM1,DIM2)
      real(C_DOUBLE) var_b(DIM1,DIM2)
      character(C_CHAR) var_c(DIM1,DIM2)
      real(C_FLOAT) var_d(DIM1,DIM2)
      real(C_LONG_DOUBLE) var_e(DIM1,DIM2)
      real(8) var_f(DIM1,DIM2)
      logical(C_BOOL) var_g(DIM1,DIM2)
      real(4) var_h(DIM1,DIM2)
   end type

   type, bind(c) :: dt2
      type(dt1) :: vdt1(DIM2,DIM1)
      real(C_DOUBLE) var_a(DIM1,DIM2)
      logical(C_BOOL) var_b(DIM1,DIM2)
      real(8) var_c(DIM1,DIM2)
      character(C_CHAR) var_d(DIM1,DIM2)
      real(C_LONG_DOUBLE) var_e(DIM1,DIM2)
      real(4) var_f(DIM1,DIM2)
      logical(C_BOOL) var_g(DIM1,DIM2)
      real(C_FLOAT) var_h(DIM1,DIM2)
   end type

   type, bind(c) :: dt3
      real(C_FLOAT) var_a(DIM1,DIM2)
      logical(C_BOOL) var_b(DIM1,DIM2)
      real(C_LONG_DOUBLE) var_c(DIM1,DIM2)
      real(8) var_d(DIM1,DIM2)
      real(4) var_e(DIM1,DIM2)
      logical(C_BOOL) var_f(DIM1,DIM2)
      real(C_DOUBLE) var_g(DIM1,DIM2)
      character(C_CHAR) var_h(DIM1,DIM2)
      type(dt2) :: vdt2(DIM2,DIM1)
   end type

   type(dt3), dimension(DIM2,DIM1) :: dt

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

       if ( dt(l,k)%vdt2(j,i)%var_a(i,j) /= dble(i+j+(j-1)*DIM2-1) ) error stop 36
       if ( (dt(l,k)%vdt2(j,i)%var_b(i,j) .neqv. .false.) ) error stop 38
       if ( dt(l,k)%vdt2(j,i)%var_c(i,j) /= dble(i+j+(j-1)*DIM2+1) ) error stop 40
       if ( (dt(l,k)%vdt2(j,i)%var_d(i,j) /= 'A') ) error stop 42
       if ( dt(l,k)%vdt2(j,i)%var_e(i,j) /= dble(i+j+(j-1)*DIM2+3) ) error stop 44
       if ( dt(l,k)%vdt2(j,i)%var_f(i,j) /= real(i+j+(j-1)*DIM2+4) ) error stop 46
       if ( (dt(l,k)%vdt2(j,i)%var_g(i,j) .neqv. .false.) ) error stop 48
       if ( dt(l,k)%vdt2(j,i)%var_h(i,j) /= real(i+j+(j-1)*DIM2+6) ) error stop 50

       if ( (dt(l,k)%vdt2(j,i)%vdt1(j,i)%var_a(i,j) .neqv. .false.) ) error stop 52
       if ( dt(l,k)%vdt2(j,i)%vdt1(j,i)%var_b(i,j) /= dble(i+j+(j-1)*DIM2) ) error stop 54
       if ( (dt(l,k)%vdt2(j,i)%vdt1(j,i)%var_c(i,j) /= 'A') ) error stop 56
       if ( dt(l,k)%vdt2(j,i)%vdt1(j,i)%var_d(i,j) /= real(i+j+(j-1)*DIM2+2) ) error stop 58
       if ( dt(l,k)%vdt2(j,i)%vdt1(j,i)%var_e(i,j) /= dble(i+j+(j-1)*DIM2+3) ) error stop 60
       if ( dt(l,k)%vdt2(j,i)%vdt1(j,i)%var_f(i,j) /= dble(i+j+(j-1)*DIM2+4) ) error stop 62
       if ( (dt(l,k)%vdt2(j,i)%vdt1(j,i)%var_g(i,j) .neqv. .false.) ) error stop 64
       if ( dt(l,k)%vdt2(j,i)%vdt1(j,i)%var_h(i,j) /= real(i+j+(j-1)*DIM2+6) ) error stop 66
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

       dt(l,k)%vdt2(j,i)%var_a(i,j) = dble(i+j+(j-1)*DIM2)
       dt(l,k)%vdt2(j,i)%var_b(i,j) = .not. dt(l,k)%vdt2(j,i)%var_b(i,j)
       dt(l,k)%vdt2(j,i)%var_c(i,j) = dble(i+j+(j-1)*DIM2+2)
       dt(l,k)%vdt2(j,i)%var_d(i,j) = 'B'
       dt(l,k)%vdt2(j,i)%var_e(i,j) = dble(i+j+(j-1)*DIM2+4)
       dt(l,k)%vdt2(j,i)%var_f(i,j) = real(i+j+(j-1)*DIM2+5)
       dt(l,k)%vdt2(j,i)%var_g(i,j) = .not. dt(l,k)%vdt2(j,i)%var_g(i,j)
       dt(l,k)%vdt2(j,i)%var_h(i,j) = real(i+j+(j-1)*DIM2+7)

       dt(l,k)%vdt2(j,i)%vdt1(j,i)%var_a(i,j) = .not. dt(l,k)%vdt2(j,i)%vdt1(j,i)%var_a(i,j)
       dt(l,k)%vdt2(j,i)%vdt1(j,i)%var_b(i,j) = dble(i+j+(j-1)*DIM2+1)
       dt(l,k)%vdt2(j,i)%vdt1(j,i)%var_c(i,j) = 'B'
       dt(l,k)%vdt2(j,i)%vdt1(j,i)%var_d(i,j) = real(i+j+(j-1)*DIM2+3)
       dt(l,k)%vdt2(j,i)%vdt1(j,i)%var_e(i,j) = dble(i+j+(j-1)*DIM2+4)
       dt(l,k)%vdt2(j,i)%vdt1(j,i)%var_f(i,j) = dble(i+j+(j-1)*DIM2+5)
       dt(l,k)%vdt2(j,i)%vdt1(j,i)%var_g(i,j) = .not. dt(l,k)%vdt2(j,i)%vdt1(j,i)%var_g(i,j)
       dt(l,k)%vdt2(j,i)%vdt1(j,i)%var_h(i,j) = real(i+j+(j-1)*DIM2+7)
      end do
     end do
    end do
   end do

end subroutine sub1

!! Test 2 Sub
subroutine sub2(dtx,dty) bind(c)
   use ISO_C_BINDING
   integer, parameter :: DIM1 = 3, DIM2 = 2

   type, bind(c) :: dt1
      logical(C_BOOL) var_a(DIM1,DIM2)
      real(C_DOUBLE) var_b(DIM1,DIM2)
      character(C_CHAR) var_c(DIM1,DIM2)
      real(C_FLOAT) var_d(DIM1,DIM2)
      real(C_LONG_DOUBLE) var_e(DIM1,DIM2)
      real(8) var_f(DIM1,DIM2)
      logical(C_BOOL) var_g(DIM1,DIM2)
      real(4) var_h(DIM1,DIM2)
   end type

   type, bind(c) :: dt2
      type(dt1) :: vdt1(DIM2,DIM1)
      real(C_DOUBLE) var_a(DIM1,DIM2)
      logical(C_BOOL) var_b(DIM1,DIM2)
      real(8) var_c(DIM1,DIM2)
      character(C_CHAR) var_d(DIM1,DIM2)
      real(C_LONG_DOUBLE) var_e(DIM1,DIM2)
      real(4) var_f(DIM1,DIM2)
      logical(C_BOOL) var_g(DIM1,DIM2)
      real(C_FLOAT) var_h(DIM1,DIM2)
   end type

   type, bind(c) :: dt3
      real(C_FLOAT) var_a(DIM1,DIM2)
      logical(C_BOOL) var_b(DIM1,DIM2)
      real(C_LONG_DOUBLE) var_c(DIM1,DIM2)
      real(8) var_d(DIM1,DIM2)
      real(4) var_e(DIM1,DIM2)
      logical(C_BOOL) var_f(DIM1,DIM2)
      real(C_DOUBLE) var_g(DIM1,DIM2)
      character(C_CHAR) var_h(DIM1,DIM2)
      type(dt2) :: vdt2(DIM2,DIM1)
   end type

   type(dt3), dimension(DIM2,DIM1), intent(in) :: dtx
   type(dt3), dimension(DIM2,DIM1), intent(out) :: dty

   do k = 1, DIM1
    do l = 1, DIM2
     do j = 1, DIM2
      do i = 1, DIM1
       if ( dtx(l,k)%var_a(i,j) /= real(2*(i+j+(j-1)*DIM2-1)) ) error stop 68
       if ( (dtx(l,k)%var_b(i,j) .neqv. .false.) ) error stop 70
       if ( dtx(l,k)%var_c(i,j) /= dble(2*(i+j+(j-1)*DIM2+1)) ) error stop 72
       if ( dtx(l,k)%var_d(i,j) /= dble(2*(i+j+(j-1)*DIM2+2)) ) error stop 74
       if ( dtx(l,k)%var_e(i,j) /= real(2*(i+j+(j-1)*DIM2+3)) ) error stop 76
       if ( (dtx(l,k)%var_f(i,j) .neqv. .false.) ) error stop 78
       if ( dtx(l,k)%var_g(i,j) /= dble(2*(i+j+(j-1)*DIM2+5)) ) error stop 80
       if ( (dtx(l,k)%var_h(i,j) /= 'A') ) error stop 82

       if ( dtx(l,k)%vdt2(j,i)%var_a(i,j) /= dble(2*(i+j+(j-1)*DIM2-1)) ) error stop 84
       if ( (dtx(l,k)%vdt2(j,i)%var_b(i,j) .neqv. .false.) ) error stop 86
       if ( dtx(l,k)%vdt2(j,i)%var_c(i,j) /= dble(2*(i+j+(j-1)*DIM2+1)) ) error stop 88
       if ( (dtx(l,k)%vdt2(j,i)%var_d(i,j) /= 'A') ) error stop 90
       if ( dtx(l,k)%vdt2(j,i)%var_e(i,j) /= dble(2*(i+j+(j-1)*DIM2+3)) ) error stop 92
       if ( dtx(l,k)%vdt2(j,i)%var_f(i,j) /= real(2*(i+j+(j-1)*DIM2+4)) ) error stop 94
       if ( (dtx(l,k)%vdt2(j,i)%var_g(i,j) .neqv. .false.) ) error stop 96
       if ( dtx(l,k)%vdt2(j,i)%var_h(i,j) /= real(2*(i+j+(j-1)*DIM2+6)) ) error stop 98

       if ( (dtx(l,k)%vdt2(j,i)%vdt1(j,i)%var_a(i,j) .neqv. .false.) ) error stop 100
       if ( dtx(l,k)%vdt2(j,i)%vdt1(j,i)%var_b(i,j) /= dble(2*(i+j+(j-1)*DIM2)) ) error stop 102
       if ( (dtx(l,k)%vdt2(j,i)%vdt1(j,i)%var_c(i,j) /= 'A') ) error stop 104
       if ( dtx(l,k)%vdt2(j,i)%vdt1(j,i)%var_d(i,j) /= real(2*(i+j+(j-1)*DIM2+2)) ) error stop 106
       if ( dtx(l,k)%vdt2(j,i)%vdt1(j,i)%var_e(i,j) /= dble(2*(i+j+(j-1)*DIM2+3)) ) error stop 108
       if ( dtx(l,k)%vdt2(j,i)%vdt1(j,i)%var_f(i,j) /= dble(2*(i+j+(j-1)*DIM2+4)) ) error stop 110
       if ( (dtx(l,k)%vdt2(j,i)%vdt1(j,i)%var_g(i,j) .neqv. .false.) ) error stop 112
       if ( dtx(l,k)%vdt2(j,i)%vdt1(j,i)%var_h(i,j) /= real(2*(i+j+(j-1)*DIM2+6)) ) error stop 114
      end do
     end do
    end do
   end do

   do k = 1, DIM1
    do l = 1, DIM2
     do j = 1, DIM2
      do i = 1, DIM1
       dty(l,k)%var_a(i,j) = real(dtx(l,k)%var_a(i,j)) + 1.0e0
       dty(l,k)%var_b(i,j) = .not. dtx(l,k)%var_b(i,j)
       dty(l,k)%var_c(i,j) = dble(dtx(l,k)%var_c(i,j)) + 3.0q0
       dty(l,k)%var_d(i,j) = dble(dtx(l,k)%var_d(i,j)) + 4.0d0
       dty(l,k)%var_e(i,j) = real(dtx(l,k)%var_e(i,j)) + 5.0e0
       dty(l,k)%var_f(i,j) = .not. dtx(l,k)%var_f(i,j)
       dty(l,k)%var_g(i,j) = dble(dtx(l,k)%var_g(i,j)) + 7.0d0
       dty(l,k)%var_h(i,j) = 'B'

       dty(l,k)%vdt2(j,i)%var_a(i,j) = dble(dtx(l,k)%vdt2(j,i)%var_a(i,j)) + 1.0d0
       dty(l,k)%vdt2(j,i)%var_b(i,j) = .not. dtx(l,k)%vdt2(j,i)%var_b(i,j)
       dty(l,k)%vdt2(j,i)%var_c(i,j) = dble(dtx(l,k)%vdt2(j,i)%var_c(i,j)) + 3.0d0
       dty(l,k)%vdt2(j,i)%var_d(i,j) = 'B'
       dty(l,k)%vdt2(j,i)%var_e(i,j) = dble(dtx(l,k)%vdt2(j,i)%var_e(i,j)) + 5.0q0
       dty(l,k)%vdt2(j,i)%var_f(i,j) = real(dtx(l,k)%vdt2(j,i)%var_f(i,j)) + 6.0e0
       dty(l,k)%vdt2(j,i)%var_g(i,j) = .not. dtx(l,k)%vdt2(j,i)%var_g(i,j)
       dty(l,k)%vdt2(j,i)%var_h(i,j) = real(dtx(l,k)%vdt2(j,i)%var_h(i,j)) + 8.0e0

       dty(l,k)%vdt2(j,i)%vdt1(j,i)%var_a(i,j) = .not. dtx(l,k)%vdt2(j,i)%vdt1(j,i)%var_a(i,j)
       dty(l,k)%vdt2(j,i)%vdt1(j,i)%var_b(i,j) = dble(dtx(l,k)%vdt2(j,i)%vdt1(j,i)%var_b(i,j)) + 2.0d0
       dty(l,k)%vdt2(j,i)%vdt1(j,i)%var_c(i,j) = 'B'
       dty(l,k)%vdt2(j,i)%vdt1(j,i)%var_d(i,j) = real(dtx(l,k)%vdt2(j,i)%vdt1(j,i)%var_d(i,j)) + 4.0e0
       dty(l,k)%vdt2(j,i)%vdt1(j,i)%var_e(i,j) = dble(dtx(l,k)%vdt2(j,i)%vdt1(j,i)%var_e(i,j)) + 5.0q0
       dty(l,k)%vdt2(j,i)%vdt1(j,i)%var_f(i,j) = dble(dtx(l,k)%vdt2(j,i)%vdt1(j,i)%var_f(i,j)) + 6.0d0
       dty(l,k)%vdt2(j,i)%vdt1(j,i)%var_g(i,j) = .not. dtx(l,k)%vdt2(j,i)%vdt1(j,i)%var_g(i,j)
       dty(l,k)%vdt2(j,i)%vdt1(j,i)%var_h(i,j) = real(dtx(l,k)%vdt2(j,i)%vdt1(j,i)%var_h(i,j)) + 8.0e0
      end do
     end do
    end do
   end do

end subroutine sub2
