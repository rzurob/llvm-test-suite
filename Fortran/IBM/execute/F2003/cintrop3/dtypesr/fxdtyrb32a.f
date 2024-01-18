!#######################################################################
!***********************************************************************
! %START
! %MAIN:
! %PRECMD: ${TR_SRC}/scrdtyb00.presh fxdtyrb32a cxdtyra32
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
!*      - Testing arrays of 2-levels deep derived types with BIND(C) attribute
!*      - Testing arrays of 2-levels deep derived types with INTENT attributes
!*      - Testing arrays of 2-levels deep derived types with integer and real components
!*      - Testing FORTRAN subroutines and C void functions
!*      - Main written in C
!*
!234567890123456789012345678901234567890123456789012345678901234567890

subroutine sub1(dt) bind(c)
   use ISO_C_BINDING
   integer, parameter :: DIM1 = 3, DIM2 = 2

   type, bind(c) :: dt1
      integer(C_INT_FAST16_T) var_a(DIM1,DIM2)
      real(C_DOUBLE) var_b(DIM1,DIM2)
      integer(C_SIGNED_CHAR) var_c(DIM1,DIM2)
      real(C_FLOAT) var_d(DIM1,DIM2)
      real(16) var_e(DIM1,DIM2)
      real(8) var_f(DIM1,DIM2)
      integer(C_INTMAX_T) var_g(DIM1,DIM2)
      real(4) var_h(DIM1,DIM2)
   end type

   type, bind(c) :: dt2
      real(C_FLOAT) var_a(DIM1,DIM2)
      integer(C_SHORT) var_b(DIM1,DIM2)
      real(16) var_c(DIM1,DIM2)
      real(8) var_d(DIM1,DIM2)
      real(4) var_e(DIM1,DIM2)
      integer(1) var_f(DIM1,DIM2)
      real(C_DOUBLE) var_g(DIM1,DIM2)
      integer(4) var_h(DIM1,DIM2)
      type(dt1) :: vdt1(DIM2,DIM1)
   end type

   type(dt2), dimension(DIM2,DIM1) :: dt

   do k = 1, DIM1
    do l = 1, DIM2
     do j = 1, DIM2
      do i = 1, DIM1
       if ( dt(l,k)%var_a(i,j) /= real(i+j+(j-1)*DIM2-1) ) error stop 20
       if ( dt(l,k)%var_b(i,j) /= i+j+(j-1)*DIM2 ) error stop 22
       if ( dt(l,k)%var_c(i,j) /= dble(i+j+(j-1)*DIM2+1) ) error stop 24
       if ( dt(l,k)%var_d(i,j) /= dble(i+j+(j-1)*DIM2+2) ) error stop 26
       if ( dt(l,k)%var_e(i,j) /= real(i+j+(j-1)*DIM2+3) ) error stop 28
       if ( dt(l,k)%var_f(i,j) /= i+j+(j-1)*DIM2+4 ) error stop 30
       if ( dt(l,k)%var_g(i,j) /= dble(i+j+(j-1)*DIM2+5) ) error stop 32
       if ( dt(l,k)%var_h(i,j) /= i+j+(j-1)*DIM2+6 ) error stop 34

       if ( dt(l,k)%vdt1(j,i)%var_a(i,j) /= i+j+(j-1)*DIM2-1 ) error stop 36
       if ( dt(l,k)%vdt1(j,i)%var_b(i,j) /= dble(i+j+(j-1)*DIM2) ) error stop 38
       if ( dt(l,k)%vdt1(j,i)%var_c(i,j) /= i+j+(j-1)*DIM2+1 ) error stop 40
       if ( dt(l,k)%vdt1(j,i)%var_d(i,j) /= real(i+j+(j-1)*DIM2+2) ) error stop 42
       if ( dt(l,k)%vdt1(j,i)%var_e(i,j) /= dble(i+j+(j-1)*DIM2+3) ) error stop 44
       if ( dt(l,k)%vdt1(j,i)%var_f(i,j) /= dble(i+j+(j-1)*DIM2+4) ) error stop 46
       if ( dt(l,k)%vdt1(j,i)%var_g(i,j) /= i+j+(j-1)*DIM2+5 ) error stop 48
       if ( dt(l,k)%vdt1(j,i)%var_h(i,j) /= real(i+j+(j-1)*DIM2+6) ) error stop 50
      end do
     end do
    end do
   end do

   do k = 1, DIM1
    do l = 1, DIM2
     do j = 1, DIM2
      do i = 1, DIM1
       dt(l,k)%var_a(i,j) = real(i+j+(j-1)*DIM2)
       dt(l,k)%var_b(i,j) = i+j+(j-1)*DIM2+1
       dt(l,k)%var_c(i,j) = dble(i+j+(j-1)*DIM2+2)
       dt(l,k)%var_d(i,j) = dble(i+j+(j-1)*DIM2+3)
       dt(l,k)%var_e(i,j) = real(i+j+(j-1)*DIM2+4)
       dt(l,k)%var_f(i,j) = i+j+(j-1)*DIM2+5
       dt(l,k)%var_g(i,j) = dble(i+j+(j-1)*DIM2+6)
       dt(l,k)%var_h(i,j) = i+j+(j-1)*DIM2+7

       dt(l,k)%vdt1(j,i)%var_a(i,j) = i+j+(j-1)*DIM2
       dt(l,k)%vdt1(j,i)%var_b(i,j) = dble(i+j+(j-1)*DIM2+1)
       dt(l,k)%vdt1(j,i)%var_c(i,j) = i+j+(j-1)*DIM2+2
       dt(l,k)%vdt1(j,i)%var_d(i,j) = real(i+j+(j-1)*DIM2+3)
       dt(l,k)%vdt1(j,i)%var_e(i,j) = dble(i+j+(j-1)*DIM2+4)
       dt(l,k)%vdt1(j,i)%var_f(i,j) = dble(i+j+(j-1)*DIM2+5)
       dt(l,k)%vdt1(j,i)%var_g(i,j) = i+j+(j-1)*DIM2+6
       dt(l,k)%vdt1(j,i)%var_h(i,j) = real(i+j+(j-1)*DIM2+7)
      end do
     end do
    end do
   end do

end subroutine sub1

subroutine sub2(dtx,dty) bind(c)
   use ISO_C_BINDING
   integer, parameter :: DIM1 = 3, DIM2 = 2

   type, bind(c) :: dt1
      integer(C_INT_FAST16_T) var_a(DIM1,DIM2)
      real(C_DOUBLE) var_b(DIM1,DIM2)
      integer(C_SIGNED_CHAR) var_c(DIM1,DIM2)
      real(C_FLOAT) var_d(DIM1,DIM2)
      real(16) var_e(DIM1,DIM2)
      real(8) var_f(DIM1,DIM2)
      integer(C_INTMAX_T) var_g(DIM1,DIM2)
      real(4) var_h(DIM1,DIM2)
   end type

   type, bind(c) :: dt2
      real(C_FLOAT) var_a(DIM1,DIM2)
      integer(C_SHORT) var_b(DIM1,DIM2)
      real(16) var_c(DIM1,DIM2)
      real(8) var_d(DIM1,DIM2)
      real(4) var_e(DIM1,DIM2)
      integer(1) var_f(DIM1,DIM2)
      real(C_DOUBLE) var_g(DIM1,DIM2)
      integer(4) var_h(DIM1,DIM2)
      type(dt1) :: vdt1(DIM2,DIM1)
   end type

   type(dt2), dimension(DIM2,DIM1), intent(in) :: dtx
   type(dt2), dimension(DIM2,DIM1), intent(out) :: dty

   do k = 1, DIM1
    do l = 1, DIM2
     do j = 1, DIM2
      do i = 1, DIM1
       if ( dtx(l,k)%var_a(i,j) /= real(2*(i+j+(j-1)*DIM2-1)) ) error stop 52
       if ( dtx(l,k)%var_b(i,j) /= 2*(i+j+(j-1)*DIM2) ) error stop 54
       if ( dtx(l,k)%var_c(i,j) /= dble(2*(i+j+(j-1)*DIM2+1)) ) error stop 56
       if ( dtx(l,k)%var_d(i,j) /= dble(2*(i+j+(j-1)*DIM2+2)) ) error stop 58
       if ( dtx(l,k)%var_e(i,j) /= real(2*(i+j+(j-1)*DIM2+3)) ) error stop 60
       if ( dtx(l,k)%var_f(i,j) /= 2*(i+j+(j-1)*DIM2+4) ) error stop 62
       if ( dtx(l,k)%var_g(i,j) /= dble(2*(i+j+(j-1)*DIM2+5)) ) error stop 64
       if ( dtx(l,k)%var_h(i,j) /= 2*(i+j+(j-1)*DIM2+6) ) error stop 66

       if ( dtx(l,k)%vdt1(j,i)%var_a(i,j) /= 2*(i+j+(j-1)*DIM2-1) ) error stop 68
       if ( dtx(l,k)%vdt1(j,i)%var_b(i,j) /= dble(2*(i+j+(j-1)*DIM2)) ) error stop 70
       if ( dtx(l,k)%vdt1(j,i)%var_c(i,j) /= 2*(i+j+(j-1)*DIM2+1) ) error stop 72
       if ( dtx(l,k)%vdt1(j,i)%var_d(i,j) /= real(2*(i+j+(j-1)*DIM2+2)) ) error stop 74
       if ( dtx(l,k)%vdt1(j,i)%var_e(i,j) /= dble(2*(i+j+(j-1)*DIM2+3)) ) error stop 76
       if ( dtx(l,k)%vdt1(j,i)%var_f(i,j) /= dble(2*(i+j+(j-1)*DIM2+4)) ) error stop 78
       if ( dtx(l,k)%vdt1(j,i)%var_g(i,j) /= 2*(i+j+(j-1)*DIM2+5) ) error stop 80
       if ( dtx(l,k)%vdt1(j,i)%var_h(i,j) /= real(2*(i+j+(j-1)*DIM2+6)) ) error stop 82
      end do
     end do
    end do
   end do

   do k = 1, DIM1
    do l = 1, DIM2
     do j = 1, DIM2
      do i = 1, DIM1
       dty(l,k)%var_a(i,j) = real(dtx(l,k)%var_a(i,j)) + 1.0e0
       dty(l,k)%var_b(i,j) = dtx(l,k)%var_b(i,j) + 2
       dty(l,k)%var_c(i,j) = dble(dtx(l,k)%var_c(i,j)) + 3.0q0
       dty(l,k)%var_d(i,j) = dble(dtx(l,k)%var_d(i,j)) + 4.0d0
       dty(l,k)%var_e(i,j) = real(dtx(l,k)%var_e(i,j)) + 5.0e0
       dty(l,k)%var_f(i,j) = dtx(l,k)%var_f(i,j) + 6
       dty(l,k)%var_g(i,j) = dble(dtx(l,k)%var_g(i,j)) + 7.0d0
       dty(l,k)%var_h(i,j) = dtx(l,k)%var_h(i,j) + 8

       dty(l,k)%vdt1(j,i)%var_a(i,j) = dtx(l,k)%vdt1(j,i)%var_a(i,j) + 1
       dty(l,k)%vdt1(j,i)%var_b(i,j) = dble(dtx(l,k)%vdt1(j,i)%var_b(i,j)) + 2.0d0
       dty(l,k)%vdt1(j,i)%var_c(i,j) = dtx(l,k)%vdt1(j,i)%var_c(i,j) + 3
       dty(l,k)%vdt1(j,i)%var_d(i,j) = real(dtx(l,k)%vdt1(j,i)%var_d(i,j)) + 4.0e0
       dty(l,k)%vdt1(j,i)%var_e(i,j) = dble(dtx(l,k)%vdt1(j,i)%var_e(i,j)) + 5.0q0
       dty(l,k)%vdt1(j,i)%var_f(i,j) = dble(dtx(l,k)%vdt1(j,i)%var_f(i,j)) + 6.0d0
       dty(l,k)%vdt1(j,i)%var_g(i,j) = dtx(l,k)%vdt1(j,i)%var_g(i,j) + 7
       dty(l,k)%vdt1(j,i)%var_h(i,j) = real(dtx(l,k)%vdt1(j,i)%var_h(i,j)) + 8.0e0
      end do
     end do
    end do
   end do

end subroutine sub2
