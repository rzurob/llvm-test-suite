!***********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 4/23/2002
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

   type, bind(c) :: dt2
      integer(C_INT16_T) var_a(DIM1,DIM2)
      integer(C_INT_FAST8_T) var_b(DIM1,DIM2)
      integer(C_INT64_T) var_c(DIM1,DIM2)
      integer(C_INT8_T) var_d(DIM1,DIM2)
      integer(C_INT_FAST32_T) var_e(DIM1,DIM2)
      integer(C_INT_FAST16_T) var_f(DIM1,DIM2)
      integer(C_INT32_T) var_g(DIM1,DIM2)
      integer(C_INT_FAST64_T) var_h(DIM1,DIM2)
      type(dt1) :: vdt1(DIM2,DIM1)
   end type

   type, bind(c) :: dt3
      type(dt2) :: vdt2(DIM2,DIM1)
      integer(C_SIZE_T) var_a(DIM1,DIM2)
      integer(C_INT_LEAST8_T) var_b(DIM1,DIM2)
      integer(C_INT_LEAST64_T) var_c(DIM1,DIM2)
      integer(C_SIGNED_CHAR) var_d(DIM1,DIM2)
      integer(C_INTPTR_T) var_e(DIM1,DIM2)
      integer(C_INT_LEAST16_T) var_f(DIM1,DIM2)
      integer(C_INTMAX_T) var_g(DIM1,DIM2)
      integer(C_INT_LEAST32_T) var_h(DIM1,DIM2)
   end type

   type(dt3), dimension(DIM2,DIM1), target :: dt
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

       if ( dt(l,k)%vdt2(j,i)%var_a(i,j) /= i+j+(j-1)*DIM2-1 ) error stop 36
       if ( dt(l,k)%vdt2(j,i)%var_b(i,j) /= i+j+(j-1)*DIM2 ) error stop 38
       if ( dt(l,k)%vdt2(j,i)%var_c(i,j) /= i+j+(j-1)*DIM2+1 ) error stop 40
       if ( dt(l,k)%vdt2(j,i)%var_d(i,j) /= i+j+(j-1)*DIM2+2 ) error stop 42
       if ( dt(l,k)%vdt2(j,i)%var_e(i,j) /= i+j+(j-1)*DIM2+3 ) error stop 44
       if ( dt(l,k)%vdt2(j,i)%var_f(i,j) /= i+j+(j-1)*DIM2+4 ) error stop 46
       if ( dt(l,k)%vdt2(j,i)%var_g(i,j) /= i+j+(j-1)*DIM2+5 ) error stop 48
       if ( dt(l,k)%vdt2(j,i)%var_h(i,j) /= i+j+(j-1)*DIM2+6 ) error stop 50

       if ( dt(l,k)%vdt2(j,i)%vdt1(j,i)%var_a(i,j) /= i+j+(j-1)*DIM2-1 ) error stop 52
       if ( dt(l,k)%vdt2(j,i)%vdt1(j,i)%var_b(i,j) /= i+j+(j-1)*DIM2 ) error stop 54
       if ( dt(l,k)%vdt2(j,i)%vdt1(j,i)%var_c(i,j) /= i+j+(j-1)*DIM2+1 ) error stop 56
       if ( dt(l,k)%vdt2(j,i)%vdt1(j,i)%var_d(i,j) /= i+j+(j-1)*DIM2+2 ) error stop 58
       if ( dt(l,k)%vdt2(j,i)%vdt1(j,i)%var_e(i,j) /= i+j+(j-1)*DIM2+3 ) error stop 60
       if ( dt(l,k)%vdt2(j,i)%vdt1(j,i)%var_f(i,j) /= i+j+(j-1)*DIM2+4 ) error stop 62
       if ( dt(l,k)%vdt2(j,i)%vdt1(j,i)%var_g(i,j) /= i+j+(j-1)*DIM2+5 ) error stop 64
       if ( dt(l,k)%vdt2(j,i)%vdt1(j,i)%var_h(i,j) /= i+j+(j-1)*DIM2+6 ) error stop 66
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

       dt(l,k)%vdt2(j,i)%var_a(i,j) = i+j+(j-1)*DIM2
       dt(l,k)%vdt2(j,i)%var_b(i,j) = i+j+(j-1)*DIM2+1
       dt(l,k)%vdt2(j,i)%var_c(i,j) = i+j+(j-1)*DIM2+2
       dt(l,k)%vdt2(j,i)%var_d(i,j) = i+j+(j-1)*DIM2+3
       dt(l,k)%vdt2(j,i)%var_e(i,j) = i+j+(j-1)*DIM2+4
       dt(l,k)%vdt2(j,i)%var_f(i,j) = i+j+(j-1)*DIM2+5
       dt(l,k)%vdt2(j,i)%var_g(i,j) = i+j+(j-1)*DIM2+6
       dt(l,k)%vdt2(j,i)%var_h(i,j) = i+j+(j-1)*DIM2+7

       dt(l,k)%vdt2(j,i)%vdt1(j,i)%var_a(i,j) = i+j+(j-1)*DIM2
       dt(l,k)%vdt2(j,i)%vdt1(j,i)%var_b(i,j) = i+j+(j-1)*DIM2+1
       dt(l,k)%vdt2(j,i)%vdt1(j,i)%var_c(i,j) = i+j+(j-1)*DIM2+2
       dt(l,k)%vdt2(j,i)%vdt1(j,i)%var_d(i,j) = i+j+(j-1)*DIM2+3
       dt(l,k)%vdt2(j,i)%vdt1(j,i)%var_e(i,j) = i+j+(j-1)*DIM2+4
       dt(l,k)%vdt2(j,i)%vdt1(j,i)%var_f(i,j) = i+j+(j-1)*DIM2+5
       dt(l,k)%vdt2(j,i)%vdt1(j,i)%var_g(i,j) = i+j+(j-1)*DIM2+6
       dt(l,k)%vdt2(j,i)%vdt1(j,i)%var_h(i,j) = i+j+(j-1)*DIM2+7
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

   type, bind(c) :: dt2
      integer(C_INT16_T) var_a(DIM1,DIM2)
      integer(C_INT_FAST8_T) var_b(DIM1,DIM2)
      integer(C_INT64_T) var_c(DIM1,DIM2)
      integer(C_INT8_T) var_d(DIM1,DIM2)
      integer(C_INT_FAST32_T) var_e(DIM1,DIM2)
      integer(C_INT_FAST16_T) var_f(DIM1,DIM2)
      integer(C_INT32_T) var_g(DIM1,DIM2)
      integer(C_INT_FAST64_T) var_h(DIM1,DIM2)
      type(dt1) :: vdt1(DIM2,DIM1)
   end type

   type, bind(c) :: dt3
      type(dt2) :: vdt2(DIM2,DIM1)
      integer(C_SIZE_T) var_a(DIM1,DIM2)
      integer(C_INT_LEAST8_T) var_b(DIM1,DIM2)
      integer(C_INT_LEAST64_T) var_c(DIM1,DIM2)
      integer(C_SIGNED_CHAR) var_d(DIM1,DIM2)
      integer(C_INTPTR_T) var_e(DIM1,DIM2)
      integer(C_INT_LEAST16_T) var_f(DIM1,DIM2)
      integer(C_INTMAX_T) var_g(DIM1,DIM2)
      integer(C_INT_LEAST32_T) var_h(DIM1,DIM2)
   end type

   type(dt3), dimension(DIM2,DIM1), intent(in) :: dtx
   type(dt3), dimension(DIM2,DIM1), intent(out), target :: dty
   type(C_PTR) :: fun2

   do k = 1, DIM1
    do l = 1, DIM2
     do j = 1, DIM2
      do i = 1, DIM1
       if ( dtx(l,k)%var_a(i,j) /= 2*(i+j+(j-1)*DIM2-1) ) error stop 68
       if ( dtx(l,k)%var_b(i,j) /= 2*(i+j+(j-1)*DIM2) ) error stop 70
       if ( dtx(l,k)%var_c(i,j) /= 2*(i+j+(j-1)*DIM2+1) ) error stop 72
       if ( dtx(l,k)%var_d(i,j) /= 2*(i+j+(j-1)*DIM2+2) ) error stop 74
       if ( dtx(l,k)%var_e(i,j) /= 2*(i+j+(j-1)*DIM2+3) ) error stop 76
       if ( dtx(l,k)%var_f(i,j) /= 2*(i+j+(j-1)*DIM2+4) ) error stop 78
       if ( dtx(l,k)%var_g(i,j) /= 2*(i+j+(j-1)*DIM2+5) ) error stop 80
       if ( dtx(l,k)%var_h(i,j) /= 2*(i+j+(j-1)*DIM2+6) ) error stop 82

       if ( dtx(l,k)%vdt2(j,i)%var_a(i,j) /= 2*(i+j+(j-1)*DIM2-1) ) error stop 84
       if ( dtx(l,k)%vdt2(j,i)%var_b(i,j) /= 2*(i+j+(j-1)*DIM2) ) error stop 86
       if ( dtx(l,k)%vdt2(j,i)%var_c(i,j) /= 2*(i+j+(j-1)*DIM2+1) ) error stop 88
       if ( dtx(l,k)%vdt2(j,i)%var_d(i,j) /= 2*(i+j+(j-1)*DIM2+2) ) error stop 90
       if ( dtx(l,k)%vdt2(j,i)%var_e(i,j) /= 2*(i+j+(j-1)*DIM2+3) ) error stop 92
       if ( dtx(l,k)%vdt2(j,i)%var_f(i,j) /= 2*(i+j+(j-1)*DIM2+4) ) error stop 94
       if ( dtx(l,k)%vdt2(j,i)%var_g(i,j) /= 2*(i+j+(j-1)*DIM2+5) ) error stop 96
       if ( dtx(l,k)%vdt2(j,i)%var_h(i,j) /= 2*(i+j+(j-1)*DIM2+6) ) error stop 98

       if ( dtx(l,k)%vdt2(j,i)%vdt1(j,i)%var_a(i,j) /= 2*(i+j+(j-1)*DIM2-1) ) error stop 100
       if ( dtx(l,k)%vdt2(j,i)%vdt1(j,i)%var_b(i,j) /= 2*(i+j+(j-1)*DIM2) ) error stop 102
       if ( dtx(l,k)%vdt2(j,i)%vdt1(j,i)%var_c(i,j) /= 2*(i+j+(j-1)*DIM2+1) ) error stop 104
       if ( dtx(l,k)%vdt2(j,i)%vdt1(j,i)%var_d(i,j) /= 2*(i+j+(j-1)*DIM2+2) ) error stop 106
       if ( dtx(l,k)%vdt2(j,i)%vdt1(j,i)%var_e(i,j) /= 2*(i+j+(j-1)*DIM2+3) ) error stop 108
       if ( dtx(l,k)%vdt2(j,i)%vdt1(j,i)%var_f(i,j) /= 2*(i+j+(j-1)*DIM2+4) ) error stop 110
       if ( dtx(l,k)%vdt2(j,i)%vdt1(j,i)%var_g(i,j) /= 2*(i+j+(j-1)*DIM2+5) ) error stop 112
       if ( dtx(l,k)%vdt2(j,i)%vdt1(j,i)%var_h(i,j) /= 2*(i+j+(j-1)*DIM2+6) ) error stop 114
      end do
     end do
    end do
   end do

   do k = 1, DIM1
    do l = 1, DIM2
     do j = 1, DIM2
      do i = 1, DIM1
       dty(l,k)%var_a(i,j) = dtx(l,k)%var_a(i,j) + 1
       dty(l,k)%var_b(i,j) = dtx(l,k)%var_b(i,j) + 2
       dty(l,k)%var_c(i,j) = dtx(l,k)%var_c(i,j) + 3
       dty(l,k)%var_d(i,j) = dtx(l,k)%var_d(i,j) + 4
       dty(l,k)%var_e(i,j) = dtx(l,k)%var_e(i,j) + 5
       dty(l,k)%var_f(i,j) = dtx(l,k)%var_f(i,j) + 6
       dty(l,k)%var_g(i,j) = dtx(l,k)%var_g(i,j) + 7
       dty(l,k)%var_h(i,j) = dtx(l,k)%var_h(i,j) + 8

       dty(l,k)%vdt2(j,i)%var_a(i,j) = dtx(l,k)%vdt2(j,i)%var_a(i,j) + 1
       dty(l,k)%vdt2(j,i)%var_b(i,j) = dtx(l,k)%vdt2(j,i)%var_b(i,j) + 2
       dty(l,k)%vdt2(j,i)%var_c(i,j) = dtx(l,k)%vdt2(j,i)%var_c(i,j) + 3
       dty(l,k)%vdt2(j,i)%var_d(i,j) = dtx(l,k)%vdt2(j,i)%var_d(i,j) + 4
       dty(l,k)%vdt2(j,i)%var_e(i,j) = dtx(l,k)%vdt2(j,i)%var_e(i,j) + 5
       dty(l,k)%vdt2(j,i)%var_f(i,j) = dtx(l,k)%vdt2(j,i)%var_f(i,j) + 6
       dty(l,k)%vdt2(j,i)%var_g(i,j) = dtx(l,k)%vdt2(j,i)%var_g(i,j) + 7
       dty(l,k)%vdt2(j,i)%var_h(i,j) = dtx(l,k)%vdt2(j,i)%var_h(i,j) + 8

       dty(l,k)%vdt2(j,i)%vdt1(j,i)%var_a(i,j) = dtx(l,k)%vdt2(j,i)%vdt1(j,i)%var_a(i,j) + 1
       dty(l,k)%vdt2(j,i)%vdt1(j,i)%var_b(i,j) = dtx(l,k)%vdt2(j,i)%vdt1(j,i)%var_b(i,j) + 2
       dty(l,k)%vdt2(j,i)%vdt1(j,i)%var_c(i,j) = dtx(l,k)%vdt2(j,i)%vdt1(j,i)%var_c(i,j) + 3
       dty(l,k)%vdt2(j,i)%vdt1(j,i)%var_d(i,j) = dtx(l,k)%vdt2(j,i)%vdt1(j,i)%var_d(i,j) + 4
       dty(l,k)%vdt2(j,i)%vdt1(j,i)%var_e(i,j) = dtx(l,k)%vdt2(j,i)%vdt1(j,i)%var_e(i,j) + 5
       dty(l,k)%vdt2(j,i)%vdt1(j,i)%var_f(i,j) = dtx(l,k)%vdt2(j,i)%vdt1(j,i)%var_f(i,j) + 6
       dty(l,k)%vdt2(j,i)%vdt1(j,i)%var_g(i,j) = dtx(l,k)%vdt2(j,i)%vdt1(j,i)%var_g(i,j) + 7
       dty(l,k)%vdt2(j,i)%vdt1(j,i)%var_h(i,j) = dtx(l,k)%vdt2(j,i)%vdt1(j,i)%var_h(i,j) + 8
      end do
     end do
    end do
   end do

   fun2 = C_LOC(dty)
end function fun2
