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
!*      - Testing arrays of 2-levels deep derived types with BIND(C) attribute
!*      - Testing arrays of 2-levels deep derived types with INTENT attributes
!*      - Testing arrays of 2-levels deep derived types with logical, character and real components
!*      - Testing FORTRAN functions and C void functions
!*      - Main written in C
!*
!234567890123456789012345678901234567890123456789012345678901234567890


module mxdtyla33
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
      real(C_FLOAT) var_a(DIM1,DIM2)
      logical(C_BOOL) var_b(DIM1,DIM2)
      real(C_LONG_DOUBLE) var_c(DIM1,DIM2)
      real(8) var_d(DIM1,DIM2)
      real(4) var_e(DIM1,DIM2)
      logical(C_BOOL) var_f(DIM1,DIM2)
      real(C_DOUBLE) var_g(DIM1,DIM2)
      character(C_CHAR) var_h(DIM1,DIM2)
      type(dt1) :: vdt1(DIM2,DIM1)
   end type

end module mxdtyla33

!! Test 1 Fun
function fun1(dt) bind(c)
   use mxdtyla33
   type(dt2), dimension(DIM2,DIM1), target :: dt
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

       if ( (dt(l,k)%vdt1(j,i)%var_a(i,j) .neqv. .false.) ) error stop 36
       if ( dt(l,k)%vdt1(j,i)%var_b(i,j) /= i+j+(j-1)*DIM2 ) error stop 38
       if ( (dt(l,k)%vdt1(j,i)%var_c(i,j) /= 'A') ) error stop 40
       if ( dt(l,k)%vdt1(j,i)%var_d(i,j) /= i+j+(j-1)*DIM2+2 ) error stop 42
       if ( dt(l,k)%vdt1(j,i)%var_e(i,j) /= i+j+(j-1)*DIM2+3 ) error stop 44
       if ( dt(l,k)%vdt1(j,i)%var_f(i,j) /= i+j+(j-1)*DIM2+4 ) error stop 46
       if ( (dt(l,k)%vdt1(j,i)%var_g(i,j) .neqv. .false.) ) error stop 48
       if ( dt(l,k)%vdt1(j,i)%var_h(i,j) /= i+j+(j-1)*DIM2+6 ) error stop 50
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

       dt(l,k)%vdt1(j,i)%var_a(i,j) = .not. dt(l,k)%vdt1(j,i)%var_a(i,j)
       dt(l,k)%vdt1(j,i)%var_b(i,j) = i+j+(j-1)*DIM2+1
       dt(l,k)%vdt1(j,i)%var_c(i,j) = 'B'
       dt(l,k)%vdt1(j,i)%var_d(i,j) = i+j+(j-1)*DIM2+3
       dt(l,k)%vdt1(j,i)%var_e(i,j) = i+j+(j-1)*DIM2+4
       dt(l,k)%vdt1(j,i)%var_f(i,j) = i+j+(j-1)*DIM2+5
       dt(l,k)%vdt1(j,i)%var_g(i,j) = .not. dt(l,k)%vdt1(j,i)%var_g(i,j)
       dt(l,k)%vdt1(j,i)%var_h(i,j) = i+j+(j-1)*DIM2+7
      end do
     end do
    end do
   end do

  fun1 = C_LOC(dt)
end function fun1

!! Test 2 Fun
function fun2(dtx,dty) bind(c)
   use mxdtyla33
   type(dt2), dimension(DIM2,DIM1), intent(in) :: dtx
   type(dt2), dimension(DIM2,DIM1), intent(out), target :: dty
   type(C_PTR) :: fun2

   do k = 1, DIM1
    do l = 1, DIM2
     do j = 1, DIM2
      do i = 1, DIM1
       if ( dtx(l,k)%var_a(i,j) /= real(2*(i+j+(j-1)*DIM2-1)) ) error stop 52
       if ( (dtx(l,k)%var_b(i,j) .neqv. .false.) ) error stop 54
       if ( dtx(l,k)%var_c(i,j) /= dble(2*(i+j+(j-1)*DIM2+1)) ) error stop 56
       if ( dtx(l,k)%var_d(i,j) /= dble(2*(i+j+(j-1)*DIM2+2)) ) error stop 58
       if ( dtx(l,k)%var_e(i,j) /= real(2*(i+j+(j-1)*DIM2+3)) ) error stop 60
       if ( (dtx(l,k)%var_f(i,j) .neqv. .false.) ) error stop 62
       if ( dtx(l,k)%var_g(i,j) /= dble(2*(i+j+(j-1)*DIM2+5)) ) error stop 64
       if ( (dtx(l,k)%var_h(i,j) /= 'A') ) error stop 66

       if ( (dtx(l,k)%vdt1(j,i)%var_a(i,j) .neqv. .false.) ) error stop 68
       if ( dtx(l,k)%vdt1(j,i)%var_b(i,j) /= 2*(i+j+(j-1)*DIM2) ) error stop 70
       if ( (dtx(l,k)%vdt1(j,i)%var_c(i,j) /= 'A') ) error stop 72
       if ( dtx(l,k)%vdt1(j,i)%var_d(i,j) /= 2*(i+j+(j-1)*DIM2+2) ) error stop 74
       if ( dtx(l,k)%vdt1(j,i)%var_e(i,j) /= 2*(i+j+(j-1)*DIM2+3) ) error stop 76
       if ( dtx(l,k)%vdt1(j,i)%var_f(i,j) /= 2*(i+j+(j-1)*DIM2+4) ) error stop 78
       if ( (dtx(l,k)%vdt1(j,i)%var_g(i,j) .neqv. .false.) ) error stop 80
       if ( dtx(l,k)%vdt1(j,i)%var_h(i,j) /= 2*(i+j+(j-1)*DIM2+6) ) error stop 82
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

       dty(l,k)%vdt1(j,i)%var_a(i,j) = .not. dtx(l,k)%vdt1(j,i)%var_a(i,j)
       dty(l,k)%vdt1(j,i)%var_b(i,j) = dtx(l,k)%vdt1(j,i)%var_b(i,j) + 2
       dty(l,k)%vdt1(j,i)%var_c(i,j) = 'B'
       dty(l,k)%vdt1(j,i)%var_d(i,j) = dtx(l,k)%vdt1(j,i)%var_d(i,j) + 4
       dty(l,k)%vdt1(j,i)%var_e(i,j) = dtx(l,k)%vdt1(j,i)%var_e(i,j) + 5
       dty(l,k)%vdt1(j,i)%var_f(i,j) = dtx(l,k)%vdt1(j,i)%var_f(i,j) + 6
       dty(l,k)%vdt1(j,i)%var_g(i,j) = .not. dtx(l,k)%vdt1(j,i)%var_g(i,j)
       dty(l,k)%vdt1(j,i)%var_h(i,j) = dtx(l,k)%vdt1(j,i)%var_h(i,j) + 8
      end do
     end do
    end do
   end do

   fun2 = C_LOC(dty)
end function fun2