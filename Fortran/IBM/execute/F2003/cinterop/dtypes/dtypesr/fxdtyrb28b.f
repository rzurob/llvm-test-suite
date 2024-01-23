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
!*      - Testing arrays of 2-levels deep derived types with integer and real components
!*      - Testing FORTRAN subroutines and C void functions
!*      - Main written in FORTRAN
!*
!234567890123456789012345678901234567890123456789012345678901234567890


module mxdtyrb28
   use ISO_C_BINDING

   integer, parameter :: DIM1 = 3, DIM2 = 2

   type, bind(c) :: dt1
      integer(C_INT_FAST16_T) :: var_a(DIM1,DIM2) = reshape((/1,2,3,4,5,6/),(/DIM1,DIM2/))
      real(C_DOUBLE) :: var_b(DIM1,DIM2) = reshape((/2.0d0,3.0d0,4.0d0,5.0d0,6.0d0,7.0d0/),(/DIM1,DIM2/))
      integer(C_SIGNED_CHAR) :: var_c(DIM1,DIM2) = reshape((/3,4,5,6,7,8/),(/DIM1,DIM2/))
      real(C_FLOAT) :: var_d(DIM1,DIM2) = reshape((/4.0e0,5.0e0,6.0e0,7.0e0,8.0e0,9.0e0/),(/DIM1,DIM2/))
      real(16) :: var_e(DIM1,DIM2) = reshape((/5.0q0,6.0q0,7.0q0,8.0q0,9.0q0,10.0q0/),(/DIM1,DIM2/))
      real(8) :: var_f(DIM1,DIM2) = reshape((/6.0d0,7.0d0,8.0d0,9.0d0,10.0d0,11.0d0/),(/DIM1,DIM2/))
      integer(C_INTMAX_T) :: var_g(DIM1,DIM2) = reshape((/7,8,9,10,11,12/),(/DIM1,DIM2/))
      real(4) :: var_h(DIM1,DIM2) = reshape((/8.0e0,9.0e0,10.0e0,11.0e0,12.0e0,13.0e0/),(/DIM1,DIM2/))
   end type

   type, bind(c) :: dt2
      real(C_FLOAT) :: var_a(DIM1,DIM2) = reshape((/1.0e0,2.0e0,3.0e0,4.0e0,5.0e0,6.0e0/),(/DIM1,DIM2/))
      integer(C_SHORT) :: var_b(DIM1,DIM2) = reshape((/2,3,4,5,6,7/),(/DIM1,DIM2/))
      real(16) :: var_c(DIM1,DIM2) = reshape((/3.0q0,4.0q0,5.0q0,6.0q0,7.0q0,8.0q0/),(/DIM1,DIM2/))
      real(8) :: var_d(DIM1,DIM2) = reshape((/4.0d0,5.0d0,6.0d0,7.0d0,8.0d0,9.0d0/),(/DIM1,DIM2/))
      real(4) :: var_e(DIM1,DIM2) = reshape((/5.0e0,6.0e0,7.0e0,8.0e0,9.0e0,10.0e0/),(/DIM1,DIM2/))
      integer(1) :: var_f(DIM1,DIM2) = reshape((/6,7,8,9,10,11/),(/DIM1,DIM2/))
      real(C_DOUBLE) :: var_g(DIM1,DIM2) = reshape((/7.0d0,8.0d0,9.0d0,10.0d0,11.0d0,12.0d0/),(/DIM1,DIM2/))
      integer(4) :: var_h(DIM1,DIM2) = reshape((/8,9,10,11,12,13/),(/DIM1,DIM2/))
      type(dt1) :: vdt1(DIM2,DIM1)
   end type

end module mxdtyrb28

program fxdtyrb28b
   use mxdtyrb28
   interface
      subroutine sub1(dt) bind(c)
         import dt2, DIM1, DIM2
         type(dt2), dimension(DIM2,DIM1) :: dt
      end subroutine sub1
   end interface

   type(dt2), dimension(DIM2,DIM1) :: dta

!! Test 1

   call sub1(dta)

   do k = 1, DIM1
    do l = 1, DIM2
     do j = 1, DIM2
      do i = 1, DIM1
       if ( dta(l,k)%var_a(i,j) /= real(i+j+(j-1)*DIM2) ) error stop 20
       if ( dta(l,k)%var_b(i,j) /= i+j+(j-1)*DIM2+1 ) error stop 22
       if ( dta(l,k)%var_c(i,j) /= dble(i+j+(j-1)*DIM2+2) ) error stop 24
       if ( dta(l,k)%var_d(i,j) /= dble(i+j+(j-1)*DIM2+3) ) error stop 26
       if ( dta(l,k)%var_e(i,j) /= real(i+j+(j-1)*DIM2+4) ) error stop 28
       if ( dta(l,k)%var_f(i,j) /= i+j+(j-1)*DIM2+5 ) error stop 30
       if ( dta(l,k)%var_g(i,j) /= dble(i+j+(j-1)*DIM2+6) ) error stop 32
       if ( dta(l,k)%var_h(i,j) /= i+j+(j-1)*DIM2+7 ) error stop 34

       if ( dta(l,k)%vdt1(j,i)%var_a(i,j) /= i+j+(j-1)*DIM2+1 ) error stop 36
       if ( dta(l,k)%vdt1(j,i)%var_b(i,j) /= dble(i+j+(j-1)*DIM2+2) ) error stop 38
       if ( dta(l,k)%vdt1(j,i)%var_c(i,j) /= i+j+(j-1)*DIM2+3 ) error stop 40
       if ( dta(l,k)%vdt1(j,i)%var_d(i,j) /= real(i+j+(j-1)*DIM2+4) ) error stop 42
       if ( dta(l,k)%vdt1(j,i)%var_e(i,j) /= dble(i+j+(j-1)*DIM2+5) ) error stop 44
       if ( dta(l,k)%vdt1(j,i)%var_f(i,j) /= dble(i+j+(j-1)*DIM2+6) ) error stop 46
       if ( dta(l,k)%vdt1(j,i)%var_g(i,j) /= i+j+(j-1)*DIM2+7 ) error stop 48
       if ( dta(l,k)%vdt1(j,i)%var_h(i,j) /= real(i+j+(j-1)*DIM2+8) ) error stop 50
      end do
     end do
    end do
   end do

end program fxdtyrb28b
