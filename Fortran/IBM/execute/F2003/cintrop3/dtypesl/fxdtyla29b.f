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
!*      - Main written in FORTRAN
!*
!234567890123456789012345678901234567890123456789012345678901234567890


module mxdtyla29
   use ISO_C_BINDING

   integer, parameter :: DIM1 = 3, DIM2 = 2

   type, bind(c) :: dt1
      logical(C_BOOL) :: var_a(DIM1,DIM2) = reshape((/.true.,.true.,.true.,.true.,.true.,.true./),(/DIM1,DIM2/))
      real(C_DOUBLE) :: var_b(DIM1,DIM2) = reshape((/2.0d0,3.0d0,4.0d0,5.0d0,6.0d0,7.0d0/),(/DIM1,DIM2/))
      character(C_CHAR) :: var_c(DIM1,DIM2) = reshape((/'A','A','A','A','A','A'/),(/DIM1,DIM2/))
      real(C_FLOAT) :: var_d(DIM1,DIM2) = reshape((/4.0e0,5.0e0,6.0e0,7.0e0,8.0e0,9.0e0/),(/DIM1,DIM2/))
      real(C_LONG_DOUBLE) :: var_e(DIM1,DIM2) = reshape((/5.0q0,6.0q0,7.0q0,8.0q0,9.0q0,10.0q0/),(/DIM1,DIM2/))
      real(8) :: var_f(DIM1,DIM2) = reshape((/6.0d0,7.0d0,8.0d0,9.0d0,10.0d0,11.0d0/),(/DIM1,DIM2/))
      logical(C_BOOL) :: var_g(DIM1,DIM2) = reshape((/.true.,.true.,.true.,.true.,.true.,.true./),(/DIM1,DIM2/))
      real(4) :: var_h(DIM1,DIM2) = reshape((/8.0e0,9.0e0,10.0e0,11.0e0,12.0e0,13.0e0/),(/DIM1,DIM2/))
   end type

   type, bind(c) :: dt2
      real(C_FLOAT) :: var_a(DIM1,DIM2) = reshape((/1.0e0,2.0e0,3.0e0,4.0e0,5.0e0,6.0e0/),(/DIM1,DIM2/))
      logical(C_BOOL) :: var_b(DIM1,DIM2) = reshape((/.true.,.true.,.true.,.true.,.true.,.true./),(/DIM1,DIM2/))
      real(C_LONG_DOUBLE) :: var_c(DIM1,DIM2) = reshape((/3.0q0,4.0q0,5.0q0,6.0q0,7.0q0,8.0q0/),(/DIM1,DIM2/))
      real(8) :: var_d(DIM1,DIM2) = reshape((/4.0d0,5.0d0,6.0d0,7.0d0,8.0d0,9.0d0/),(/DIM1,DIM2/))
      real(4) :: var_e(DIM1,DIM2) = reshape((/5.0e0,6.0e0,7.0e0,8.0e0,9.0e0,10.0e0/),(/DIM1,DIM2/))
      logical(C_BOOL) :: var_f(DIM1,DIM2) = reshape((/.true.,.true.,.true.,.true.,.true.,.true./),(/DIM1,DIM2/))
      real(C_DOUBLE) :: var_g(DIM1,DIM2) = reshape((/7.0d0,8.0d0,9.0d0,10.0d0,11.0d0,12.0d0/),(/DIM1,DIM2/))
      character(C_CHAR) :: var_h(DIM1,DIM2) = reshape((/'A','A','A','A','A','A'/),(/DIM1,DIM2/))
      type(dt1) :: vdt1(DIM2,DIM1)
   end type

end module mxdtyla29

program fxdtyla29b
   use mxdtyla29
   interface
      function fun1(dt) bind(c)
         import dt2, C_PTR, DIM1, DIM2
         type(dt2), dimension(DIM2,DIM1) :: dt
         type(C_PTR) :: fun1
      end function fun1
   end interface

   type(dt2), dimension(DIM2,DIM1) :: dta
   type(dt2), pointer :: dtr(:,:)
   type(C_PTR) :: dtp

!! Test 1

   dtp = fun1(dta)

   call C_F_POINTER(dtp,dtr,(/DIM2,DIM1/))

   do k = 1, DIM1
    do l = 1, DIM2
     do j = 1, DIM2
      do i = 1, DIM1
       if ( dta(l,k)%var_a(i,j) /= real(i+j+(j-1)*DIM2) ) error stop 20
       if ( (dta(l,k)%var_b(i,j) .neqv. .false.) ) error stop 22
       if ( dta(l,k)%var_c(i,j) /= dble(i+j+(j-1)*DIM2+2) ) error stop 24
       if ( dta(l,k)%var_d(i,j) /= dble(i+j+(j-1)*DIM2+3) ) error stop 26
       if ( dta(l,k)%var_e(i,j) /= real(i+j+(j-1)*DIM2+4) ) error stop 28
       if ( (dta(l,k)%var_f(i,j) .neqv. .false.) ) error stop 30
       if ( dta(l,k)%var_g(i,j) /= dble(i+j+(j-1)*DIM2+6) ) error stop 32
       if ( (dta(l,k)%var_h(i,j) /= 'B') ) error stop 34

       if ( dtr(l,k)%var_a(i,j) /= real(i+j+(j-1)*DIM2) ) error stop 36
       if ( (dtr(l,k)%var_b(i,j) .neqv. .false.) ) error stop 38
       if ( dtr(l,k)%var_c(i,j) /= dble(i+j+(j-1)*DIM2+2) ) error stop 40
       if ( dtr(l,k)%var_d(i,j) /= dble(i+j+(j-1)*DIM2+3) ) error stop 42
       if ( dtr(l,k)%var_e(i,j) /= real(i+j+(j-1)*DIM2+4) ) error stop 44
       if ( (dtr(l,k)%var_f(i,j) .neqv. .false.) ) error stop 46
       if ( dtr(l,k)%var_g(i,j) /= dble(i+j+(j-1)*DIM2+6) ) error stop 48
       if ( (dtr(l,k)%var_h(i,j) /= 'B') ) error stop 50

       if ( (dta(l,k)%vdt1(j,i)%var_a(i,j) .neqv. .false.) ) error stop 52
       if ( dta(l,k)%vdt1(j,i)%var_b(i,j) /= i+j+(j-1)*DIM2+2 ) error stop 54
       if ( (dta(l,k)%vdt1(j,i)%var_c(i,j) /= 'B') ) error stop 56
       if ( dta(l,k)%vdt1(j,i)%var_d(i,j) /= i+j+(j-1)*DIM2+4 ) error stop 58
       if ( dta(l,k)%vdt1(j,i)%var_e(i,j) /= i+j+(j-1)*DIM2+5 ) error stop 60
       if ( dta(l,k)%vdt1(j,i)%var_f(i,j) /= i+j+(j-1)*DIM2+6 ) error stop 62
       if ( (dta(l,k)%vdt1(j,i)%var_g(i,j) .neqv. .false.) ) error stop 64
       if ( dta(l,k)%vdt1(j,i)%var_h(i,j) /= i+j+(j-1)*DIM2+8 ) error stop 66

       if ( (dtr(l,k)%vdt1(j,i)%var_a(i,j) .neqv. .false.) ) error stop 68
       if ( dtr(l,k)%vdt1(j,i)%var_b(i,j) /= i+j+(j-1)*DIM2+2 ) error stop 70
       if ( (dtr(l,k)%vdt1(j,i)%var_c(i,j) /= 'B') ) error stop 72
       if ( dtr(l,k)%vdt1(j,i)%var_d(i,j) /= i+j+(j-1)*DIM2+4 ) error stop 74
       if ( dtr(l,k)%vdt1(j,i)%var_e(i,j) /= i+j+(j-1)*DIM2+5 ) error stop 76
       if ( dtr(l,k)%vdt1(j,i)%var_f(i,j) /= i+j+(j-1)*DIM2+6 ) error stop 78
       if ( (dtr(l,k)%vdt1(j,i)%var_g(i,j) .neqv. .false.) ) error stop 80
       if ( dtr(l,k)%vdt1(j,i)%var_h(i,j) /= i+j+(j-1)*DIM2+8 ) error stop 82
      end do
     end do
    end do
   end do

end program fxdtyla29b