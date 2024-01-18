!#######################################################################
!***********************************************************************
! %START
! %MAIN:
! %PRECMD: ${TR_SRC}/scrdtya00.presh fxdtyia28b cxdtyia28b
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
!*      - Testing arrays of 2-levels deep derived types with integer components
!*      - Testing FORTRAN subroutines and C void functions
!*      - Main written in FORTRAN
!*
!234567890123456789012345678901234567890123456789012345678901234567890


module mxdtyia28
   use ISO_C_BINDING

   integer, parameter :: DIM1 = 3, DIM2 = 2

   type, bind(c) :: dt1
      integer(1) :: var_a(DIM1,DIM2) = reshape((/1,2,3,4,5,6/),(/DIM1,DIM2/))
      integer(C_INT) :: var_b(DIM1,DIM2) = reshape((/2,3,4,5,6,7/),(/DIM1,DIM2/))
      integer(C_SHORT) :: var_c(DIM1,DIM2) = reshape((/3,4,5,6,7,8/),(/DIM1,DIM2/))
      integer(8) :: var_d(DIM1,DIM2) = reshape((/4,5,6,7,8,9/),(/DIM1,DIM2/))
      integer(C_LONG) :: var_e(DIM1,DIM2) = reshape((/5,6,7,8,9,10/),(/DIM1,DIM2/))
      integer(2) :: var_f(DIM1,DIM2) = reshape((/6,7,8,9,10,11/),(/DIM1,DIM2/))
      integer(C_LONG_LONG) :: var_g(DIM1,DIM2) = reshape((/7,8,9,10,11,12/),(/DIM1,DIM2/))
      integer(4) :: var_h(DIM1,DIM2) = reshape((/8,9,10,11,12,13/),(/DIM1,DIM2/))
   end type

   type, bind(c) :: dt2
      integer(C_INT16_T) :: var_a(DIM1,DIM2) = reshape((/1,2,3,4,5,6/),(/DIM1,DIM2/))
      integer(C_INT_FAST8_T) :: var_b(DIM1,DIM2) = reshape((/2,3,4,5,6,7/),(/DIM1,DIM2/))
      integer(C_INT64_T) :: var_c(DIM1,DIM2) = reshape((/3,4,5,6,7,8/),(/DIM1,DIM2/))
      integer(C_INT8_T) :: var_d(DIM1,DIM2) = reshape((/4,5,6,7,8,9/),(/DIM1,DIM2/))
      integer(C_INT_FAST32_T) :: var_e(DIM1,DIM2) = reshape((/5,6,7,8,9,10/),(/DIM1,DIM2/))
      integer(C_INT_FAST16_T) :: var_f(DIM1,DIM2) = reshape((/6,7,8,9,10,11/),(/DIM1,DIM2/))
      integer(C_INT32_T) :: var_g(DIM1,DIM2) = reshape((/7,8,9,10,11,12/),(/DIM1,DIM2/))
      integer(C_INT_FAST64_T) :: var_h(DIM1,DIM2) = reshape((/8,9,10,11,12,13/),(/DIM1,DIM2/))
      type(dt1) :: vdt1(DIM2,DIM1)
   end type

end module mxdtyia28

program fxdtyia28b
   use mxdtyia28
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
       if ( dta(l,k)%var_a(i,j) /= i+j+(j-1)*DIM2 ) error stop 20
       if ( dta(l,k)%var_b(i,j) /= i+j+(j-1)*DIM2+1 ) error stop 22
       if ( dta(l,k)%var_c(i,j) /= i+j+(j-1)*DIM2+2 ) error stop 24
       if ( dta(l,k)%var_d(i,j) /= i+j+(j-1)*DIM2+3 ) error stop 26
       if ( dta(l,k)%var_e(i,j) /= i+j+(j-1)*DIM2+4 ) error stop 28
       if ( dta(l,k)%var_f(i,j) /= i+j+(j-1)*DIM2+5 ) error stop 30
       if ( dta(l,k)%var_g(i,j) /= i+j+(j-1)*DIM2+6 ) error stop 32
       if ( dta(l,k)%var_h(i,j) /= i+j+(j-1)*DIM2+7 ) error stop 34

       if ( dta(l,k)%vdt1(j,i)%var_a(i,j) /= i+j+(j-1)*DIM2+1 ) error stop 36
       if ( dta(l,k)%vdt1(j,i)%var_b(i,j) /= i+j+(j-1)*DIM2+2 ) error stop 38
       if ( dta(l,k)%vdt1(j,i)%var_c(i,j) /= i+j+(j-1)*DIM2+3 ) error stop 40
       if ( dta(l,k)%vdt1(j,i)%var_d(i,j) /= i+j+(j-1)*DIM2+4 ) error stop 42
       if ( dta(l,k)%vdt1(j,i)%var_e(i,j) /= i+j+(j-1)*DIM2+5 ) error stop 44
       if ( dta(l,k)%vdt1(j,i)%var_f(i,j) /= i+j+(j-1)*DIM2+6 ) error stop 46
       if ( dta(l,k)%vdt1(j,i)%var_g(i,j) /= i+j+(j-1)*DIM2+7 ) error stop 48
       if ( dta(l,k)%vdt1(j,i)%var_h(i,j) /= i+j+(j-1)*DIM2+8 ) error stop 50
      end do
     end do
    end do
   end do

end program fxdtyia28b
