!#######################################################################
!***********************************************************************
! %START
! %MAIN:
! %PRECMD: ${TR_SRC}/scrdtya00.presh fxdtyia28 cxdtyia28
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

end module mxdtyia28

module auxmod
   use mxdtyia28

   interface operator(+)
      module procedure adddty_s, adddty_d
   end interface

contains

   elemental function adddty_s(dtx,dty)
      type(dt1), intent(in) :: dtx, dty
      type(dt1) :: adddty_s

      do i = 1, DIM1
       do j = 1, DIM2
         adddty_s%var_a(i,j) = dtx%var_a(i,j) + dty%var_a(i,j)
         adddty_s%var_b(i,j) = dtx%var_b(i,j) + dty%var_b(i,j)
         adddty_s%var_c(i,j) = dtx%var_c(i,j) + dty%var_c(i,j)
         adddty_s%var_d(i,j) = dtx%var_d(i,j) + dty%var_d(i,j)
         adddty_s%var_e(i,j) = dtx%var_e(i,j) + dty%var_e(i,j)
         adddty_s%var_f(i,j) = dtx%var_f(i,j) + dty%var_f(i,j)
         adddty_s%var_g(i,j) = dtx%var_g(i,j) + dty%var_g(i,j)
         adddty_s%var_h(i,j) = dtx%var_h(i,j) + dty%var_h(i,j)
       end do
      end do

   end function adddty_s

   elemental function adddty_d(dtx,dty)
      type(dt2), intent(in) :: dtx, dty
      type(dt2) :: adddty_d

      do i = 1, DIM1
       do j = 1, DIM2
         adddty_d%var_a(i,j) = dtx%var_a(i,j) + dty%var_a(i,j)
         adddty_d%var_b(i,j) = dtx%var_b(i,j) + dty%var_b(i,j)
         adddty_d%var_c(i,j) = dtx%var_c(i,j) + dty%var_c(i,j)
         adddty_d%var_d(i,j) = dtx%var_d(i,j) + dty%var_d(i,j)
         adddty_d%var_e(i,j) = dtx%var_e(i,j) + dty%var_e(i,j)
         adddty_d%var_f(i,j) = dtx%var_f(i,j) + dty%var_f(i,j)
         adddty_d%var_g(i,j) = dtx%var_g(i,j) + dty%var_g(i,j)
         adddty_d%var_h(i,j) = dtx%var_h(i,j) + dty%var_h(i,j)
       end do
      end do
      adddty_d%vdt1 = adddty_s(dtx%vdt1,dty%vdt1)

   end function adddty_d

end module auxmod

program fxdtyia28
   use mxdtyia28
   use auxmod
   interface
      subroutine sub1(dt) bind(c)
         import dt2, DIM1, DIM2
         type(dt2), dimension(DIM2,DIM1) :: dt
      end subroutine sub1
      subroutine sub2(dtx,dty) bind(c)
         import dt2, DIM1, DIM2
         type(dt2), dimension(DIM2,DIM1), intent(in) :: dtx
         type(dt2), dimension(DIM2,DIM1), intent(out) :: dty
      end subroutine sub2
   end interface

   type(dt1), parameter :: dt01 = dt1( &
              reshape((/1,2,3,4,5,6/),(/DIM1,DIM2/)),&
              reshape((/2,3,4,5,6,7/),(/DIM1,DIM2/)),&
              reshape((/3,4,5,6,7,8/),(/DIM1,DIM2/)),&
              reshape((/4,5,6,7,8,9/),(/DIM1,DIM2/)),&
              reshape((/5,6,7,8,9,10/),(/DIM1,DIM2/)),&
              reshape((/6,7,8,9,10,11/),(/DIM1,DIM2/)),&
              reshape((/7,8,9,10,11,12/),(/DIM1,DIM2/)),&
              reshape((/8,9,10,11,12,13/),(/DIM1,DIM2/)))

   type(dt2), parameter :: dt0 = dt2( &
              reshape((/1,2,3,4,5,6/),(/DIM1,DIM2/)),&
              reshape((/2,3,4,5,6,7/),(/DIM1,DIM2/)),&
              reshape((/3,4,5,6,7,8/),(/DIM1,DIM2/)),&
              reshape((/4,5,6,7,8,9/),(/DIM1,DIM2/)),&
              reshape((/5,6,7,8,9,10/),(/DIM1,DIM2/)),&
              reshape((/6,7,8,9,10,11/),(/DIM1,DIM2/)),&
              reshape((/7,8,9,10,11,12/),(/DIM1,DIM2/)),&
              reshape((/8,9,10,11,12,13/),(/DIM1,DIM2/)),&
              reshape((/dt01,dt01,dt01,dt01,dt01,dt01/),(/DIM2,DIM1/)))

   type(dt2), dimension(DIM2,DIM1) :: dta, dtb

!! Test 1

   dta = reshape((/dt0,dt0,dt0,dt0,dt0,dt0/),(/DIM2,DIM1/))

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

!! Test 2

   dta = reshape((/dt0,dt0,dt0,dt0,dt0,dt0/),(/DIM2,DIM1/))

   call sub2(dta+dta,dtb)

   do k = 1, DIM1
    do l = 1, DIM2
     do j = 1, DIM2
      do i = 1, DIM1
       if ( dtb(l,k)%var_a(i,j) /= 2*(i+j+(j-1)*DIM2)-1 ) error stop 52
       if ( dtb(l,k)%var_b(i,j) /= 2*(i+j+(j-1)*DIM2+1)-1 ) error stop 54
       if ( dtb(l,k)%var_c(i,j) /= 2*(i+j+(j-1)*DIM2+2)-1 ) error stop 56
       if ( dtb(l,k)%var_d(i,j) /= 2*(i+j+(j-1)*DIM2+3)-1 ) error stop 58
       if ( dtb(l,k)%var_e(i,j) /= 2*(i+j+(j-1)*DIM2+4)-1 ) error stop 60
       if ( dtb(l,k)%var_f(i,j) /= 2*(i+j+(j-1)*DIM2+5)-1 ) error stop 62
       if ( dtb(l,k)%var_g(i,j) /= 2*(i+j+(j-1)*DIM2+6)-1 ) error stop 64
       if ( dtb(l,k)%var_h(i,j) /= 2*(i+j+(j-1)*DIM2+7)-1 ) error stop 66

       if ( dtb(l,k)%vdt1(j,i)%var_a(i,j) /= 2*(i+j+(j-1)*DIM2) ) error stop 68
       if ( dtb(l,k)%vdt1(j,i)%var_b(i,j) /= 2*(i+j+(j-1)*DIM2+1) ) error stop 70
       if ( dtb(l,k)%vdt1(j,i)%var_c(i,j) /= 2*(i+j+(j-1)*DIM2+2) ) error stop 72
       if ( dtb(l,k)%vdt1(j,i)%var_d(i,j) /= 2*(i+j+(j-1)*DIM2+3) ) error stop 74
       if ( dtb(l,k)%vdt1(j,i)%var_e(i,j) /= 2*(i+j+(j-1)*DIM2+4) ) error stop 76
       if ( dtb(l,k)%vdt1(j,i)%var_f(i,j) /= 2*(i+j+(j-1)*DIM2+5) ) error stop 78
       if ( dtb(l,k)%vdt1(j,i)%var_g(i,j) /= 2*(i+j+(j-1)*DIM2+6) ) error stop 80
       if ( dtb(l,k)%vdt1(j,i)%var_h(i,j) /= 2*(i+j+(j-1)*DIM2+7) ) error stop 82
      end do
     end do
    end do
   end do

end program fxdtyia28
