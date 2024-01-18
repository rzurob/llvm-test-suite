!#######################################################################
!***********************************************************************
! %START
! %MAIN:
! %PRECMD: ${TR_SRC}/scrdtyf00.presh fxdtylb13 cxdtyla13
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
!*      - Testing arrays of single derived types with BIND(C) attribute
!*      - Testing arrays of single derived types with INTENT attributes
!*      - Testing arrays of single derived types with logical, character and real components
!*      - Testing FORTRAN functions and C functions
!*      - Main written in FORTRAN
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module mxdtylb13
   use ISO_C_BINDING

   integer, parameter :: DIM1 = 3, DIM2 = 2

   type, bind(c) :: dt1
      real(C_FLOAT) var_a(DIM1,DIM2)
      logical(C_BOOL) var_b(DIM1,DIM2)
      real(16) var_c(DIM1,DIM2)
      real(8) var_d(DIM1,DIM2)
      real(4) var_e(DIM1,DIM2)
      logical(C_BOOL) var_f(DIM1,DIM2)
      real(C_DOUBLE) var_g(DIM1,DIM2)
      character(C_CHAR) var_h(DIM1,DIM2)
   end type

end module mxdtylb13

module auxmod
   use mxdtylb13

   interface operator(+)
      module procedure adddty
   end interface

!----------------------------------------------------------
contains
   function adddty(dtx,dty)
      type(dt1), dimension(DIM2,DIM1), intent(in) :: dtx, dty
      type(dt1), dimension(DIM2,DIM1) :: adddty

      do k = 1, DIM1
       do l = 1, DIM2
        do j = 1, DIM2
         do i = 1, DIM1
           adddty(l,k)%var_a(i,j) = dtx(l,k)%var_a(i,j) + dty(l,k)%var_a(i,j)
           adddty(l,k)%var_b(i,j) = dtx(l,k)%var_b(i,j) .and. dty(l,k)%var_b(i,j)
           adddty(l,k)%var_c(i,j) = dtx(l,k)%var_c(i,j) + dty(l,k)%var_c(i,j)
           adddty(l,k)%var_d(i,j) = dtx(l,k)%var_d(i,j) + dty(l,k)%var_d(i,j)
           adddty(l,k)%var_e(i,j) = dtx(l,k)%var_e(i,j) + dty(l,k)%var_e(i,j)
           adddty(l,k)%var_f(i,j) = dtx(l,k)%var_f(i,j) .and. dty(l,k)%var_f(i,j)
           adddty(l,k)%var_g(i,j) = dtx(l,k)%var_g(i,j) + dty(l,k)%var_g(i,j)
           adddty(l,k)%var_h(i,j) = dtx(l,k)%var_h(i,j)
         end do
        end do
       end do
      end do

   end function adddty
end module auxmod

program fxdtylb13
   use mxdtylb13
   use auxmod
   interface
      function fun1(dt) bind(c)
         import dt1, C_PTR, DIM1, DIM2
         type(dt1), dimension(DIM2,DIM1) :: dt
         type(C_PTR) :: fun1
      end function fun1
      function fun2(dtx,dty) bind(c)
         import dt1, C_PTR, DIM1, DIM2
         type(dt1), dimension(DIM2,DIM1), intent(in) :: dtx
         type(dt1), dimension(DIM2,DIM1), intent(out) :: dty
         type(C_PTR) :: fun2
      end function fun2
   end interface

   type(dt1), parameter :: dt0 = dt1( &
              reshape((/1.0e0,2.0e0,3.0e0,4.0e0,5.0e0,6.0e0/),(/DIM1,DIM2/)),&
              reshape((/.true.,.true.,.true.,.true.,.true.,.true./),(/DIM1,DIM2/)),&
              reshape((/3.0q0,4.0q0,5.0q0,6.0q0,7.0q0,8.0q0/),(/DIM1,DIM2/)),&
              reshape((/4.0d0,5.0d0,6.0d0,7.0d0,8.0d0,9.0d0/),(/DIM1,DIM2/)),&
              reshape((/5.0e0,6.0e0,7.0e0,8.0e0,9.0e0,10.0e0/),(/DIM1,DIM2/)),&
              reshape((/.true.,.true.,.true.,.true.,.true.,.true./),(/DIM1,DIM2/)),&
              reshape((/7.0d0,8.0d0,9.0d0,10.0d0,11.0d0,12.0d0/),(/DIM1,DIM2/)),&
              reshape((/'A','A','A','A','A','A'/),(/DIM1,DIM2/)))

   type(dt1), dimension(DIM2,DIM1) :: dta, dtb
   type(dt1), pointer :: dtc(:,:)
   type(C_PTR) :: dtp

!----------------------------------------------------------
!! Test 1

   dta = reshape((/dt0,dt0,dt0,dt0,dt0,dt0/),(/DIM2,DIM1/))

   dtp = fun1(dta)

   call C_F_POINTER(dtp,dtc,(/DIM2,DIM1/))

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

       if ( dtc(l,k)%var_a(i,j) /= real(i+j+(j-1)*DIM2) ) error stop 36
       if ( (dtc(l,k)%var_b(i,j) .neqv. .false.) ) error stop 38
       if ( dtc(l,k)%var_c(i,j) /= dble(i+j+(j-1)*DIM2+2) ) error stop 40
       if ( dtc(l,k)%var_d(i,j) /= dble(i+j+(j-1)*DIM2+3) ) error stop 42
       if ( dtc(l,k)%var_e(i,j) /= real(i+j+(j-1)*DIM2+4) ) error stop 44
       if ( (dtc(l,k)%var_f(i,j) .neqv. .false.) ) error stop 46
       if ( dtc(l,k)%var_g(i,j) /= dble(i+j+(j-1)*DIM2+6) ) error stop 48
       if ( (dtc(l,k)%var_h(i,j) /= 'B') ) error stop 50
      end do
     end do
    end do
   end do

!----------------------------------------------------------
!! Test 2

   dta = reshape((/dt0,dt0,dt0,dt0,dt0,dt0/),(/DIM2,DIM1/))

   dtp = fun2(dta+dta,dtb)

   call C_F_POINTER(dtp,dtc,(/DIM2,DIM1/))

   do k = 1, DIM1
    do l = 1, DIM2
     do j = 1, DIM2
      do i = 1, DIM1
       if ( dtb(l,k)%var_a(i,j) /= real(2*(i+j+(j-1)*DIM2)-1) ) error stop 52
       if ( (dtb(l,k)%var_b(i,j) .neqv. .false.) ) error stop 54
       if ( dtb(l,k)%var_c(i,j) /= dble(2*(i+j+(j-1)*DIM2+2)-1) ) error stop 56
       if ( dtb(l,k)%var_d(i,j) /= dble(2*(i+j+(j-1)*DIM2+3)-1) ) error stop 58
       if ( dtb(l,k)%var_e(i,j) /= real(2*(i+j+(j-1)*DIM2+4)-1) ) error stop 60
       if ( (dtb(l,k)%var_f(i,j) .neqv. .false.) ) error stop 62
       if ( dtb(l,k)%var_g(i,j) /= dble(2*(i+j+(j-1)*DIM2+6)-1) ) error stop 64
       if ( (dtb(l,k)%var_h(i,j) /= 'B') ) error stop 66

       if ( dtc(l,k)%var_a(i,j) /= real(2*(i+j+(j-1)*DIM2)-1) ) error stop 68
       if ( (dtc(l,k)%var_b(i,j) .neqv. .false.) ) error stop 70
       if ( dtc(l,k)%var_c(i,j) /= dble(2*(i+j+(j-1)*DIM2+2)-1) ) error stop 72
       if ( dtc(l,k)%var_d(i,j) /= dble(2*(i+j+(j-1)*DIM2+3)-1) ) error stop 74
       if ( dtc(l,k)%var_e(i,j) /= real(2*(i+j+(j-1)*DIM2+4)-1) ) error stop 76
       if ( (dtc(l,k)%var_f(i,j) .neqv. .false.) ) error stop 78
       if ( dtc(l,k)%var_g(i,j) /= dble(2*(i+j+(j-1)*DIM2+6)-1) ) error stop 80
       if ( (dtc(l,k)%var_h(i,j) /= 'B') ) error stop 82
      end do
     end do
    end do
   end do

!----------------------------------------------------------
end program fxdtylb13
