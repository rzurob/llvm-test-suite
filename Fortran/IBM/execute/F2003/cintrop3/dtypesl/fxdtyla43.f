!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
!***********************************************************************
! %START
! %MAIN:
! %PRECMD: ${TR_SRC}/scrdtya01.presh fxdtyla43
! %COMPOPTS:
! %GROUP: redherring.f
! %VERIFY: 
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!***********************************************************************
!***********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : Support for derived types with BIND(C) attr.
!*
!*  PROGRAMMER                 : Alberto Alvarez-Mesquide
!*  DATE                       : 4/23/2002
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived types with BIND(C) attribute
!*  SECONDARY FUNCTIONS TESTED : see below 
!*
!*  DRIVER STANZA              : 
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!*      - Testing arrays of 3-levels deep derived types with BIND(C) attribute
!*      - Testing arrays of 3-levels deep derived types with INTENT attributes
!*      - Testing arrays of 3-levels deep derived types with logical, character and real components
!*      - Testing FORTRAN internal subroutines and host association
!*      - Main written in FORTRAN
!*
!234567890123456789012345678901234567890123456789012345678901234567890


module mxdtyla43
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
      real(C_DOUBLE) var_a(DIM1,DIM2)
      logical(C_BOOL) var_b(DIM1,DIM2)
      real(8) var_c(DIM1,DIM2)
      character(C_CHAR) var_d(DIM1,DIM2)
      real(C_LONG_DOUBLE) var_e(DIM1,DIM2)
      real(4) var_f(DIM1,DIM2)
      logical(C_BOOL) var_g(DIM1,DIM2)
      real(C_FLOAT) var_h(DIM1,DIM2)
      type(dt1) :: vdt1(DIM2,DIM1)
   end type

   type, bind(c) :: dt3
      type(dt2) :: vdt2(DIM2,DIM1)
      real(C_FLOAT) var_a(DIM1,DIM2)
      logical(C_BOOL) var_b(DIM1,DIM2)
      real(C_LONG_DOUBLE) var_c(DIM1,DIM2)
      real(8) var_d(DIM1,DIM2)
      real(4) var_e(DIM1,DIM2)
      logical(C_BOOL) var_f(DIM1,DIM2)
      real(C_DOUBLE) var_g(DIM1,DIM2)
      character(C_CHAR) var_h(DIM1,DIM2)
   end type

end module mxdtyla43

module auxmod
   use mxdtyla43

   interface operator(+)
      module procedure adddty_s, adddty_d, adddty_t
   end interface

contains

   elemental function adddty_s(dtx,dty)
      type(dt1), intent(in) :: dtx, dty
      type(dt1) :: adddty_s

      do i = 1, DIM1
       do j = 1, DIM2
         adddty_s%var_a(i,j) = dtx%var_a(i,j) .and. dty%var_a(i,j)
         adddty_s%var_b(i,j) = dtx%var_b(i,j) + dty%var_b(i,j)
         adddty_s%var_c(i,j) = dtx%var_c(i,j)
         adddty_s%var_d(i,j) = dtx%var_d(i,j) + dty%var_d(i,j)
         adddty_s%var_e(i,j) = dtx%var_e(i,j) + dty%var_e(i,j)
         adddty_s%var_f(i,j) = dtx%var_f(i,j) + dty%var_f(i,j)
         adddty_s%var_g(i,j) = dtx%var_g(i,j) .and. dty%var_g(i,j)
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
         adddty_d%var_b(i,j) = dtx%var_b(i,j) .and. dty%var_b(i,j)
         adddty_d%var_c(i,j) = dtx%var_c(i,j) + dty%var_c(i,j)
         adddty_d%var_d(i,j) = dtx%var_d(i,j)
         adddty_d%var_e(i,j) = dtx%var_e(i,j) + dty%var_e(i,j)
         adddty_d%var_f(i,j) = dtx%var_f(i,j) + dty%var_f(i,j)
         adddty_d%var_g(i,j) = dtx%var_g(i,j) .and. dty%var_g(i,j)
         adddty_d%var_h(i,j) = dtx%var_h(i,j) + dty%var_h(i,j)
       end do
      end do
      adddty_d%vdt1 = adddty_s(dtx%vdt1,dty%vdt1)

   end function adddty_d

   elemental function adddty_t(dtx,dty)
      type(dt3), intent(in) :: dtx, dty
      type(dt3) :: adddty_t

      do i = 1, DIM1
       do j = 1, DIM2
         adddty_t%var_a(i,j) = dtx%var_a(i,j) + dty%var_a(i,j)
         adddty_t%var_b(i,j) = dtx%var_b(i,j) .and. dty%var_b(i,j)
         adddty_t%var_c(i,j) = dtx%var_c(i,j) + dty%var_c(i,j)
         adddty_t%var_d(i,j) = dtx%var_d(i,j) + dty%var_d(i,j)
         adddty_t%var_e(i,j) = dtx%var_e(i,j) + dty%var_e(i,j)
         adddty_t%var_f(i,j) = dtx%var_f(i,j) .and. dty%var_f(i,j)
         adddty_t%var_g(i,j) = dtx%var_g(i,j) + dty%var_g(i,j)
         adddty_t%var_h(i,j) = dtx%var_h(i,j)
       end do
      end do
      adddty_t%vdt2 = adddty_d(dtx%vdt2,dty%vdt2)

   end function adddty_t

end module auxmod

program fxdtyla43
   use mxdtyla43
   use auxmod

   type(dt1), parameter :: dt01 = dt1( &
              reshape((/.true.,.true.,.true.,.true.,.true.,.true./),(/DIM1,DIM2/)),&
              reshape((/2.0d0,3.0d0,4.0d0,5.0d0,6.0d0,7.0d0/),(/DIM1,DIM2/)),&
              reshape((/'A','A','A','A','A','A'/),(/DIM1,DIM2/)),&
              reshape((/4.0e0,5.0e0,6.0e0,7.0e0,8.0e0,9.0e0/),(/DIM1,DIM2/)),&
              reshape((/5.0q0,6.0q0,7.0q0,8.0q0,9.0q0,10.0q0/),(/DIM1,DIM2/)),&
              reshape((/6.0d0,7.0d0,8.0d0,9.0d0,10.0d0,11.0d0/),(/DIM1,DIM2/)),&
              reshape((/.true.,.true.,.true.,.true.,.true.,.true./),(/DIM1,DIM2/)),&
              reshape((/8.0e0,9.0e0,10.0e0,11.0e0,12.0e0,13.0e0/),(/DIM1,DIM2/)))

   type(dt2), parameter :: dt02 = dt2( &
              reshape((/1.0d0,2.0d0,3.0d0,4.0d0,5.0d0,6.0d0/),(/DIM1,DIM2/)),&
              reshape((/.true.,.true.,.true.,.true.,.true.,.true./),(/DIM1,DIM2/)),&
              reshape((/3.0d0,4.0d0,5.0d0,6.0d0,7.0d0,8.0d0/),(/DIM1,DIM2/)),&
              reshape((/'A','A','A','A','A','A'/),(/DIM1,DIM2/)),&
              reshape((/5.0q0,6.0q0,7.0q0,8.0q0,9.0q0,10.0q0/),(/DIM1,DIM2/)),&
              reshape((/6.0e0,7.0e0,8.0e0,9.0e0,10.0e0,11.0e0/),(/DIM1,DIM2/)),&
              reshape((/.true.,.true.,.true.,.true.,.true.,.true./),(/DIM1,DIM2/)),&
              reshape((/8.0e0,9.0e0,10.0e0,11.0e0,12.0e0,13.0e0/),(/DIM1,DIM2/)),&
              reshape((/dt01,dt01,dt01,dt01,dt01,dt01/),(/DIM2,DIM1/)))

   type(dt3), parameter :: dt0 = dt3( &
              reshape((/dt02,dt02,dt02,dt02,dt02,dt02/),(/DIM2,DIM1/)),&
              reshape((/1.0e0,2.0e0,3.0e0,4.0e0,5.0e0,6.0e0/),(/DIM1,DIM2/)),&
              reshape((/.true.,.true.,.true.,.true.,.true.,.true./),(/DIM1,DIM2/)),&
              reshape((/3.0q0,4.0q0,5.0q0,6.0q0,7.0q0,8.0q0/),(/DIM1,DIM2/)),&
              reshape((/4.0d0,5.0d0,6.0d0,7.0d0,8.0d0,9.0d0/),(/DIM1,DIM2/)),&
              reshape((/5.0e0,6.0e0,7.0e0,8.0e0,9.0e0,10.0e0/),(/DIM1,DIM2/)),&
              reshape((/.true.,.true.,.true.,.true.,.true.,.true./),(/DIM1,DIM2/)),&
              reshape((/7.0d0,8.0d0,9.0d0,10.0d0,11.0d0,12.0d0/),(/DIM1,DIM2/)),&
              reshape((/'A','A','A','A','A','A'/),(/DIM1,DIM2/)))

   type(dt3), dimension(DIM2,DIM1) :: dta

!! Test 1

   dta = reshape((/dt0,dt0,dt0,dt0,dt0,dt0/),(/DIM2,DIM1/))

   call sub1

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

       if ( dta(l,k)%vdt2(j,i)%var_a(i,j) /= dble(i+j+(j-1)*DIM2) ) error stop 36
       if ( (dta(l,k)%vdt2(j,i)%var_b(i,j) .neqv. .false.) ) error stop 38
       if ( dta(l,k)%vdt2(j,i)%var_c(i,j) /= dble(i+j+(j-1)*DIM2+2) ) error stop 40
       if ( (dta(l,k)%vdt2(j,i)%var_d(i,j) /= 'B') ) error stop 42
       if ( dta(l,k)%vdt2(j,i)%var_e(i,j) /= dble(i+j+(j-1)*DIM2+4) ) error stop 44
       if ( dta(l,k)%vdt2(j,i)%var_f(i,j) /= real(i+j+(j-1)*DIM2+5) ) error stop 46
       if ( (dta(l,k)%vdt2(j,i)%var_g(i,j) .neqv. .false.) ) error stop 48
       if ( dta(l,k)%vdt2(j,i)%var_h(i,j) /= real(i+j+(j-1)*DIM2+7) ) error stop 50

       if ( (dta(l,k)%vdt2(j,i)%vdt1(j,i)%var_a(i,j) .neqv. .false.) ) error stop 52
       if ( dta(l,k)%vdt2(j,i)%vdt1(j,i)%var_b(i,j) /= dble(i+j+(j-1)*DIM2+1) ) error stop 54
       if ( (dta(l,k)%vdt2(j,i)%vdt1(j,i)%var_c(i,j) /= 'B') ) error stop 56
       if ( dta(l,k)%vdt2(j,i)%vdt1(j,i)%var_d(i,j) /= real(i+j+(j-1)*DIM2+3) ) error stop 58
       if ( dta(l,k)%vdt2(j,i)%vdt1(j,i)%var_e(i,j) /= dble(i+j+(j-1)*DIM2+4) ) error stop 60
       if ( dta(l,k)%vdt2(j,i)%vdt1(j,i)%var_f(i,j) /= dble(i+j+(j-1)*DIM2+5) ) error stop 62
       if ( (dta(l,k)%vdt2(j,i)%vdt1(j,i)%var_g(i,j) .neqv. .false.) ) error stop 64
       if ( dta(l,k)%vdt2(j,i)%vdt1(j,i)%var_h(i,j) /= real(i+j+(j-1)*DIM2+7) ) error stop 66
      end do
     end do
    end do
   end do

!! Test 3

   dta = reshape((/dt0,dt0,dt0,dt0,dt0,dt0/),(/DIM2,DIM1/))

   call sub3(dta+dta)

   do k = 1, DIM1
    do l = 1, DIM2
     do j = 1, DIM2
      do i = 1, DIM1
       if ( dta(l,k)%var_a(i,j) /= real(2*(i+j+(j-1)*DIM2-1)+1) ) error stop 68
       if ( (dta(l,k)%var_b(i,j) .neqv. .false.) ) error stop 70
       if ( dta(l,k)%var_c(i,j) /= dble(2*(i+j+(j-1)*DIM2+1)+3) ) error stop 72
       if ( dta(l,k)%var_d(i,j) /= dble(2*(i+j+(j-1)*DIM2+2)+4) ) error stop 74
       if ( dta(l,k)%var_e(i,j) /= real(2*(i+j+(j-1)*DIM2+3)+5) ) error stop 76
       if ( (dta(l,k)%var_f(i,j) .neqv. .false.) ) error stop 78
       if ( dta(l,k)%var_g(i,j) /= dble(2*(i+j+(j-1)*DIM2+5)+7) ) error stop 80
       if ( (dta(l,k)%var_h(i,j) /= 'B') ) error stop 82

       if ( dta(l,k)%vdt2(j,i)%var_a(i,j) /= dble(2*(i+j+(j-1)*DIM2-1)+1) ) error stop 84
       if ( (dta(l,k)%vdt2(j,i)%var_b(i,j) .neqv. .false.) ) error stop 86
       if ( dta(l,k)%vdt2(j,i)%var_c(i,j) /= dble(2*(i+j+(j-1)*DIM2+1)+3) ) error stop 88
       if ( (dta(l,k)%vdt2(j,i)%var_d(i,j) /= 'B') ) error stop 90
       if ( dta(l,k)%vdt2(j,i)%var_e(i,j) /= dble(2*(i+j+(j-1)*DIM2+3)+5) ) error stop 92
       if ( dta(l,k)%vdt2(j,i)%var_f(i,j) /= real(2*(i+j+(j-1)*DIM2+4)+6) ) error stop 94
       if ( (dta(l,k)%vdt2(j,i)%var_g(i,j) .neqv. .false.) ) error stop 96
       if ( dta(l,k)%vdt2(j,i)%var_h(i,j) /= real(2*(i+j+(j-1)*DIM2+6)+8) ) error stop 98

       if ( (dta(l,k)%vdt2(j,i)%vdt1(j,i)%var_a(i,j) .neqv. .false.) ) error stop 100
       if ( dta(l,k)%vdt2(j,i)%vdt1(j,i)%var_b(i,j) /= dble(2*(i+j+(j-1)*DIM2)+2) ) error stop 102
       if ( (dta(l,k)%vdt2(j,i)%vdt1(j,i)%var_c(i,j) /= 'B') ) error stop 104
       if ( dta(l,k)%vdt2(j,i)%vdt1(j,i)%var_d(i,j) /= real(2*(i+j+(j-1)*DIM2+2)+4) ) error stop 106
       if ( dta(l,k)%vdt2(j,i)%vdt1(j,i)%var_e(i,j) /= dble(2*(i+j+(j-1)*DIM2+3)+5) ) error stop 108
       if ( dta(l,k)%vdt2(j,i)%vdt1(j,i)%var_f(i,j) /= dble(2*(i+j+(j-1)*DIM2+4)+6) ) error stop 110
       if ( (dta(l,k)%vdt2(j,i)%vdt1(j,i)%var_g(i,j) .neqv. .false.) ) error stop 112
       if ( dta(l,k)%vdt2(j,i)%vdt1(j,i)%var_h(i,j) /= real(2*(i+j+(j-1)*DIM2+6)+8) ) error stop 114
      end do
     end do
    end do
   end do

contains

!! Test 1 Sub
subroutine sub1()

   do k = 1, DIM1
    do l = 1, DIM2
     do j = 1, DIM2
      do i = 1, DIM1
       if ( dta(l,k)%var_a(i,j) /= real(i+j+(j-1)*DIM2-1) ) error stop 116
       if ( (dta(l,k)%var_b(i,j) .neqv. .true.) ) error stop 118
       if ( dta(l,k)%var_c(i,j) /= dble(i+j+(j-1)*DIM2+1) ) error stop 120
       if ( dta(l,k)%var_d(i,j) /= dble(i+j+(j-1)*DIM2+2) ) error stop 122
       if ( dta(l,k)%var_e(i,j) /= real(i+j+(j-1)*DIM2+3) ) error stop 124
       if ( (dta(l,k)%var_f(i,j) .neqv. .true.) ) error stop 126
       if ( dta(l,k)%var_g(i,j) /= dble(i+j+(j-1)*DIM2+5) ) error stop 128
       if ( (dta(l,k)%var_h(i,j) /= 'A') ) error stop 130

       if ( dta(l,k)%vdt2(j,i)%var_a(i,j) /= dble(i+j+(j-1)*DIM2-1) ) error stop 132
       if ( (dta(l,k)%vdt2(j,i)%var_b(i,j) .neqv. .true.) ) error stop 134
       if ( dta(l,k)%vdt2(j,i)%var_c(i,j) /= dble(i+j+(j-1)*DIM2+1) ) error stop 136
       if ( (dta(l,k)%vdt2(j,i)%var_d(i,j) /= 'A') ) error stop 138
       if ( dta(l,k)%vdt2(j,i)%var_e(i,j) /= dble(i+j+(j-1)*DIM2+3) ) error stop 140
       if ( dta(l,k)%vdt2(j,i)%var_f(i,j) /= real(i+j+(j-1)*DIM2+4) ) error stop 142
       if ( (dta(l,k)%vdt2(j,i)%var_g(i,j) .neqv. .true.) ) error stop 144
       if ( dta(l,k)%vdt2(j,i)%var_h(i,j) /= real(i+j+(j-1)*DIM2+6) ) error stop 146

       if ( (dta(l,k)%vdt2(j,i)%vdt1(j,i)%var_a(i,j) .neqv. .true.) ) error stop 148
       if ( dta(l,k)%vdt2(j,i)%vdt1(j,i)%var_b(i,j) /= dble(i+j+(j-1)*DIM2) ) error stop 150
       if ( (dta(l,k)%vdt2(j,i)%vdt1(j,i)%var_c(i,j) /= 'A') ) error stop 152
       if ( dta(l,k)%vdt2(j,i)%vdt1(j,i)%var_d(i,j) /= real(i+j+(j-1)*DIM2+2) ) error stop 154
       if ( dta(l,k)%vdt2(j,i)%vdt1(j,i)%var_e(i,j) /= dble(i+j+(j-1)*DIM2+3) ) error stop 156
       if ( dta(l,k)%vdt2(j,i)%vdt1(j,i)%var_f(i,j) /= dble(i+j+(j-1)*DIM2+4) ) error stop 158
       if ( (dta(l,k)%vdt2(j,i)%vdt1(j,i)%var_g(i,j) .neqv. .true.) ) error stop 160
       if ( dta(l,k)%vdt2(j,i)%vdt1(j,i)%var_h(i,j) /= real(i+j+(j-1)*DIM2+6) ) error stop 162
      end do
     end do
    end do
   end do

   do k = 1, DIM1
    do l = 1, DIM2
     do j = 1, DIM2
      do i = 1, DIM1
       dta(l,k)%var_a(i,j) = real(i+j+(j-1)*DIM2)
       dta(l,k)%var_b(i,j) = .not. dta(l,k)%var_b(i,j)
       dta(l,k)%var_c(i,j) = dble(i+j+(j-1)*DIM2+2)
       dta(l,k)%var_d(i,j) = dble(i+j+(j-1)*DIM2+3)
       dta(l,k)%var_e(i,j) = real(i+j+(j-1)*DIM2+4)
       dta(l,k)%var_f(i,j) = .not. dta(l,k)%var_f(i,j)
       dta(l,k)%var_g(i,j) = dble(i+j+(j-1)*DIM2+6)
       dta(l,k)%var_h(i,j) = 'B'

       dta(l,k)%vdt2(j,i)%var_a(i,j) = dble(i+j+(j-1)*DIM2)
       dta(l,k)%vdt2(j,i)%var_b(i,j) = .not. dta(l,k)%vdt2(j,i)%var_b(i,j)
       dta(l,k)%vdt2(j,i)%var_c(i,j) = dble(i+j+(j-1)*DIM2+2)
       dta(l,k)%vdt2(j,i)%var_d(i,j) = 'B'
       dta(l,k)%vdt2(j,i)%var_e(i,j) = dble(i+j+(j-1)*DIM2+4)
       dta(l,k)%vdt2(j,i)%var_f(i,j) = real(i+j+(j-1)*DIM2+5)
       dta(l,k)%vdt2(j,i)%var_g(i,j) = .not. dta(l,k)%vdt2(j,i)%var_g(i,j)
       dta(l,k)%vdt2(j,i)%var_h(i,j) = real(i+j+(j-1)*DIM2+7)

       dta(l,k)%vdt2(j,i)%vdt1(j,i)%var_a(i,j) = .not. dta(l,k)%vdt2(j,i)%vdt1(j,i)%var_a(i,j)
       dta(l,k)%vdt2(j,i)%vdt1(j,i)%var_b(i,j) = dble(i+j+(j-1)*DIM2+1)
       dta(l,k)%vdt2(j,i)%vdt1(j,i)%var_c(i,j) = 'B'
       dta(l,k)%vdt2(j,i)%vdt1(j,i)%var_d(i,j) = real(i+j+(j-1)*DIM2+3)
       dta(l,k)%vdt2(j,i)%vdt1(j,i)%var_e(i,j) = dble(i+j+(j-1)*DIM2+4)
       dta(l,k)%vdt2(j,i)%vdt1(j,i)%var_f(i,j) = dble(i+j+(j-1)*DIM2+5)
       dta(l,k)%vdt2(j,i)%vdt1(j,i)%var_g(i,j) = .not. dta(l,k)%vdt2(j,i)%vdt1(j,i)%var_g(i,j)
       dta(l,k)%vdt2(j,i)%vdt1(j,i)%var_h(i,j) = real(i+j+(j-1)*DIM2+7)
      end do
     end do
    end do
   end do

end subroutine sub1

!! Test 3 Sub
subroutine sub3(dtu)
   type(dt3), dimension(DIM2,DIM1), intent(in) :: dtu

   do k = 1, DIM1
    do l = 1, DIM2
     do j = 1, DIM2
      do i = 1, DIM1
       if ( dtu(l,k)%var_a(i,j) /= real(2*(i+j+(j-1)*DIM2-1)) ) error stop 164
       if ( (dtu(l,k)%var_b(i,j) .neqv. .true.) ) error stop 166
       if ( dtu(l,k)%var_c(i,j) /= dble(2*(i+j+(j-1)*DIM2+1)) ) error stop 168
       if ( dtu(l,k)%var_d(i,j) /= dble(2*(i+j+(j-1)*DIM2+2)) ) error stop 170
       if ( dtu(l,k)%var_e(i,j) /= real(2*(i+j+(j-1)*DIM2+3)) ) error stop 172
       if ( (dtu(l,k)%var_f(i,j) .neqv. .true.) ) error stop 174
       if ( dtu(l,k)%var_g(i,j) /= dble(2*(i+j+(j-1)*DIM2+5)) ) error stop 176
       if ( (dtu(l,k)%var_h(i,j) /= 'A') ) error stop 178

       if ( dtu(l,k)%vdt2(j,i)%var_a(i,j) /= dble(2*(i+j+(j-1)*DIM2-1)) ) error stop 180
       if ( (dtu(l,k)%vdt2(j,i)%var_b(i,j) .neqv. .true.) ) error stop 182
       if ( dtu(l,k)%vdt2(j,i)%var_c(i,j) /= dble(2*(i+j+(j-1)*DIM2+1)) ) error stop 184
       if ( (dtu(l,k)%vdt2(j,i)%var_d(i,j) /= 'A') ) error stop 186
       if ( dtu(l,k)%vdt2(j,i)%var_e(i,j) /= dble(2*(i+j+(j-1)*DIM2+3)) ) error stop 188
       if ( dtu(l,k)%vdt2(j,i)%var_f(i,j) /= real(2*(i+j+(j-1)*DIM2+4)) ) error stop 190
       if ( (dtu(l,k)%vdt2(j,i)%var_g(i,j) .neqv. .true.) ) error stop 192
       if ( dtu(l,k)%vdt2(j,i)%var_h(i,j) /= real(2*(i+j+(j-1)*DIM2+6)) ) error stop 194

       if ( (dtu(l,k)%vdt2(j,i)%vdt1(j,i)%var_a(i,j) .neqv. .true.) ) error stop 196
       if ( dtu(l,k)%vdt2(j,i)%vdt1(j,i)%var_b(i,j) /= dble(2*(i+j+(j-1)*DIM2)) ) error stop 198
       if ( (dtu(l,k)%vdt2(j,i)%vdt1(j,i)%var_c(i,j) /= 'A') ) error stop 200
       if ( dtu(l,k)%vdt2(j,i)%vdt1(j,i)%var_d(i,j) /= real(2*(i+j+(j-1)*DIM2+2)) ) error stop 202
       if ( dtu(l,k)%vdt2(j,i)%vdt1(j,i)%var_e(i,j) /= dble(2*(i+j+(j-1)*DIM2+3)) ) error stop 204
       if ( dtu(l,k)%vdt2(j,i)%vdt1(j,i)%var_f(i,j) /= dble(2*(i+j+(j-1)*DIM2+4)) ) error stop 206
       if ( (dtu(l,k)%vdt2(j,i)%vdt1(j,i)%var_g(i,j) .neqv. .true.) ) error stop 208
       if ( dtu(l,k)%vdt2(j,i)%vdt1(j,i)%var_h(i,j) /= real(2*(i+j+(j-1)*DIM2+6)) ) error stop 210
      end do
     end do
    end do
   end do

   do k = 1, DIM1
    do l = 1, DIM2
     do j = 1, DIM2
      do i = 1, DIM1
       dta(l,k)%var_a(i,j) = real(dtu(l,k)%var_a(i,j)) + 1.0e0
       dta(l,k)%var_b(i,j) = .not. dtu(l,k)%var_b(i,j)
       dta(l,k)%var_c(i,j) = dble(dtu(l,k)%var_c(i,j)) + 3.0q0
       dta(l,k)%var_d(i,j) = dble(dtu(l,k)%var_d(i,j)) + 4.0d0
       dta(l,k)%var_e(i,j) = real(dtu(l,k)%var_e(i,j)) + 5.0e0
       dta(l,k)%var_f(i,j) = .not. dtu(l,k)%var_f(i,j)
       dta(l,k)%var_g(i,j) = dble(dtu(l,k)%var_g(i,j)) + 7.0d0
       dta(l,k)%var_h(i,j) = 'B'

       dta(l,k)%vdt2(j,i)%var_a(i,j) = dble(dtu(l,k)%vdt2(j,i)%var_a(i,j)) + 1.0d0
       dta(l,k)%vdt2(j,i)%var_b(i,j) = .not. dtu(l,k)%vdt2(j,i)%var_b(i,j)
       dta(l,k)%vdt2(j,i)%var_c(i,j) = dble(dtu(l,k)%vdt2(j,i)%var_c(i,j)) + 3.0d0
       dta(l,k)%vdt2(j,i)%var_d(i,j) = 'B'
       dta(l,k)%vdt2(j,i)%var_e(i,j) = dble(dtu(l,k)%vdt2(j,i)%var_e(i,j)) + 5.0q0
       dta(l,k)%vdt2(j,i)%var_f(i,j) = real(dtu(l,k)%vdt2(j,i)%var_f(i,j)) + 6.0e0
       dta(l,k)%vdt2(j,i)%var_g(i,j) = .not. dtu(l,k)%vdt2(j,i)%var_g(i,j)
       dta(l,k)%vdt2(j,i)%var_h(i,j) = real(dtu(l,k)%vdt2(j,i)%var_h(i,j)) + 8.0e0

       dta(l,k)%vdt2(j,i)%vdt1(j,i)%var_a(i,j) = .not. dtu(l,k)%vdt2(j,i)%vdt1(j,i)%var_a(i,j)
       dta(l,k)%vdt2(j,i)%vdt1(j,i)%var_b(i,j) = dble(dtu(l,k)%vdt2(j,i)%vdt1(j,i)%var_b(i,j)) + 2.0d0
       dta(l,k)%vdt2(j,i)%vdt1(j,i)%var_c(i,j) = 'B'
       dta(l,k)%vdt2(j,i)%vdt1(j,i)%var_d(i,j) = real(dtu(l,k)%vdt2(j,i)%vdt1(j,i)%var_d(i,j)) + 4.0e0
       dta(l,k)%vdt2(j,i)%vdt1(j,i)%var_e(i,j) = dble(dtu(l,k)%vdt2(j,i)%vdt1(j,i)%var_e(i,j)) + 5.0q0
       dta(l,k)%vdt2(j,i)%vdt1(j,i)%var_f(i,j) = dble(dtu(l,k)%vdt2(j,i)%vdt1(j,i)%var_f(i,j)) + 6.0d0
       dta(l,k)%vdt2(j,i)%vdt1(j,i)%var_g(i,j) = .not. dtu(l,k)%vdt2(j,i)%vdt1(j,i)%var_g(i,j)
       dta(l,k)%vdt2(j,i)%vdt1(j,i)%var_h(i,j) = real(dtu(l,k)%vdt2(j,i)%vdt1(j,i)%var_h(i,j)) + 8.0e0
      end do
     end do
    end do
   end do

end subroutine sub3

end program fxdtyla43
