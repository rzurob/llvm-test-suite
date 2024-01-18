!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
!***********************************************************************
! %START
! %MAIN:
! %PRECMD: ${TR_SRC}/scrdtyc00.presh fxdtycc16a cxdtycc16
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
!*      - Testing arrays of single derived types with BIND(C) attribute
!*      - Testing arrays of single derived types with INTENT attributes
!*      - Testing arrays of single derived types with integer and complex components
!*      - Testing FORTRAN subroutines and C void functions
!*      - Main written in C
!*
!234567890123456789012345678901234567890123456789012345678901234567890

subroutine sub1(dt) bind(c)
   use ISO_C_BINDING
   integer, parameter :: DIM1 = 3, DIM2 = 2

   type, bind(c) :: dt1
      complex(C_FLOAT_COMPLEX) var_a(DIM1,DIM2)
      integer(C_SHORT) var_b(DIM1,DIM2)
      complex(C_LONG_DOUBLE_COMPLEX) var_c(DIM1,DIM2)
      complex(8) var_d(DIM1,DIM2)
      complex(4) var_e(DIM1,DIM2)
      integer(1) var_f(DIM1,DIM2)
      complex(C_DOUBLE_COMPLEX) var_g(DIM1,DIM2)
      integer(4) var_h(DIM1,DIM2)
   end type

   type(dt1), dimension(DIM2,DIM1) :: dt

   do k = 1, DIM1
    do l = 1, DIM2
     do j = 1, DIM2
      do i = 1, DIM1
       if ( dt(l,k)%var_a(i,j) /= (real(i+j+(j-1)*DIM2-1),real(i+j+(j-1)*DIM2-1)) ) error stop 20
       if ( dt(l,k)%var_b(i,j) /= i+j+(j-1)*DIM2 ) error stop 22
       if ( dt(l,k)%var_c(i,j) /= (dble(i+j+(j-1)*DIM2+1),dble(i+j+(j-1)*DIM2+1)) ) error stop 24
       if ( dt(l,k)%var_d(i,j) /= (dble(i+j+(j-1)*DIM2+2),dble(i+j+(j-1)*DIM2+2)) ) error stop 26
       if ( dt(l,k)%var_e(i,j) /= (real(i+j+(j-1)*DIM2+3),real(i+j+(j-1)*DIM2+3)) ) error stop 28
       if ( dt(l,k)%var_f(i,j) /= i+j+(j-1)*DIM2+4 ) error stop 30
       if ( dt(l,k)%var_g(i,j) /= (dble(i+j+(j-1)*DIM2+5),dble(i+j+(j-1)*DIM2+5)) ) error stop 32
       if ( dt(l,k)%var_h(i,j) /= i+j+(j-1)*DIM2+6 ) error stop 34
      end do
     end do
    end do
   end do

   do k = 1, DIM1
    do l = 1, DIM2
     do j = 1, DIM2
      do i = 1, DIM1
       dt(l,k)%var_a(i,j) = (real(i+j+(j-1)*DIM2),real(i+j+(j-1)*DIM2))
       dt(l,k)%var_b(i,j) = i+j+(j-1)*DIM2+1
       dt(l,k)%var_c(i,j) = (dble(i+j+(j-1)*DIM2+2),dble(i+j+(j-1)*DIM2+2))
       dt(l,k)%var_d(i,j) = (dble(i+j+(j-1)*DIM2+3),dble(i+j+(j-1)*DIM2+3))
       dt(l,k)%var_e(i,j) = (real(i+j+(j-1)*DIM2+4),real(i+j+(j-1)*DIM2+4))
       dt(l,k)%var_f(i,j) = i+j+(j-1)*DIM2+5
       dt(l,k)%var_g(i,j) = (dble(i+j+(j-1)*DIM2+6),dble(i+j+(j-1)*DIM2+6))
       dt(l,k)%var_h(i,j) = i+j+(j-1)*DIM2+7
      end do
     end do
    end do
   end do

end subroutine sub1

subroutine sub2(dtx,dty) bind(c)
   use ISO_C_BINDING
   integer, parameter :: DIM1 = 3, DIM2 = 2

   type, bind(c) :: dt1
      complex(C_FLOAT_COMPLEX) var_a(DIM1,DIM2)
      integer(C_SHORT) var_b(DIM1,DIM2)
      complex(C_LONG_DOUBLE_COMPLEX) var_c(DIM1,DIM2)
      complex(8) var_d(DIM1,DIM2)
      complex(4) var_e(DIM1,DIM2)
      integer(1) var_f(DIM1,DIM2)
      complex(C_DOUBLE_COMPLEX) var_g(DIM1,DIM2)
      integer(4) var_h(DIM1,DIM2)
   end type

   type(dt1), dimension(DIM2,DIM1), intent(in) :: dtx
   type(dt1), dimension(DIM2,DIM1), intent(out) :: dty

   do k = 1, DIM1
    do l = 1, DIM2
     do j = 1, DIM2
      do i = 1, DIM1
       if ( dtx(l,k)%var_a(i,j) /= (real(2*(i+j+(j-1)*DIM2-1)),real(2*(i+j+(j-1)*DIM2-1))) ) error stop 36
       if ( dtx(l,k)%var_b(i,j) /= 2*(i+j+(j-1)*DIM2) ) error stop 38
       if ( dtx(l,k)%var_c(i,j) /= (dble(2*(i+j+(j-1)*DIM2+1)),dble(2*(i+j+(j-1)*DIM2+1))) ) error stop 40
       if ( dtx(l,k)%var_d(i,j) /= (dble(2*(i+j+(j-1)*DIM2+2)),dble(2*(i+j+(j-1)*DIM2+2))) ) error stop 42
       if ( dtx(l,k)%var_e(i,j) /= (real(2*(i+j+(j-1)*DIM2+3)),real(2*(i+j+(j-1)*DIM2+3))) ) error stop 44
       if ( dtx(l,k)%var_f(i,j) /= 2*(i+j+(j-1)*DIM2+4) ) error stop 46
       if ( dtx(l,k)%var_g(i,j) /= (dble(2*(i+j+(j-1)*DIM2+5)),dble(2*(i+j+(j-1)*DIM2+5))) ) error stop 48
       if ( dtx(l,k)%var_h(i,j) /= 2*(i+j+(j-1)*DIM2+6) ) error stop 50
      end do
     end do
    end do
   end do

   do k = 1, DIM1
    do l = 1, DIM2
     do j = 1, DIM2
      do i = 1, DIM1
       dty(l,k)%var_a(i,j) = (real(2*(i+j+(j-1)*DIM2-1)+1),real(2*(i+j+(j-1)*DIM2-1)+1))
       dty(l,k)%var_b(i,j) = 2*(i+j+(j-1)*DIM2)+1
       dty(l,k)%var_c(i,j) = (dble(2*(i+j+(j-1)*DIM2+1)+1),dble(2*(i+j+(j-1)*DIM2+1)+1))
       dty(l,k)%var_d(i,j) = (dble(2*(i+j+(j-1)*DIM2+2)+1),dble(2*(i+j+(j-1)*DIM2+2)+1))
       dty(l,k)%var_e(i,j) = (real(2*(i+j+(j-1)*DIM2+3)+1),real(2*(i+j+(j-1)*DIM2+3)+1))
       dty(l,k)%var_f(i,j) = 2*(i+j+(j-1)*DIM2+4)+1
       dty(l,k)%var_g(i,j) = (dble(2*(i+j+(j-1)*DIM2+5)+1),dble(2*(i+j+(j-1)*DIM2+5)+1))
       dty(l,k)%var_h(i,j) = 2*(i+j+(j-1)*DIM2+6)+1
      end do
     end do
    end do
   end do

end subroutine sub2
