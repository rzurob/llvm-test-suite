! *********************************************************************
!* ===================================================================
!* XL Fortran Test Case                         IBM INTERNAL USE ONLY
!* ===================================================================
!*
!* TEST CASE TITLE              : PointerDummyArgument555f.f
!*
!* PROGRAMMER                   : Dorra Bouchiha
!* DATE                         : July 28, 2013
!* ORIGIN                       : AIX Complier Development
!*                              : IBM Software Solutions Toronto Lab
!*
!* PRIMARY FUNCTIONS TESTED     : C Interop: ALLOCATABLE and POINTER dummy argument
!* SECONDARY FUNTIONS TESTED    :
!*
!* DRIVER STANZA                :
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  : Calling a BIND(C) procedure defined in C from Fortran
!*                                - multiple optional dummy arg. Use keyword for the call
!*                                - optional dummy arg. with pointer attribute
!*                                - derived type 
!*
!* Actual Argument:
!*  pointer to explicit-shape array of derived type
!* Dummy Argument:
!*  pointer array of derived type
!*
!234567890123456789012345678901234567890123456789012345678901234567890
module  mod
  use iso_c_binding
  implicit none

  type, bind(c) :: dt1
	integer(c_int) :: a
	integer(c_int) :: b(2)
  end type

  type, bind(c) :: dt2
	real(c_float) :: c(2)
  	type(dt1) :: d1
  end type

end module mod

module testmod
  use mod

  interface
    subroutine c_sub_1D(arg1, ptr, arg2) bind(c)
        import
        type(dt1), pointer :: ptr(:)                              !dimension(5)
        integer(c_short), optional :: arg1, arg2 
    end subroutine c_sub_1D

    subroutine c_sub_1D_opt(arg1, ptr, arg2) bind(c)
        import
        type(dt1), pointer :: ptr(:)                              !dimension(5)
        integer(c_short), pointer, optional :: arg1, arg2       
    end subroutine c_sub_1D_opt

    integer(c_int) function c_fnc_2D(ptr) bind(c)
        import
        type(dt2), pointer :: ptr(:,:)   !dimension(5, 3)
    end function c_fnc_2D
  end interface
end module testmod

program AllocatableDummyArgument555f
  use iso_c_binding
  use mod
  use testmod
  implicit none
  
  integer i, j, k
  integer(c_int) size
  integer, parameter :: DIM1=5, DIM2=3

  integer(c_short), pointer :: psi
  type(dt1), pointer :: pdt1_arr(:)
  type(dt2), pointer :: pdt2_arr(:,:)

  integer(c_short), target :: tsi
  type(dt1), dimension(DIM1), target :: tdt1_arr
  type(dt2), dimension(DIM1, DIM2), target :: tdt2_arr


  do i=1, DIM1
    do j=1, 2
        tdt1_arr(i)%a = i
        tdt1_arr(i)%b(j) = i + j
    enddo
  enddo
  pdt1_arr => tdt1_arr

!debug 
!  do i=1, DIM1
!      print*, "pdt1_arr(",i,")%a :", pdt1_arr(i)%a
!      print*, "pdt1_arr(",i,")%b :", pdt1_arr(i)%b
!  enddo

  tsi = 100
  psi => tsi
!debug 
!  print*, "psi :", psi


  ! Using optional dummy arg. before and/or after the pointer dummy arg. 
  ! optional dummy arg. does not have the pointer attribute
  call c_sub_1D(ptr=pdt1_arr)
  call c_sub_1D(ptr=pdt1_arr, arg2=psi)
  call c_sub_1D(psi, pdt1_arr, psi)
  call c_sub_1D(arg1=psi, ptr=pdt1_arr)

  ! optional dummy arg. has the pointer attribute
  call c_sub_1D_opt(ptr=pdt1_arr)
  call c_sub_1D_opt(ptr=pdt1_arr, arg2=psi)
  call c_sub_1D_opt(psi, pdt1_arr, psi)
  call c_sub_1D_opt(arg1=psi, ptr=pdt1_arr)

  do j=1, DIM2 
    do i=1, DIM1
      do k=1, 2
        tdt2_arr(i,j)%c(k) = i+j+k*1.0
        tdt2_arr(i,j)%d1 = tdt1_arr(i)
      enddo
    enddo
  enddo

  pdt2_arr => tdt2_arr

!debug 
!  do j=1, DIM2 
!    do i=1, DIM1
!      print*, "pdt2_arr(",i,",",j,")%c :", pdt2_arr(i,j)%c
!      print*, "pdt2_arr(",i,",",j,")%d1%a :", pdt2_arr(i,j)%d1%a
!      print*, "pdt2_arr(",i,",",j,")%d1%b :", pdt2_arr(i,j)%d1%b
!    enddo
!  enddo

  size = c_fnc_2D(pdt2_arr)
  if( size .ne. 15 ) ERROR STOP 10

end program AllocatableDummyArgument555f
