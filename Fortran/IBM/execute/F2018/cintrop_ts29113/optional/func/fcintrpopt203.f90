
! *********************************************************************
!* ===================================================================
!* XL Fortran Test Case                         IBM INTERNAL USE ONLY
!* ===================================================================
!*
!* TEST CASE TITLE              : fcintrpopt203.f
!*
!* PROGRAMMER                   : Ying Zhang
!* DATE                         : June 25, 2012
!* ORIGIN                       : AIX Complier Development
!*                              : IBM Software Solutions Toronto Lab
!*
!* PRIMARY FUNCTIONS TESTED     : 399982 - C Interop: Optional Argument
!* SECONDARY FUNTIONS TESTED    :
!*
!* DRIVER STANZA                :
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  :
!*
!* Calling a BIND(C) procedure from Fortran, (where the procedure is defined in C)
!*
!* Actual Argument:
!*  POINTER to explicit-shape array of intrinsic type
!* Dummy Argument:
!*  array of structures + some interoperable C types
!*
!* ===================================================================
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*  12/06/14    YZ     -Initial Version
!* ===================================================================
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module  dtmod
  use iso_c_binding
  implicit none

  type, bind(c) :: dt1
	integer(c_long_long) :: a
	integer(c_int16_t) :: b(2)
  end type

  type, bind(c) :: dt2
	real(c_float) :: c(2)
  	type(dt1) :: d1
  end type

end module dtmod

module testmod
  use dtmod

  interface
    subroutine c_sub_test1(arg1, arg2) bind(c)
	import
	type(dt1), optional, dimension(5) :: arg1
	integer(c_short), optional :: arg2
    end subroutine c_sub_test1

    integer(c_int) function c_func_test2(arg1) bind(c)
	import
	type(dt2), optional, dimension(5, 3) :: arg1
    end function c_func_test2

  end interface
end module testmod

program test
  use iso_c_binding
  use dtmod
  use testmod
  implicit none
  
  integer i, j, k, tmp
  integer(c_size_t) size
  integer, parameter :: DIM1=5, DIM2=3

  type(dt1), pointer :: pdt1_arr(:)
  integer(c_short), pointer :: psi
  type(dt2), pointer :: pdt2_arr(:,:)

  type(dt1), dimension(DIM1), target :: tdt1_arr
  integer(c_short), target :: tsi
  type(dt2), dimension(DIM1, DIM2), target :: tdt2_arr


  do i=1, DIM1
    do j=1, 2
	tdt1_arr(i)%a = i
	tdt1_arr(i)%b(j) = i + j
    enddo
  enddo
  pdt1_arr => tdt1_arr

  tsi = 100
  psi => tsi

  call c_sub_test1()
  call c_sub_test1(pdt1_arr)
  call c_sub_test1(arg2=psi)
  call c_sub_test1(pdt1_arr, psi)

  do j=1, DIM2 
    do i=1, DIM1
      do k=1, 2
	tdt2_arr(i,j)%c(k) = i+j+k*1.0
        tdt2_arr(i,j)%d1 = tdt1_arr(i)
      enddo
    enddo
  enddo

  pdt2_arr => tdt2_arr

  size = c_func_test2()

  size = c_func_test2(tdt2_arr)

end program test
