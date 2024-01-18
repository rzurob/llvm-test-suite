! *********************************************************************
!* ===================================================================
!* XL Fortran Test Case                         IBM INTERNAL USE ONLY
!* ===================================================================
!*
!* TEST CASE TITLE              : fcintrpopt105.f
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
!* Calling a BIND(C) procedure from Fortran, where the procedure is defined in Fortran
!*
!* Actual Argument:
!*   When have multiple optional dummy arguments in a function or subroutine, supply
!*   with no actual argument, or actual arguments for all the optional ones, or
!*   actual arguments for some of the optional arguments
!*
!* Dummy Argument:
!*  explicit-shape array of intrinsic type and assumed sized array of derived type
!*
!* ===================================================================
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*  12/06/14    YZ     -Initial Version
!* ===================================================================
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module testmod
  use iso_c_binding

  type, bind(c) :: dt0
      integer(c_int64_t) :: ia
  end type

  type, bind(c) :: dt1
      integer(c_int_least64_t) :: ia
      type(dt0) :: d0
  end type

interface

  subroutine sub1(arg1, size, arg2) bind(c)
   use iso_c_binding
   import dt0
   integer :: size
   integer(c_int32_t), optional, intent(inout), dimension(5) :: arg1
   type(dt0), optional, intent(inout), dimension(*) :: arg2
  end subroutine sub1

  logical(c_bool) function func1(size, arg1, arg2) bind(c)
   use iso_c_binding
   import dt1
   integer :: size
   type(dt1), optional, intent(in), dimension(*) :: arg1
   integer(c_int_fast64_t), optional, intent(in), dimension(2,5) :: arg2
  end function func1

end interface

end module testmod

program test
  use testmod
  implicit none
  
  integer, parameter :: DIM1=2, DIM2=3
  logical res
  integer i,j

  integer(c_int32_t) ci32_arr(5)
  type(dt0), allocatable, target :: dt0_arr(:,:) 

  type(dt1) :: dt1_arr(DIM1, DIM2)
  integer(c_int_fast64_t) cif64_arr(2,5)

  ci32_arr = (/1,2,3,4,5/)
  allocate(dt0_arr(DIM1,DIM2))
  do i=1, DIM1
    do j=1, DIM2
	dt0_arr(i, j)%ia = i+j
    enddo
  enddo
  call sub1(ci32_arr, DIM1*DIM2)
  call sub1(size=DIM1*DIM2, arg2=dt0_arr)

  do i=1, DIM1 
    do j=1, DIM2
       dt1_arr(i, j)%ia = i
       dt1_arr(i, j)%d0%ia = j+100
    enddo
  enddo

  cif64_arr = reshape ((/1,2,1,2,1,2,1,2,1,2/),(/2,5/))
  res = func1(DIM1*DIM2, dt1_arr)
  print *, res

  res = func1(DIM1*DIM2, arg2=cif64_arr)
  print *, res
end program test

subroutine sub1(arg1, size, arg2) bind(c)
   use iso_c_binding
   use testmod, only : dt0
   integer :: size
   integer(c_int32_t), optional, intent(inout), dimension(5) :: arg1
   type(dt0), optional, intent(inout), dimension(*) :: arg2

   if (present(arg1)) then
     do i=1, 5
        print *, arg1(i)
     end do
   else
        print *, "arg1 in sub1 not presnet"
   endif

   if (present(arg2)) then
     do i=1, size
        print *, arg2(i)
     end do
   else
        print *, "arg2 in sub1 not presnet"
   endif

end subroutine sub1

logical(c_bool) function func1(size, arg1, arg2) bind(c)
   use iso_c_binding
   use testmod, only : dt1
   integer :: size
   type(dt1), optional, intent(in), dimension(*) :: arg1
   integer(c_int_fast64_t), optional, intent(in), dimension(2,5) :: arg2

   func1 = .true.

   if (present(arg1)) then
     do i=1, size
        print *, arg1(i)
     end do
   else
        print *, "arg1 in func1 not presnet"
   endif

   if (present(arg2)) then
     do i=1, 5
        print *, arg2(1, i)
     end do
   else
        print *, "arg2 in func1 not presnet"
   endif

   func1 = present(arg1) .and. present(arg2)

end function func1

