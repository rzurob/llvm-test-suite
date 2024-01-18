! *********************************************************************
!* ===================================================================
!* XL Fortran Test Case                         IBM INTERNAL USE ONLY
!* ===================================================================
!*
!* TEST CASE TITLE              : fcintrpopt104.f
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
!*   associated POINTER
!*
!* Dummy Argument:
!*   assumed sized array of intrinsic type
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
interface
  subroutine sub1(size, arg1) bind(c)
   use iso_c_binding
   integer :: size
   integer(c_int8_t), optional, intent(inout), dimension(*) :: arg1
  end subroutine sub1

  subroutine sub2(size1, size2, arg1, arg2) bind(c)
   use iso_c_binding
   integer :: size1, size2
   real(c_double), optional, intent(in), dimension(*) :: arg1
   integer(c_int_fast8_t), optional, intent(inout), dimension(*) :: arg2
  end subroutine sub2

  integer(c_int) function func1(size1, size2, size3, arg1, arg2, arg3) bind(c)
   use iso_c_binding
   integer :: size1, size2, size3
   integer(c_long), optional, intent(in), dimension(*) :: arg1
   integer(c_long_long), optional, intent(in), dimension(*) :: arg2
   integer(c_int_fast16_t), optional, intent(inout), dimension(*) :: arg3
  end function func1
end interface
end module testmod

program test
  use iso_c_binding
  use testmod
  implicit none
  
  integer, parameter :: DIM1=3, DIM2=3, DIM3=3 
  integer totalsize

  integer(c_int8_t), pointer :: pci8_arr(:, :)
  real(c_double), pointer :: pcd_arr(:)
  integer(c_int_fast8_t), pointer :: pif8_arr(:,:)  
  integer(c_long), pointer :: pl_arr(:,:,:)
  integer(c_long_long), pointer :: pll_arr(:,:)
  integer(c_int_fast16_t), pointer :: pif16_arr(:,:,:)

  integer(c_int8_t), dimension(DIM1, DIM2), target :: tci8_arr
  real(c_double), dimension(DIM1), target :: tcd_arr
  integer(c_int_fast8_t), dimension(DIM1, DIM2), target :: tif8_arr
  integer(c_long), dimension(DIM1, DIM2, DIM3), target :: tl_arr
  integer(c_long_long), dimension(DIM1, DIM2), target :: tll_arr
  integer(c_int_fast16_t), dimension(DIM1, DIM2, DIM3), target :: tif16_arr

  tci8_arr = reshape ((/1,2,3,4,5,6,7,8,9/),(/DIM1, DIM2/))
  pci8_arr => tci8_arr
  call sub1(0)
  call sub1(DIM1*DIM2, pci8_arr)

  tcd_arr = (/10.0, 11.0, 12.0/)
  pcd_arr => tcd_arr
  tif8_arr = reshape ((/11,22,33,44,55,66,77,88,99/),(/DIM1, DIM2/))
  pif8_arr => tif8_arr
  call sub2(DIM1, 0, pcd_arr)
  call sub2(0, DIM1*DIM2, arg2=pif8_arr)

  tl_arr = reshape ((/1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3/),(/DIM1, DIM2, DIM3/))
  pl_arr => tl_arr
  tll_arr = reshape ((/7,8,9,7,8,9,7,8,9/),(/DIM1, DIM2/))
  pll_arr => tll_arr
  tif16_arr = reshape ((/56,2,3,56,2,3,56,2,3,56,2,3,56,2,3,56,2,3,56,2,3,56,2,3,56,2,3/),(/DIM1, DIM2, DIM3/))
  pif16_arr => tif16_arr 
  totalsize = func1(0,0,0)
  totalsize = func1(DIM1*DIM2*DIM3,0,0, pl_arr)
  totalsize = func1(0,DIM1*DIM2,0, arg2=pll_arr)
  totalsize = func1(0,0,DIM1*DIM2*DIM3, arg3=pif16_arr)
  totalsize = func1(0,DIM1*DIM2,DIM1*DIM2*DIM3, arg3=pif16_arr, arg2=pll_arr)

end program test

subroutine sub1(size, arg1) bind(c)
   use iso_c_binding
   integer :: size
   integer(c_int8_t), optional, intent(inout), dimension(*) :: arg1

   if (present(arg1)) then
     do i=1, size
        print *, arg1(i)
     end do
   else
        print *, "arg1 in sub1 not presnet"
   endif

end subroutine sub1

subroutine sub2(size1, size2, arg1, arg2) bind(c)
   use iso_c_binding
   integer :: size1, size2
   real(c_double), optional, intent(in), dimension(*) :: arg1
   integer(c_int_fast8_t), optional, intent(inout), dimension(*) :: arg2

   if (present(arg1)) then
     do i=1, size1
        print *, arg1(i)
     end do
   else
        print *, "arg1 in sub2 not presnet"
   endif

   if (present(arg2)) then
     do i=1, size2
        print *, arg2(i)
     end do
   else
        print *, "arg2 in sub2 not presnet"
   endif

end subroutine sub2

integer(c_int) function func1(size1, size2, size3, arg1, arg2, arg3) bind(c)
   use iso_c_binding
   integer :: size1, size2, size3
   integer(c_long), optional, intent(in), dimension(*) :: arg1
   integer(c_long_long), optional, intent(in), dimension(*) :: arg2
   integer(c_int_fast16_t), optional, intent(inout), dimension(*) :: arg3

   func1 = 0

   if (present(arg1)) then
     do i=1, size1
        print *, arg1(i)
     end do
     func1 = func1 + size1
   else
        print *, "arg1 in func1 not presnet"
   endif

   if (present(arg2)) then
     do i=1, size2
        print *, arg2(i)
     end do
     func1 = func1 + size2
   else
        print *, "arg2 in func1 not presnet"
   endif

   if (present(arg3)) then
     do i=1, size3
        print *, arg3(i)
     end do
     func1 = func1 + size3
   else
        print *, "arg3 in func1 not presnet"
   endif

end function func1   
