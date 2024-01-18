! *********************************************************************
!* ===================================================================
!*
!* DATE                         : January 25, 2013
!* ORIGIN                       : AIX Complier Development
!*
!* PRIMARY FUNCTIONS TESTED     : C Interop: ALLOCATABLE and POINTER dummy argument
!* SECONDARY FUNTIONS TESTED    :
!*
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  : Calling a Fortran BIND(C) procedure from Fortran
!*                                - pointer dummy argument enhancement
!*
!234567890123456789012345678901234567890123456789012345678901234567890
module mod
  use iso_c_binding
  implicit none

  contains
    subroutine sub1(arr) bind(c)
      integer(c_int), pointer, intent(in) :: arr(:)

      if( .not. associated(arr) ) ERROR STOP 10
      print *, arr
      print *, size(arr), lbound(arr), ubound(arr), shape(arr)
    end subroutine

    integer(c_int) function arr_add(arr1, arr2) bind(c)
      integer(c_int), pointer, intent(in) :: arr1(:)
      integer(c_int), pointer, intent(in) :: arr2(:)
      integer i

      if( .not. associated(arr1) ) ERROR STOP 20
      if( .not. associated(arr2) ) ERROR STOP 21
      if( size(arr2) < size(arr1) ) then
            print*, "Program will go out of bound"
            ERROR STOP 22
      end if

      arr_add = 0

      do i = 1, size(arr1), 1
        arr_add = arr_add + arr1(i) + arr2(i)
      end do
    end function

    subroutine arr_part(v, sec) bind(c)
      integer(c_int), pointer, intent(in) :: v
      integer(c_int), pointer, intent(in) :: sec(:)
      integer(c_int) :: res
      integer i

      if( .not.   associated(v) ) ERROR STOP 30
      if( .not. associated(sec) ) ERROR STOP 31

      res = 0

      print *, v
      print *, size(sec)

      do i = 1, size(sec), 1
        res = res + sec(i)
      end do
      res = res + v

      print *, res
    end

    subroutine dimen_arr(arr1, arr2) bind(c)
      integer(c_int), pointer, intent(in) :: arr1(:, :)
      integer(c_int), pointer, intent(in) :: arr2(:, :)

      if( .not. associated(arr1) ) ERROR STOP 40
      if( .not. associated(arr2) ) ERROR STOP 41
      print *, arr1
      print *, arr2
    end

    subroutine misc(a, b, c) bind(c)
      integer(c_int), pointer, intent(in) :: a(:, :), c(:, :)
      integer(c_int), pointer, intent(in) :: b

      if( .not. associated(a) ) ERROR STOP 50
      if( .not. associated(b) ) ERROR STOP 51
      if( .not. associated(c) ) ERROR STOP 52
      print *, "a: ", a
      print *, "b: ", b
      print *, "c: ", c
    end
end module mod

  use mod
  implicit none

  integer, pointer :: arr_p(:)
  integer, target :: arr1(5)

  integer, pointer :: dimen_p(:, :)
  integer, target :: arr2(3, 3)

  integer i,j

  arr1 = [1, 2, 3, 4, 5]

  do i = 1, 3
    do j = 1, 3
      arr2(i, j) = i * j
    end do
  end do

  arr_p => arr1
  dimen_p => arr2

  call sub1(arr_p)
  call sub1(arr1)

  print *, "arr_add: ", arr_add(arr_p, arr1)

  call arr_part(arr1(2), arr1(3:5))

  call dimen_arr(arr2, dimen_p)

  call arr_part(arr2(3, 3), arr2(1, 2:3))

  call dimen_arr(arr2(1:2, 2:3), dimen_p)

  call misc(dimen_p, arr2(2, 3), arr2(2:3, 1:2))
end
