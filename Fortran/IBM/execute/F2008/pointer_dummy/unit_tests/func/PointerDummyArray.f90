!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : PointerDummyArray.f
!*
!*  DATE                       : May 22, 2011
!*  ORIGIN                     : Compiler Development, IBM CDL
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature Number 916820
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION:
!*  -----------
!*  Fortran 2008 allows procedure calls where:
!*	  1. The dummy argument has the POINTER and INTENT(IN) attributes, and
!*	  2. The actual argument is a nonpointer that has the TARGET attribute
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module arr_test
  contains
    subroutine sub1(arr)
      integer, pointer, intent(in) :: arr(:)

      print *, arr
      print *, size(arr)
    end subroutine

    integer function arr_add(arr1, arr2)
      integer, pointer, intent(in) :: arr1(:)
      integer, pointer, intent(in) :: arr2(:)
      integer :: res

      res = 0

      do i = 1, size(arr1), 1
        res = res + arr1(i) + arr2(i)
      end do

      arr_add = res
    end function

    subroutine arr_part(v, sec)
      integer, pointer, intent(in) :: v
      integer, pointer, intent(in) :: sec(:)
      integer :: res

      res = 0

      print *, v
      print *, size(sec)

      do i = 1, size(sec), 1
        res = res + sec(i)
      end do
      res = res + v

      print *, res
    end

    subroutine dimen_arr(arr1, arr2)
      integer, pointer, intent(in) :: arr1(:, :)
      integer, pointer, intent(in) :: arr2(:, :)

      print *, arr1
      print *, arr2
    end

    subroutine misc(a, b, c)
      integer, pointer, intent(in) :: a(:, :), c(:, :)
      integer, pointer, intent(in) :: b

      print *, "a(Not pointer dummy enhancement): ", a
      print *, "b(Pointer dummy enhancement): ", b
      print *, "c(Pointer dummy enhancement): ", c
    end
end module arr_test

program t
  use arr_test

  integer, pointer :: arr_p(:)
  integer, target :: arr1(5)

  integer, pointer :: dimen_p(:, :)
  integer, target :: arr2(3, 3)

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
