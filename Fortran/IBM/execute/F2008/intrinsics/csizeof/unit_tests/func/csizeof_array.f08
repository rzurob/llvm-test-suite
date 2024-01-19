      program t
      use, intrinsic :: iso_c_binding
      implicit none

      interface
        subroutine sub10(arr10, i, j)
          use, intrinsic :: iso_c_binding
          implicit none
          real(c_float) arr10(1:, :, 1:)
          integer i, j
        end subroutine
      end interface

      real(c_float) arrx1(30)
      real(c_float) arrx2(4, 6, 8)
      real(c_float) arrx3(2, 8)

      call sub0(3, 4)
      call sub1(4, 5)
      call sub2(3, arrx1)

      call sub10(arrx2, 1, 2)
      call sub11(arrx3, 4, 5)
      call sub12(3, 4)

      end

      subroutine sub0(i, j) ! Explicit-shape arrays
      use, intrinsic :: iso_c_binding
      implicit none
      complex(c_long_double_complex) :: arr0(10, 20) ! 32 * 200
      integer(c_int) i, j ! i =3 j =4
      integer(C_SIZE_T) :: rt

      rt = c_sizeof(arr0)
      if (rt /= 32 * 200) error stop 1

      rt = c_sizeof(arr0(3, 9))
      if (rt /= 32) error stop 2

      rt = c_sizeof(arr0(2:3, 6:9))
      if (rt /= 32 * 2 * 4) error stop 3

      rt = c_sizeof(arr0(i, j))
      if (rt /= 32) error stop 4

      rt = c_sizeof(arr0(i:5, j:10))
      if (rt /= 32 * size(arr0(i:5, j:10))) error stop 5

      end subroutine

      subroutine sub1(col, row) ! Explicit-shape Auto arrays
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int) col, row ! col = 4, row = 5
      integer(c_int) i, j
      character(c_char) :: arr1(col:8, 1:row) ! 1 * 25
      integer(C_SIZE_T) :: rt
      i = col - 3
      j = row -4

      rt = c_sizeof(arr1)
      if (rt /= 1 * size(arr1)) error stop 6

      rt = c_sizeof(arr1(1, 2))
      if (rt /= 1) error stop 7

      rt = c_sizeof(arr1(2:3, 6:9))
      if (rt /= 1 * 2 * 4) error stop 8

      rt = c_sizeof(arr1(i, j+3))
      if (rt /= 1) error stop 9

      rt = c_sizeof(arr1(i:5, j:3))
      if (rt /= 1 * size(arr1(i:5, j:3))) error stop 10

      end subroutine

      subroutine sub2(row, arr2) ! Explicit-shape Adjustable arrays
      use, intrinsic :: iso_c_binding
      implicit none
      integer(c_int) row, i  ! row =3, arrx(30)
      real(c_float) arr2(3*row) ! 4 * 9
      integer(C_SIZE_T) ::rt
      i = row - 2

      rt = c_sizeof(arr2)
      if (rt /= 4 * size(arr2)) error stop 11

      rt = c_sizeof(arr2(1))
      if (rt /= 4) error stop 12

      rt = c_sizeof(arr2(2:3))
      if (rt /= 4 * 2) error stop 13

      rt = c_sizeof(arr2(i+3))
      if (rt /= 4) error stop 14

      rt = c_sizeof(arr2(i:5))
      if (rt /= 4 * (5 - i + 1)) error stop 15

      end subroutine

      subroutine sub10(arr10, i, j) ! element or section of Assumed-shape arrays
      use, intrinsic :: iso_c_binding
      implicit none
      real(c_float) arr10(1:, :, 1:)
      integer(c_size_t) :: rt
      integer i, j !i =1 j=2

      rt = c_sizeof(arr10(i+1:i+2, j+1:j+2, i:j))
      if (rt /= 4 * size(arr10(i+1:i+2, j+1:j+2, i:j))) error stop 16

      rt = c_sizeof(arr10(i+1, j+2, i+j))
      if (rt /= 4) error stop 17

      rt = c_sizeof(4+arr10)
      if (rt /= 4 * size(arr10)) error stop 18

      rt = c_sizeof(arr10*kind(4))
      if (rt /= 4 * size(arr10)) error stop 19
      end subroutine

      subroutine sub11(arr11, i, j) ! element or section of Assumed-size arrays
      use, intrinsic :: iso_c_binding
      implicit none
      integer(c_size_t) :: rt
      real(c_float) :: arr11(2, *)
      integer i, j  !i =4 j=5

      rt = c_sizeof(arr11(i+1:i+2, j+1:j+2))
      if (rt /= 4 * 2 * 2) error stop 20

      rt = c_sizeof(arr11(i+1, j+2))
      if (rt /= 4) error stop 21

      end subroutine

      subroutine sub12(i, j) ! element or section of Deferred-shape arrays
      use, intrinsic :: iso_c_binding
      implicit none
      integer i, j !i =3 j=4
      integer(c_size_t) :: rt
      real, allocatable, dimension(:, :, :) :: arr12_1  !Allocatable arrays
      character, pointer, dimension(:, :) :: arr12_2  !Array pointers
      character, target, dimension(5, 10)  :: arr12_2_t

      ALLOCATE(arr12_1(10, -4:5, 20))
      arr12_2 => arr12_2_t

      rt = c_sizeof(arr12_1(i+1:i+2, j+1:j+2, i+j:i+j))
      if (rt /= 4 * 2 * 2 * 1) error stop 21

      rt = c_sizeof(arr12_1(i+1, j+2, i+j))
      if (rt /= 4) error stop 22

      rt = c_sizeof(arr12_2(i+1:i+2, j+1:j+2))
      if (rt /= 1 * 2 * 2) error stop 23

      rt = c_sizeof(arr12_2(i+1, j+2))
      if (rt /= 1) error stop 24

      rt = c_sizeof(4+arr12_1)
      if (rt /= 4 * size(arr12_1)) error stop 25

      rt = c_sizeof(arr12_1*kind(4))
      if (rt /= 4 * size(arr12_1)) error stop 26

      end subroutine
