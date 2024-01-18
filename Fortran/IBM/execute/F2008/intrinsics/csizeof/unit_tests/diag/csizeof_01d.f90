      program t
      use, intrinsic :: iso_c_binding
      implicit none

      integer rt

      byte y
      character(len=2) ch
      logical(2) logc2
      logical(4) logc4
      logical(8) logc8
      logical    logc

      real :: arrx(20, 20)

      rt = c_sizeof(ch(1:2)) ! Illegal.  Substring is not C interoperable
      rt = c_sizeof(y)       ! Illegal.  Byte is not C interoperable

      rt = c_sizeof(.true.)  ! Illegal.  Only logical(1) is C interoperable
      rt = c_sizeof(logc2)   ! Illegal.  Logical(2) is not C interoperable
      rt = c_sizeof(logc4)   ! Illegal.  Logical(4) is not C interoperable
      rt = c_sizeof(logc8)   ! Illegal.  Logical(8) is not C interoperable
      rt = c_sizeof(logc)    ! Illegal.  Default logical is not C interoperable

      call sub1(arrx, 1, 2)
      end

      subroutine sub0(arr0) ! Assumed-shape arrays
      use, intrinsic :: iso_c_binding
      implicit none
      real(c_float) arr0(1:, :, 10:)
      integer(c_size_t) :: rt

      rt = c_sizeof(arr0)        ! Illegal.  Assumed-shape array
      rt = c_sizeof(4+arr0)      ! allowed
      rt = c_sizeof(arr0*kind(4))! allowed
      end subroutine

      subroutine sub1(arr1, i, j) ! Assumed-size arrays
      use, intrinsic :: iso_c_binding
      implicit none
      integer(c_size_t) :: rt
      real :: arr1(2, *)
      integer i, j
      rt = c_sizeof(arr1)        ! Illegal.  Assumed-sized array

      end subroutine

      subroutine sub2() ! Deferred-shape arrays
      use, intrinsic :: iso_c_binding
      implicit none
      integer(c_size_t) :: rt
      real, allocatable, dimension(:, :, :) :: arr2_1  !Allocatable array
      character, pointer, dimension(:, :) :: arr2_2   !pointer array
      character, target, dimension(5, 10)  :: arr2_2_t

      ALLOCATE(arr2_1(10, -4:5, 20))
      arr2_2 => arr2_2_t

      rt = c_sizeof(arr2_1)         ! Illegal.  Allocatable array
      rt = c_sizeof(arr2_2)         ! Illegal.  Pointer array

      rt = c_sizeof(4+arr2_1)       ! allowed
      rt = c_sizeof(arr2_1*kind(4)) ! allowed

      end subroutine

      subroutine sub3(arr3_1, arr3_2, n) ! character type with len!=1
      use, intrinsic :: iso_c_binding
      implicit none
        integer(c_size_t) rt
        integer i, j, n
        character(len=10), parameter:: str1 ="123456789012345"
        character(len=1) :: str2
        character(len=20) :: str3 = "abc"
        character(len=1) :: str4="a"
        character(*) arr3_1
        character(1) arr3_2(*)
        character(n) arr3_3

        i = 2 ; j =5
        rt = c_sizeof("ab")      ! Illegal.  Char len > 1
        rt = c_sizeof(str1)      ! Illegal str1 has len=10
        rt = c_sizeof(str1(1:2)) ! Illegal
        rt = c_sizeof(str1(3:1))
        rt = c_sizeof(str3)
        rt = c_sizeof(str3(1:2))

        rt = c_sizeof(str1(j:j))
        rt = c_sizeof(str1(i:j))
        rt = c_sizeof(str1(5:j))
        rt = c_sizeof(str2(j:j))
        rt = c_sizeof(str2(i:j))
        rt = c_sizeof(str3(j:j))
        rt = c_sizeof(str3(i:j))
        rt = c_sizeof(str3(i:2))
        rt = c_sizeof(str4(i:i))
        rt = c_sizeof(str4(i:j))

        rt = c_sizeof(arr3_1)
        rt = c_sizeof(arr3_2)
        rt = c_sizeof(arr3_3)
      end subroutine

      subroutine sub4() ! expression and function reference
      use, intrinsic :: iso_c_binding
      implicit none
        interface
          function func1()
            use, intrinsic :: iso_c_binding
            integer(c_long_long), pointer, dimension(:) :: func1
          end function

          function func2()
          use, intrinsic :: iso_c_binding
          integer(c_long_long), pointer, dimension(:, :) :: func2
          end function
        end interface

        integer(c_size_t) :: rt
        logical(c_bool) logc1
        logical(2) logc2
        logical(4) logc4
        logical(8) logc8

        rt = c_sizeof("a" // "bb")

        rt = c_sizeof(2+func1()) ! allowed
        rt = c_sizeof(func1()-3) ! allowed
        rt = c_sizeof(func1())
        rt = c_sizeof(2*func2()) ! allowed
        rt = c_sizeof(func2()-3) ! allowed
        rt = c_sizeof(func2())

        rt = c_sizeof(logc1 .and. logc2)
        rt = c_sizeof(logc2 .and. logc4)
        rt = c_sizeof(logc4 .and. logc8)

      end subroutine

      subroutine sub5() ! pointer, allocatable
      use, intrinsic :: iso_c_binding
      implicit none
        integer(c_size_t) :: rt
        real(c_float), pointer :: pr
        character, allocatable :: pch
        integer(c_int), pointer :: parr1
        integer(c_int), allocatable :: parr2
        character, allocatable :: pcharr(:)
        real(c_float), pointer :: prarr(:, :)
        type, bind(c) :: my_type
          real(c_double) :: d1
          integer(c_int) :: int1
        end type

        type(my_type), pointer :: pdt(:)

        rt = c_sizeof(pr)
        rt = c_sizeof(pch)
        rt = c_sizeof(parr1)
        rt = c_sizeof(parr2)
        rt = c_sizeof(pcharr)
        rt = c_sizeof(prarr)
        rt = c_sizeof(pdt)
      end subroutine

      subroutine sub7() !zero-size
        use, intrinsic :: iso_c_binding
        implicit none
        character(0)  zero_size1
        character(1)  zero_size2(0)
        real(c_float) zero_size3(0)
        integer rt
        rt = c_sizeof(zero_size1)
        rt = c_sizeof(zero_size2)
        rt = c_sizeof(zero_size3)
      end
