!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : February 24, 2011
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature Number 303977
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION:
!*  -----------
!*  Now Fortran 2008 allows internal procedures and pointers to such procedures
!*  to be actual arguments.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module m
  contains
    subroutine test_pass_inter_proc1(testArr)
      interface
        subroutine testArr(arr)
          integer, intent(in) :: arr(3)
        end subroutine
      end interface

      integer :: arr1(3) = 2
      integer, dimension(3) :: arr2 = 3
      integer :: arr3(-1:1) = -2

      call testArr(arr1)
      call testArr(arr2)
      call testArr(arr3)
    end

    subroutine test_pass_inter_proc2(testArr)
      interface
        function testArr(low, high)
          integer :: low, high
        end function
      end interface

      print "(f4.1)", testArr(-1, 9)
      print "(f4.1)", testArr(3, 5)
    end

    subroutine test_pass_inter_proc3(testArr)
      interface
        subroutine testArr(x, arr)
          integer :: x
          integer, dimension(x*3) :: arr
        end subroutine
      end interface

      integer, dimension(9) :: arr1
      integer :: arr2(3)

      arr1 = [-1, -2, -3, -4, -5, -6, -7, -8, -9]
      arr2 = 5

      call testArr(3, arr1)
      call testArr(1, arr2)
    end

    subroutine test_pass_inter_proc4(testArr)
      interface
        integer function testArr(arr)
          integer :: arr(1:,:,10:)
        end function
      end interface

      integer, dimension(10,11:20,30) :: arr

      print *, testArr(arr)
    end

    subroutine test_pass_inter_proc5(testArr)
      interface
        subroutine testArr(arr)
          integer :: arr(2, *)
        end subroutine
      end interface

      integer x(3, 2)

      integer :: count = 1

      do i=1,3
        do j=1,2
          x(i,j) = count
          count = count + 1
        enddo
      enddo

      call testArr(x)
    end
end module

program PassInterProcArr
  use m
  call test_pass_inter_proc1(testArr1)

  call test_pass_inter_proc2(testArr2)

  call test_pass_inter_proc3(testArr3)

  call test_pass_inter_proc4(testArr4)

  call test_pass_inter_proc5(testArr5)

  contains
    subroutine testArr1(arr) ! explicit-shape array
      integer, intent(in) :: arr(3)
      print *, arr
    end

    function testArr2(low, high) ! automatic array
      integer :: low, high
      real :: arr(low:5, 3:high)
      integer :: l, u

      l = lbound(arr, 1)
      u = ubound(arr, 2)
      arr = 2

      testArr2 = (6 - l) * (u - 2) * arr(5, 3)
    end

    subroutine testArr3(x, arr) ! adjustable array
      integer :: x
      integer, dimension(x*3) :: arr

      do i=x+1, x*3
        arr(i) = i
      enddo
      print *, arr
    end

    integer function testArr4(arr) ! assumed-shape array
      integer :: arr(1:,:,10:)

      testArr4 = lbound(arr, 1) + lbound(arr, 2) + lbound(arr, 3) + &
                 ubound(arr, 1) + ubound(arr, 2) + ubound(arr, 3)
    end

    subroutine testArr5(arr) ! assumed-size array
      integer :: arr(2, *)

      print *, size(arr, 1)
      print *, arr(:, 1)
      print *, arr(:, 2)
    end subroutine
end
