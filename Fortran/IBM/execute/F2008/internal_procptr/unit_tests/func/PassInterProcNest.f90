!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : March 01, 2011
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
    subroutine sub1(index)
      integer, intent(in) :: index

      print *, sub1_func(func1, index)

      contains
        integer function sub1_func(achar_func, index)
          interface
            character function achar_func(i)
              integer, intent(in) :: i
            end function
          end interface

          character :: c

          c = achar_func(88)

          sub1_func = ichar(c) - index
        end
    end

    character function func1(i)
      integer, intent(in) :: i

      func1 = achar(i)
    end

    subroutine sub2(arr1, arr2)
      integer, intent(inout) :: arr1(:)
      integer, intent(inout) :: arr2(:)

      print *, func2(csub1, csub2, arr1, arr2)

      contains
        subroutine csub1(arr)
          integer, intent(inout) :: arr(:)

          do i=1, size(arr)
            if (arr(i) == 0) then
              arr(i) = 11
            end if
          enddo
        end

        subroutine csub2(arr)
          integer, intent(inout) :: arr(:)

          do i=1, size(arr)
            if (arr(i) == 0) then
              arr(i) = 22
            end if
          enddo
        end
    end

    integer function func2(s1, s2, a1, a2)
      interface
        subroutine s1(arr)
          integer, intent(inout) :: arr(:)
        end subroutine

        subroutine s2(arr)
          integer, intent(inout) :: arr(:)
        end subroutine
      end interface

      integer, intent(inout) :: a1(:)
      integer, intent(inout) :: a2(:)
      integer :: temp

      temp = 0

      call s1(a1) ! -1 2 11
      call s2(a2) ! 22 20 22 -7

      do i=1, size(a1)
        temp = temp + a1(i)
      enddo

      do j=1, size(a2)
        temp = temp + a2(j)
      enddo

      func2 = temp
    end
end module

program PassInterProcNest
  use m

  integer :: arr1(3) = [-1, 2, 0]
  integer :: arr2(4) = [0, 20, 0, -7]

  call sub1(5) ! 83

  call sub2(arr1, arr2) ! 69
end


