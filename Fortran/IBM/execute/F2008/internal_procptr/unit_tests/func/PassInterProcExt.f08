!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : February 28, 2011
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
  real, parameter :: PI = 3.14
  integer, dimension(5) :: arr = [1,2,3,4,5]
  character(10) :: string = "HelloWorld"

  type :: base(k, l)
    integer, kind :: k
    integer, len :: l
    integer(k) :: value = 999
    character(l) :: str = "Hello"
  end type

  type(base(4,5)), save :: dt
end module

program PassInterProcExt
  call wrapper()
end program

subroutine wrapper()
  use m

  interface
    subroutine call_sub(proc)
      interface
        subroutine proc()
        end subroutine
      end interface
    end subroutine
  end interface

  call call_sub(sub)

  call call_func(func)

  contains
    subroutine sub() ! Internal procedure sub
      print "(a, f4.2)", "PI is ", PI
      print *, "string = ", string
      print *, "arr = ", arr
      print *, "base = ", dt
    end

    integer function func(a, b) result(rel)
      integer, intent(in) :: a(:), b(:)
      integer :: temp

      temp = 0

      do i=1, size(a), 1
        temp = a(i) + b(i) + temp
      enddo

      rel = temp
    end
end subroutine

subroutine call_sub(proc)
  interface
    subroutine proc()
    end subroutine
  end interface

  procedure(proc), pointer :: ptr
  ptr=>proc

  print *, "Start to call1: "
  call proc() ! Call by internal procedure passed
  call ptr()  ! Call by internal procedure pointer
end

subroutine call_func(proc)
  interface
    integer function proc(a, b) result(rel)
      integer, intent(in) :: a(:), b(:)
    end function
  end interface

  procedure(proc), pointer :: ptr
  integer, dimension(5) :: a, b, c ,d

  ptr=>proc
  a = -2
  b = -1
  c = 33
  d = 55

  print *, "Start to call2: "
  print *, proc(a, b) ! Call by internal procedure passed
  print *, ptr(c, d)  ! Call by internal procedure pointer
end
