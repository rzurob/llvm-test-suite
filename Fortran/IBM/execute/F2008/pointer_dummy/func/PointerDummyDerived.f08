!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : June 4, 2011
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

module dt_test
  type dt
    integer :: i
    integer :: j
  end type

  type dtp(k, l)
    integer, kind :: k
    integer, len :: l
    integer(k) :: i
    integer :: j(l)
  end type

  type dtpc(k, l)
    integer, kind :: k
    integer, len :: l
    integer(k) :: i
    character(l) :: c
  end type

  contains
    subroutine sub1(arg)
      type(dt), pointer, intent(in) :: arg

      print *, arg%i
      print *, arg%j
    end subroutine

    subroutine sub2(arg)
      type(dtp(4, 3)), pointer, intent(in) :: arg

      print *, arg%i
      print *, arg%j
    end subroutine

    subroutine sub3(arg1, arg2)
      integer :: i, j
      type(dt), pointer, intent(in) :: arg1
      type(dtp(4, 3)), pointer, intent(in) :: arg2

      print *, arg1%i
      print *, arg1%j
      print *, arg2%i
      print *, arg2%j
    end subroutine

    subroutine sub4(i, j)
      integer, pointer, intent(in) :: i, j

      print *, i
      print *, j
    end subroutine

    integer function func1(arg1, i, arg2)
      type(dtpc(4, 10)), pointer, intent(in) :: arg1, arg2
      integer :: i

      func1 = len_trim(arg1%c) + i + len_trim(arg2%c)
    end
end module dt_test

program t
  use dt_test

  type(dt), target :: var1
  type(dtp(4, 3)), target :: var2
  type(dtpc(4, 10)), target :: var3 = dtpc(4, 10)(229, "HelloWorld")
  integer :: arr(3)

  var1%i = 17
  var1%j = -133

  arr = -5
  var2%i = 19
  var2%j = arr

  call sub1(var1)
  call sub2(var2)
  call sub3(var1, var2)
  call sub4(var1%j, var2%i) ! Component

  print *, "result: ", func1(var3, -4, var3)
end

