!*  ===================================================================
!*
!*  FUNCTIONAL TESTED          : Dummy argument is assumed-shape array
!*                               with assumed type parameter
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789

type dttype(d)
  integer, len  :: d
  integer :: element(d)
end type

type(dttype(4)) :: obj1(2, 4)
integer int_val

int_val = 0

do jj = 1, 4
  do ii = 1, 2
    int_val = int_val + 10
    obj1(ii, jj)%element = int_val
  end do
end do

call sub1(obj1)
contains

subroutine sub1(pa)

type(dttype(*)) :: pa(:, :)

do jj = 1, size(pa, 2)
  do ii = 1, size(pa, 1)
    print *, pa(ii, jj)%element
  end do
end do

end subroutine

end
