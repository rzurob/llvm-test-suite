!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : Argument association with DTP
!*                             :
!*  PROGRAMMER                 : Huiwen Li
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  FUNCTIONAL TESTED          : Dummy argument is assumed-size array
!*                               with auto type parameter
!*
!*  DRIVER STANZA              :
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
  integer :: element(2*d*2)
end type

type(dttype(2)) :: obj1(2, 4)
integer int_val

int_val = 0

do jj = 1, 4
  do ii = 1, 2
    int_val = int_val + 10
    obj1(ii, jj)%element = int_val
  end do
end do

! - pass non-contiguous array
call sub1(obj1(:, 1:4:2), 2)

contains
subroutine sub1(pa, len_tp)
integer len_tp
type(dttype(len_tp)) :: pa(*)

print *, pa(2)%d
print *, ubound(pa(2)%element)
print *, pa(2)%element

print *, pa(4)%d
print *, ubound(pa(4)%element)
print *, pa(4)%element

end subroutine

end

