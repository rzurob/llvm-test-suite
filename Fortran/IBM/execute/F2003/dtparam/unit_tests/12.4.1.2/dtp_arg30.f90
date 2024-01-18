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
!*  DIAGNOSTIC TESTED          : Dummy argument has 'auto' type parameters
!*                               which are for the length of a character
!*                               component and bound for an array component.
!*
!*  DRIVER STANZA              :
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890

type dttype(d, m)
  integer, len  :: d, m
  integer id
  character(d) :: name
  real data(m)
end type

integer name_len, data_size
type(dttype(8+2, 4)) :: obj1
obj1%id = 1
obj1%name= 'fortran_1'
obj1%data = 1.0

name_len = 10
data_size = 4
call sub1(obj1, name_len, data_size)
contains
subroutine sub1(pa, len1, len2)
integer len1, len2
type(dttype(len1, len2)) :: pa

print *, "record id is: ", pa%id
print *, "record name is: ", pa%name
print *, "name length is: ", len(pa%name)
print *, "record size is: ", ubound(pa%data)
print *, pa%data

end subroutine

end
