!*  ===================================================================
!*
!*                             :
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  FUNCTIONAL TESTED          : Type component is a string whose length
!*                               is based on length type parameter.
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890

type dttype(d)
  integer, len  :: d
  character(d) :: astring
end type

integer num
type(dttype(2+2)) :: obj1
obj1%astring = '12'

call sub1(obj1)

if (obj1%astring .ne. '1234') error stop 1

contains
subroutine sub1(pa)
integer len_tp
type(dttype(*)) :: pa

pa%astring = trim(pa%astring) // '34'	! - ICE. Refer to defect 321810

end subroutine
end
