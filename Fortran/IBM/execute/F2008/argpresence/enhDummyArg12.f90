!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : enhDummyArg10.f
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Ying Zhang
!*  DATE                       : 2011-09-01
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Enhancement to determining dummy argument presence
!*
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  REFERENCE                  : Feature Number 386700
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  KEYWORD(S)                 : 
!*  TARGET(S)                  : 
!*  NUMBER OF TESTS CONDITIONS : 
!*
!*  DESCRIPTION
!*
!*  Argument using functions with side effect
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

type base
integer, pointer :: p!(:)
end type
integer :: counter = 0
type(base) :: x(10)

print *, "before", counter
call sub(x(foo())%p)
print *, "after", counter

contains
subroutine sub(arg)
integer, optional :: arg!(:)
end subroutine

integer function foo()
counter = counter + 1
foo = 1
end 

end 
