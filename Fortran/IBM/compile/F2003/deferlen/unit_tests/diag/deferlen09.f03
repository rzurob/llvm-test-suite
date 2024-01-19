!*  ===================================================================
!*
!*  PRIMARY FUNCTIONS TESTED   : Diagnostic test
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : The actual argument attributes must
!*                               match those specified by an accessible
!*                               explicit interface.
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890
interface
   function fun(x)
      character(4), allocatable :: x
      character (len=4) :: fun
   end function
end interface

character(:), allocatable :: char
character*4 result
allocate(character(4)::char)
char = '1234'

!issue error message here
result = fun(char)
end


