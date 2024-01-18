!*  ===================================================================
!*
!*  PRIMARY FUNCTIONS TESTED   : Diagnostic test
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : The ALLOCATE statement for character
!*                               with deferred length must have type
!*                               spec or SOURCE =
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890
type A
   character(:), allocatable :: char1
end type

type B
   character(:), pointer :: char2
end type

type (A) a1
type (B) b1

allocate(a1%char1)
allocate(b1%char2)

end
