!*  ===================================================================
!*
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Unit testing
!*
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  DESCRIPTION                : Testing the allocatable attributes on
!*                               characters with deferred length.
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890
type :: student
   character(:), allocatable :: name
end type student

type( student), pointer :: pStu
type( student), target :: tStu

allocate (character(20)::tStu%name)
tStu%name = "Don"

pStu=>tStu
if  (pStu%name /= 'Don') error stop 1

deallocate (tStu%name)

end
