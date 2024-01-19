!*  ===================================================================
!*
!*  PRIMARY FUNCTIONS TESTED   : Unit testing
!*
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  DESCRIPTION                : Testing the concatanation  on
!*                               characters with deferred length.
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890
implicit none
type Province
   character(:), allocatable :: name(:)
end type

type(Province) pro
character(:), allocatable :: canada(:)
character(:), pointer     :: ptr
character(:), target,  allocatable :: result

allocate (character(16)::canada(10))
allocate (pro%name(10), source = canada)

canada(1)  = "Alberta"
canada(2)  = "British Columbia"
canada(3)  = "Saskatchewan"
canada(4)  = "Manitoba"
canada(5)  = "Quebec"
canada(6)  = "Ontario"
canada(7)  = "Newfoundland"
canada(8)  = "Prince Edward"
canada(9)  = "New Brunswick"
canada(10) = "Nova Scotia"

pro%name = canada

allocate(character(len(canada(1)))::result)
result = canada(1)
ptr => result

if (result /= "Alberta") error stop 1
if (ptr    /= "Alberta") error stop 2

deallocate(result)
allocate(character(len(canada(1)) + len(ptr))::result)

result = canada(1)//canada(7)
ptr => result
if (result /= "Alberta         Newfoundland") error stop 3
if (ptr    /= "Alberta         Newfoundland") error stop 4

deallocate(canada, pro%name)

end

