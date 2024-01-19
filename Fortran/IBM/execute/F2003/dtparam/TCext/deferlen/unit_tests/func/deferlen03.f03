! GB DTP extension using:
! ftcx_dtp -qk -qnol /tstdev/F2003/deferlen/unit_tests/func/deferlen03.f
! opt variations: -qck -qnok -ql

!*  ===================================================================
!*
!*  PRIMARY FUNCTIONS TESTED   : Unit testing
!*
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  DESCRIPTION                : Testing the pointer attributes on
!*                               characters with deferred length.
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890
implicit none
type Person(k1)    ! (4)
    integer, kind :: k1
   character(:), pointer :: name
end type

type(Person(4)) p
character(:), pointer  :: char
character(8), target   ::  char1

! Test 1 - Before allocate the characters

if(associated(char) .or. associated(p%name)) error stop 1

! Test 2 - Allocate the characters and make sure allocation
!          status changes

char => char1
p%name => char

if ((.not.associated(char)) .or. (.not.associated(p%name))) error stop 2

char1 = "John"

if ((char .ne. "John") .or. (p%name .ne. "John")) error stop 3

! Test 3 - testing the deallocate

char => null()
p%name => null()

if(associated(char) .or. associated(p%name)) error stop 4

end

