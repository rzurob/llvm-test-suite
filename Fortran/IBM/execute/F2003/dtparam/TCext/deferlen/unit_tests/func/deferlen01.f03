! GB DTP extension using:
! ftcx_dtp -qk -ql /tstdev/F2003/deferlen/unit_tests/func/deferlen01.f
! opt variations: -qck -qnok -qnol

!*  ===================================================================
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
implicit none
type Person(k1,n1)    ! (4,20)
    integer, kind :: k1
    integer, len  :: n1
   character(:), allocatable :: name
end type

type(Person(4,20)) p
character(:), allocatable  :: char
character(8) char1

! Test 1 - Before allocate the characters

if(allocated(char) .or. allocated(p%name)) error stop 1

! Test 2 - Allocate the characters and make sure allocation
!          status changes

allocate (character(10)::char)
allocate (p%name, source = char1)

if ((.not.allocated(char)) .or. (.not.allocated(p%name))) error stop 2

char = "John"
p%name = "Ken"

if ((char .ne. "John") .or. (p%name .ne. "Ken")) error stop 3

! Test 3 - testing the deallocate

deallocate(char, p%name)
if(allocated(char) .or. allocated(p%name)) error stop 4

end

