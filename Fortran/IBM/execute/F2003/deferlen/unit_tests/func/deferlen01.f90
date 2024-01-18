!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : Deferred Character Length
!*
!*  PROGRAMMER                 : James Ren
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Unit testing
!*
!*  DRIVER STANZA              : xlf90/95
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  DESCRIPTION                : Testing the allocatable attributes on  
!*                               characters with deferred length.
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890
implicit none
type Person
   character(:), allocatable :: name
end type

type(Person) p
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

