! GB DTP extension using:
! ftcx_dtp -qk -qnol /tstdev/F2003/deferlen/unit_tests/func/deferlen16.f
! opt variations: -qck -qnok -ql

!*  ===================================================================
!*
!*  PRIMARY FUNCTIONS TESTED   : Unit testing
!*
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  DESCRIPTION                : Testing the manipulations on
!*                               characters with deferred length.
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890
implicit none
type Person(k1)    ! (4)
    integer, kind :: k1
   character(:), allocatable :: name
   character(:), allocatable :: address
end type

type(Person(4)) p
character(:), pointer  :: char(:,:)
character(8), pointer  ::  char1
character(8), target   ::  achar(2,2)

allocate (character(40) :: p%address)

char  => achar
char1 => char(1,1)

achar(1,1) = "Jim Khan"
achar(1,2) = "Toronto"
achar(2,1) = "Ontario"
achar(2,2) = "Markham"

allocate (p%name, source = char1)

p%address = char(2,2)//' '//char(2,1)

if(p%name .ne. "Jim Khan") error stop 1

if (p%address .ne. "Markham  Ontario") error stop 2

deallocate(p%name, p%address)

end
