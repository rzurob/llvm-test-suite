! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/F2003/deferlen/unit_tests/func/deferlen21.f
! opt variations: -qnock -qnok -qnol

!*  ===================================================================
!*
!*  PRIMARY FUNCTIONS TESTED   : Unit testing
!*
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  DESCRIPTION                : Testing the user defined operator on
!*                               characters with deferred length.
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890

module m

type Person(k1,n1)    ! (4,20)
    integer, kind :: k1
    integer, len  :: n1
   sequence
   character(:), pointer :: name
end type

interface operator(+)
      function add(p1, p2)
         import Person
         type(Person(4,*)), intent(in):: p1, p2
         character(len(p1%name) + len(p2%name)) add
      end function
end interface

end module m

use m
type(Person(4,20)) p1, p2
character(17) result
allocate(character(7)  :: p1%name)
allocate(character(10) :: p2%name)

p1%name = 'Kennedy'
p2%name = 'Washington'

result = p1 + p2

if (result /= 'KennedyWashington') error stop 1
end

function add(p1, p2)
   use m
   type(Person(4,*)), intent(in):: p1, p2
   character(len(p1%name) + len(p2%name)) add
   add = p1%name//p2%name
end function