! GB DTP extension using:
! ftcx_dtp -qck -qnol /tstdev/F2003/deferlen/unit_tests/func/deferlen47.f
! opt variations: -qnock -ql

!*  ===================================================================
!*
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Unit testing
!*
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  DESCRIPTION                : Testing the deferred length character
!*                               in function and subroutine call
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890

module m
type :: person(k1)    ! (4)
   integer, kind :: k1
   integer(k1)      age
   character (:), pointer :: info(:,:)
end type person

contains
function GetPersonInfo(p)
   type (person(4)) :: p, GetPersonInfo
   GetPersonInfo%info => p%info
   GetPersonInfo%age = p%age
end function

subroutine SetInfo(p, age, char)
   type (person(4)) :: p
   character(:), target, allocatable :: char(:,:)
   integer age, d1, d2

   d1 = size(char, dim=1)
   d2 = size(char, dim=2)

   allocate (p%info(d1, d2), source = char)
   p%age = age
   p%info => char

end subroutine

end module

use m

type (person(4)) p1, p2
character(:), allocatable, target :: char(:,:)
procedure (type(person(4))), pointer :: ptr=>NULL()

allocate (character(10)::char(1,2))
char(1,1) = 'Larry'
char(1,2) = 'Engineer'

call SetInfo(p1, 30, char)

if (p1%info(1,1) /= 'Larry')    error stop 1
if (p1%info(1,2) /= 'Engineer') error stop 2

ptr =>GetPersonInfo
p2 = ptr(p1)

if (p2%info(1,1) /= 'Larry')    error stop 1
if (p2%info(1,2) /= 'Engineer') error stop 2

end

