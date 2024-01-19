! GB DTP extension using:
! ftcx_dtp -qk -ql /tstdev/F2003/deferlen/unit_tests/func/deferlen14.f
! opt variations: -qck -qnok -qnol

!*  ===================================================================
!*
!*  PRIMARY FUNCTIONS TESTED   : Unit testing
!*
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  DESCRIPTION                : Testing the substrings operation on
!*                               characters with deferred length.
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890
implicit none
type Province(k1,n1)    ! (4,20)
    integer, kind :: k1
    integer, len  :: n1
   character(:), allocatable :: name(:)
end type

type(Province(4,20)) pro
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

allocate(character(5)::result)
result = canada(1)(1:2)//" "//canada(2)(1:1)//canada(2)(9:9)
ptr => result
if (result /= "Al BC") error stop 1
if (ptr    /= "Al BC") error stop 2

deallocate(result)
allocate(character(len(canada(10)))::result)

result = canada(6)(1:8)//canada(5)(1:7)
ptr => result
if (result /= "Ontario Quebec") error stop 3
if (ptr    /= "Ontario Quebec") error stop 4

deallocate(canada, pro%name)

end

