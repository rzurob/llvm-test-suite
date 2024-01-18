! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/F2003/deferlen/unit_tests/func/deferlen13.f
! opt variations: -qnock -qnok -qnol

!*  ===================================================================
!*
!*  ORIGIN                     : AIX Compiler Development,
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

allocate(result, source = canada(10))

result = canada(1)
ptr => result

if (result /= "Alberta") error stop 1
if (ptr    /= "Alberta") error stop 2

deallocate(result)
allocate(result, source=(canada(1)//"ABCD"))

result = canada(1)//canada(7)(1:4)
ptr => result
if (result /= "Alberta         Newf") error stop 3
if (ptr    /= "Alberta         Newf") error stop 4

deallocate(result, canada, pro%name)

end

