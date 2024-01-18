! GB DTP extension using:
! ftcx_dtp -qk -qnol /tstdev/F2003/deferlen/unit_tests/diag/deferlen12.f
! opt variations: -qck -qnok -ql

!*  ===================================================================
!*
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Diagnostic test
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : The ALLOCATE statement for character
!*                               with deferred length must have type
!*                               spec or SOURCE =
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890
type A(k1)    ! (4)
   integer, kind :: k1
   character(:), allocatable :: char1
end type

type B(k2)    ! (4)
    integer, kind :: k2
   character(:), pointer :: char2
end type

type (A(4)) a1
type (B(4)) b1

allocate(a1%char1)
allocate(b1%char2)

end
