! GB DTP extension using:
! ftcx_dtp -qk -ql /tstdev/F2003/deferlen/unit_tests/diag/deferlen01.f
! opt variations: -qck -qnok -qnol

!*  ===================================================================
!*
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Diagnostic test
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : There must be ALLOCATABLE or POINTER
!*                               attribute for deferred length character.
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890

implicit none
type A(k1,n1)    ! (4,20)
    integer, kind :: k1
    integer, len  :: n1
   character(:) :: char
end type

character(:) :: char1
character(:), dimension(10, 10)  :: char2
character(:)  char3 /'I am illegal'/
character(:), parameter :: char4 = "A"

end
