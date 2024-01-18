!*  ===================================================================
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
        contains
           character (:) function pfnc ()
           end function pfnc
        end

