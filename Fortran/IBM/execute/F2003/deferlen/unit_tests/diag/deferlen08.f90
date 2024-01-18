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

       interface
           character (:) function pfnc (n)
           end function pfnc
        end interface

        end

        character (:) function pfnc (n)
           integer :: n
        end function

