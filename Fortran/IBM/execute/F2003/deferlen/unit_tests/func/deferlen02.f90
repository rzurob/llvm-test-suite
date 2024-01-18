!*  ===================================================================
!*
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Unit testing
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : Testing the ASSOCIATED intrinsic related
!*                               with characters with deferred length
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890
implicit none
character(:), pointer :: char => null()
character(8), target  :: char1

if (associated(char)) then
   error stop 1
end if

char1 = "12345"
char => char1

if (.not.associated(char)) then
   error stop 2
end if

nullify(char)

if (associated(char)) then
   error stop 3
end if
end

