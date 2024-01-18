!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : acetnone33
!*
!*  DATE                       : 2006-09-27
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : unltd polymorphic AC in ASSOCIATE construct
!*
!*  REFERENCE                  : Feature Number 289053
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Use unlimited polymorphic AC in an associate construct, verifying the
!*  type of the selector.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program acetnone33

  implicit none

  logical         :: errorFound
  integer, target :: i
  real, target    :: r

  errorFound = .false.

  associate (unk1 => [rainbow(.true.)])
     associate (unk2 => [rainbow(.false.)])

        select type(unk1)
        type is (integer); print *, 'unk1 is correct'
        type is (real);    print *, 'unk1 is incorrect';   errorFound = .true.
        class default;     print *, 'unknown class for 1'; errorFound = .true.
        end select

        select type(unk2)
        type is (real);    print *, 'unk2 is correct'
        type is (integer); print *, 'unk2 is incorrect';   errorFound = .true.
        class default;     print *, 'unknown class for 2'; errorFound = .true.
        end select

     end associate
  end associate

  if (errorFound) stop 2

contains

  function rainbow(sel)
    logical, intent(in)  :: sel
    class(*), pointer :: rainbow
    if (sel) then
       rainbow => i
    else
       rainbow => r
    end if
  end function rainbow

end program acetnone33
