! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 12/14/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : select type (intrinsic types can not appear in
!                               CLASS IS type guard statement)
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program fselTyp500d1
    class (*), pointer :: x

    integer(4), target :: i1

    i1 = 10
    x => i1

    select type (x)
        class is (integer)          !<-- this is illegal

        class is (real)             !<-- illegal
        class is (complex)          !<-- illegal
        class is (character(*))     !<-- illegal
        class is (logical(8))       !<-- illegal

        type is (integer(4))
            print *, x
    end select

end

