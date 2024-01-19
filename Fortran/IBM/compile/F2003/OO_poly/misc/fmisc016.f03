! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 10/06/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : miscellaneous items (defect 293002)
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

program fmisc016
    integer, parameter :: const = 10

    integer, pointer :: j
    associate (x => 10)
        allocate (j, stat = x)    !<-- illegal

        print *, x
    end associate

    allocate (j, stat=const)    !<-- illegal
    allocate (j, stat=if1())     !<-- illegal
    allocate (j, stat=10)    !<-- illegal
end

integer function if1()
    if1 = 10
end function

