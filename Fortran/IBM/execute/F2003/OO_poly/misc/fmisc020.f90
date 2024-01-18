! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 10/19/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : miscellaneous item (defect 294243, TC 1)
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

program fmisc020
    logical, allocatable, target :: x(:,:)

    integer u(2), v(1)

    u = (/0, 0/)

    v = (/-1/)

    allocate (x(0:1,-1:0))

    x=reshape ((/((mod(i,2) == 0), i=1,4)/), (/2,2/))

    associate (y => x (u,v))
    end associate
end

