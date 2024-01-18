! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 10/20/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : structure constructor (unlimited
!                               poly-allocatable array component; use array
!                               section as the data-source; dynamic type is
!                               logical)
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

program fconstr033a4_1
    class (*), allocatable :: x(:,:)

    type base
        class (*), allocatable :: data(:)
    end type

    integer u(2), v(1)

    u = (/0, 0/)

    v = (/-1/)

    allocate (x(0:1,-1:0), source=reshape ((/((mod(i,2) == 0), i=1,4)/), &
                            (/2,2/)))

    !! test the structure constructor in associate construct
    associate (y => base(reshape(x(u, v), (/2/))))
        print *, allocated(y%data), lbound(y%data), ubound(y%data)
    end associate
end
