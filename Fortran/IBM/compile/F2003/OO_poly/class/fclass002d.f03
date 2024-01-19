! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/12/2005
!*
!*  DESCRIPTION                : CLASS keyword (unlimited poly entities in the
!                               array constructor)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program fclass002d
    type base
        integer*4 :: id = 10
    end type

    integer*4, target :: i = 1
    type(base), target :: b1

    class(*), pointer :: x, x1

    x => i
    x1 => b1

    print *, (/x, x1/)      !<-- illegal

end
