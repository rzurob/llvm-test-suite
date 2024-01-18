!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/17/2005
!*
!*  DESCRIPTION                : miscellaneous item (defect 296753)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program fmisc015a1
    integer, pointer :: i2

    associate (x => 100)
        x = 1                       !<-- illegal
        read (*, *, iostat = x) i1  !<-- illegal

        allocate (i2, stat=x)       !<-- illegal

        do x = 1, 2                 !<-- illegal
            print *, 'abc'
        end do

        associate (y => x)
            y = 2                   !<-- this is illegal too
        end associate
    end associate
end
