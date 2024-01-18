!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/06/2005
!*
!*  DESCRIPTION                : data pointer assignment (C610: component name
!                               is based on the declared type of the data
!                               object)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program fpAssgn002d1

    type p
        integer abc
    end type

    class(*), pointer :: x
    type (p), target :: p1

    x => p1

    print *, x%abc      !! illegal data-ref
end
