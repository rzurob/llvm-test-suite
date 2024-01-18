!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/23/2005
!*
!*  DESCRIPTION                : allocate statement (an IBM extension?: allow
!                               different length type-parameter in source-expr)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program falloc029
    character(10), pointer :: c1
    integer err

    !! a little relaxation of rule in standard is allowed here: length
    !type-parameter does not match between source-expr and that of c1
    allocate (c1, source='abc 1234567890', stat=err)

    if (err /= 0) error stop 1_4

    if (c1 /= 'abc 123456') error stop 2_4
end
