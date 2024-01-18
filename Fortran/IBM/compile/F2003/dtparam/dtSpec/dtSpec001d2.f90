!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/01/2006
!*
!*  DESCRIPTION                : dtparam
!                               Case: diag. test: type parameter declaration in
!                               derived type definition.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program dtSpec001d2
    !! compiler should diagnose the following derived type definition
    type bad
        integer, kind :: k

    end type

    type (bad(4)) :: b1
    class(bad(:)), allocatable :: b2
end
