! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 10/26/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : NULL (basic test that allocatable variable can
!                               be used as the actual arg for NULL())
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

program fnull500
    integer, allocatable :: i1(:)

    allocate (i1(20), source=(/(j, j= 1,20)/))

    call test1 (null(i1))

    contains

    subroutine test1 (x)
        integer, allocatable :: x(:)

        if (allocated (x)) error stop 1_4
    end subroutine

    end
