! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/13/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC (FROM, TO)
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                :
!*                               FROM is selector of associate construct
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

    type base
    end type

    type, extends( base) :: child
        class(base), allocatable :: TO(:,:,:)
    end type

    type(child) :: b
    type(child), allocatable :: FROM(:,:,:)

    allocate ( from(3,3,3) )

    associate ( x => b%TO )
            call move_alloc ( from, x )
    end associate

    end
