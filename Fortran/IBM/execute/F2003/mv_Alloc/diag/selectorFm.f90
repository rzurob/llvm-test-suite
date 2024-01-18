! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/13/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC (FROM, TO)
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                :
!*                               FROM is selector of select type
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

    type A
    end type

    type base
        class(*), allocatable :: TO
        class(A), allocatable :: FROM
    end type

    type(base), pointer :: b

    allocate ( b )

    allocate ( A:: b%from )

    select type ( x => b%from )
        type is ( A)
            call move_alloc ( x, b%TO)
    end select

    end
