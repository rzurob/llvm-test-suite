! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/13/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC (FROM, TO)
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : 1.TO and pointer are of poly type of parent
!*                               2.FROM is of non-poly type of child
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

    type base
        integer iA
    end type

    type, extends(base) :: child
    end type

    type, extends(child) :: deepchild
    end type

    type(deepchild), target, allocatable :: from
    class(child), pointer :: p
    class(base), target, allocatable :: to

    allocate(to, source = child (99) )
    allocate(from, source = deepchild(73) )

    p => from

    call move_alloc(from, to)


    select type ( to )
        type is ( deepchild )
            if ( .not. associated(p, to) ) stop 21
            if ( p%iA /= 73 ) stop 23
	class default
	    stop 31
    end select

    end
