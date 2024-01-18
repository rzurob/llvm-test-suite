! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/mv_Alloc/ptrAssc/diffDeclDT4all.f
! opt variations: -ql

! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            : diffDeclDT4all.f 
!*
!*  PROGRAMMER                 : Michelle Zhang 
!*  DATE                       : 06/13/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC (FROM, TO)
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*                              
!*
!*  DRIVER STANZA              : xlf2003
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

    type base(k1)    ! (4) 
        integer, kind :: k1
        integer(k1)      iA
    end type

    type, extends(base) :: child    ! (4) 
    end type

    type, extends(child) :: deepchild    ! (4)
    end type

    type(deepchild(4)), target, allocatable :: from  
    class(child(4)), pointer :: p
    class(base(4)), target, allocatable :: to

    allocate(to, source = child(4) (99) ) 
    allocate(from, source = deepchild(4)(73) )

    p => from

    call move_alloc(from, to)


    select type ( to )
        type is ( deepchild(4) )
            if ( .not. associated(p, to) ) stop 21
            if ( p%iA /= 73 ) stop 23
	class default
	    stop 31
    end select

    end
