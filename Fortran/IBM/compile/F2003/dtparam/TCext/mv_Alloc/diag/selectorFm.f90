! GB DTP extension using:
! ftcx_dtp -qk -qnol -qnodefaultpv /tstdev/F2003/mv_Alloc/diag/selectorFm.f
! opt variations: -qnok -ql -qdefaultpv -qreuse=self

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

    type A(k1)    ! (4)
        integer, kind :: k1
    end type

    type base(k2)    ! (4)
        integer, kind             :: k2
        class(*), allocatable :: TO
        class(A(k2)), allocatable :: FROM
    end type

    type(base(4)), pointer :: b

    allocate ( b )

    allocate ( A(4):: b%from )

    select type ( x => b%from )
        type is ( A(4))
            call move_alloc ( x, b%TO)
    end select

    end
