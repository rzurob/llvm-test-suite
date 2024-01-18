! GB DTP extension using:
! ftcx_dtp -qk -ql /tstdev/F2003/mv_Alloc/ptrAssc/ulmt4allAssocname.f
! opt variations: -qnok -qnol

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/13/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC (FROM, TO)
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : 1.TO/FROM/pointer are all of type class(*)
!*                               2. TO is a component as a selector associated
!*                                  with a associated name
!*                               3. test association in select type block
!*                               defect 322504
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

    type base(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
        class(*), allocatable :: a
    end type

    type(base(4,20)), target :: b
    class(*), target, allocatable :: c
    class(*), pointer :: p

    allocate(integer :: b%a)
    allocate(base(4,20) :: c)

    p => c

    call move_alloc(c, b%a)

    select type ( x=> b%a )
        type is (base(4,*))
            if ( .not. associated(p, x) ) error stop 21
        class default
            stop 23
    end select

    end
