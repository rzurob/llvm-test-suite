! GB DTP extension using:
! ftcx_dtp -qck -qnol /tstdev/F2003/mv_Alloc/ptrAssc/polyPntToDTfrm.f
! opt variations: -qnock -ql

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

    type A(k1)    ! (4)
        integer, kind :: k1
        integer(k1)      iA
    end type

    type, extends(A) :: B    ! (4)
        character(:), allocatable :: ch
    end type

    class(A(4)), target, allocatable :: a1(:)
    type(B(4)), target, allocatable :: b1(:)
    class(A(4)), pointer :: p1(:)
    integer i

    allocate(a1(7), source = (/ ( A(4)(i), i = -1, 5 ) /) )
    allocate(b1(5), source = (/ ( B(4)(i, char(i+80)), i = 1, 5 ) /) )

    p1 => b1

    call move_alloc(b1, a1)

    if ( .not. allocated(a1) ) error stop 11
    if ( allocated(b1) ) error stop 12

    if ( .not. associated(p1, a1) ) error stop 21

    select type (p1)
        type is (B(4))
            print *, p1%iA
            print *, (/ (p1(i)%ch, i=1,5) /)
    end select

    end
