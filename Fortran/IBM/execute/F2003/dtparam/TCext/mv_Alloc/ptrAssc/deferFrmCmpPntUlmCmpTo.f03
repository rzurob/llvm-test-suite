! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodeferredlp /tstdev/F2003/mv_Alloc/ptrAssc/deferFrmCmpPntUlmCmpTo.f
! opt variations: -qnok -qnol -qdeferredlp

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/13/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC (FROM, TO)
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : FROM is of type character(:)
!*                               TO is of class(*), component of DT
!*                               pointer is of type character(:), comp of DT
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    character(:, kind=1), pointer :: p1(:)
    character(len=:), allocatable, target :: from(:,:)
end module

program main
use m
    type ::  base(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
        class(*), allocatable :: to (:,:)
        character(len=:, kind=1), pointer :: p2(:,:)
    end type

    class(base(4,20)), allocatable, target :: a

    allocate(a)

    allocate(p1(-10:-1), source = (/ '01','23','45','67','89','02','24','68','13','57' /))

    allocate(from(3,3), source = reshape( p1(-10:-2), (/3,3/)) )

    a%p2 => from

    call move_alloc(from, a%to)

    select type (x => a%to)
        type is (character(*))
            if ( .not. associated(a%p2, x)  ) error stop 21
            print *, x
    end select
end

