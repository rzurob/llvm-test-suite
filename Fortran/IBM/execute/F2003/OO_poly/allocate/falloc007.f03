! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/26/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : ALLOCATE (allocat-objects' dynamic types are
!                               their declared types if neither type-spec nor
!                               source= present in the ALLOCATE statement)
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

module m
    type base
        class(*), pointer :: data => null()

        contains

        procedure, nopass :: type => baseType
    end type

    type, extends(base) :: child
        integer(8) :: id
        character(20) :: name

        contains

        procedure, nopass :: type => childType
    end type

    type, extends (child) :: gen3
        logical(8) :: set

        contains

        procedure, nopass :: type => gen3Type
    end type

    class (base), allocatable :: b1_m
    class (child), pointer :: c1_m(:)
    class (gen3), allocatable :: g1_m(:,:)

    contains

    character(10) function baseType ()
        baseType = 'base'
    end function

    character(10) function childType ()
        childType = 'child'
    end function

    character(10) function gen3Type ()
        gen3Type = 'gen3'
    end function
end module


program falloc007
use m
    complex(16), pointer :: cx1(:)

    character(15), allocatable :: ch1(:,:)

    allocate (b1_m, c1_m(200:201), g1_m(-10:2, 3:5))

    allocate (cx1(1000:1008))

    allocate (ch1(19:25, 2:10))

    if (b1_m%type() /= 'base') error stop 1_4

    if (c1_m%type() /= 'child') error stop 2_4

    if (g1_m%type() /= 'gen3') error stop 3_4

    if ((size (c1_m) /= 2) .or. (size(g1_m) /= 39)) error stop 4_4

    if ((kind (cx1) /= 16) .or. (size(cx1) /= 9)) error stop 5_4

    if ((kind (ch1) /= kind ('A')) .or. (len (ch1) /= 15) .or. &
        (size (ch1) /= 63)) error stop 6_4
end
