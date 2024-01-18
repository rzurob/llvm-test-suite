! GB DTP extension using:
! ftcx_dtp -qnock -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/implicit/fimplct007a.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                :
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

program fimplct007a
    implicit class (base(4,20)) (b), class(child(4,20)) (c)

    type base(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
        contains

        procedure, nopass :: typeID => baseTypeID
    end type

    type, extends(base) :: child    ! (4,20)
        contains

        procedure, nopass :: typeID => childTypeID
    end type

    interface
        integer*4 function baseTypeID ()
        end function

        integer*4 function childTypeID ()
        end function
    end interface

    pointer b1, b2(:), c1, c2(:)
    allocatable b3, b4(:), c3, c4(:)

    if (b3%typeID() /= 1) error stop 1_4
    if (b4%typeID() /= 1) error stop 2_4

    if (c3%typeID () /= 2) error stop 3_4
    if (c4%typeID () /= 2) error stop 4_4

    b1 => null()
    nullify (c1, c2)
    b2 => c2

    if (b1%typeID() /= 1) error stop 5_4
    if (b2%typeID() /= 1) error stop 6_4

    if (c1%typeID() /= 2) error stop 7_4
    if (c2%typeID() /= 2) error stop 8_4
end

integer*4 function baseTypeID ()
    baseTypeID = 1
end function

integer*4 function childTypeID ()
    childTypeID = 2
end function
