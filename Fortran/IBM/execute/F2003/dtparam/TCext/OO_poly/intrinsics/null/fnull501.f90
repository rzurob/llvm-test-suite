! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/intrinsics/null/fnull501.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 10/27/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : intrinsic function NULL (interact with
!                               same_type_as)
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

program fnull501
    type base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)      id
    end type

    type, extends(base) :: child(k2,n1)    ! (4,1,16)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name
    end type

    class (base(4)), pointer :: b1

    allocate (child(4,1,16) :: b1)

    if (same_type_as (b1, null(b1))) error stop 1_4

    if (.not. same_type_as (base(4)(10), null(b1))) error stop 2_4

    end
