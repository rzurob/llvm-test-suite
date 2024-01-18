! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg016d.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/19/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (deferred-shape array
!*                               shall not have bounds info in the declaration)
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
    type base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)   :: id = -1
    end type

    type, extends (base) :: child(k2,n1)    ! (4,1,20)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name = 'default'
    end type

    contains

    subroutine test2 (b)
        class (base(4)), allocatable, intent(inout) :: b(23:)
        type (child(4,1,20)), save :: c1(6) = (/child(4,1,20) (1, 'c1_1'), child(4,1,20)(2, 'c1_2'), &
            child(4,1,20)(3,'c1_3'), child(4,1,20)(4,'c1_4'), child(4,1,20)(5,'c1_5'), child(4,1,20)(6,'c1_6')/)

        if (.not. allocated (b)) then
            allocate (b(10:15), source=c1)
        end if
    end subroutine
end module

program fArg016d
end
