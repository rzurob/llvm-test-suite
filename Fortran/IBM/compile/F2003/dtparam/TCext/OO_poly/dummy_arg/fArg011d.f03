! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg011d.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/23/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (associate-name assumes
!                               the restrictions of selector)
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
        integer(k1)   :: id

        contains

        procedure :: assgnID => assgnBaseID
    end type

    contains

    subroutine assgnBaseID (b, id)
        class (base(4)), intent(inout) :: b
        integer(4), intent(in) :: id

        b%id = id
    end subroutine

    subroutine test1 (b)
        class (base(4)), intent(in) :: b

        associate (x => b)
            call x%assgnID (10)     !<-- this is illegal
        end associate
    end subroutine
end module

program fArg011d
end
