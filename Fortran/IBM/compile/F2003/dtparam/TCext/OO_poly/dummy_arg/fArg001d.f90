! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg001d.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/10/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (C401 or C482: abstract
!                               nonpoly data can not be declared to be with type
!                               abstract)
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
    type, abstract :: base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)   :: i
    end type

    type, extends (base) :: child(k2,n1)    ! (4,1,20)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name
    end type

    contains

    subroutine abc (b)
        type (base(4)) :: b  !<-- this is illegal
    end subroutine
end module

program fArg001d
end
