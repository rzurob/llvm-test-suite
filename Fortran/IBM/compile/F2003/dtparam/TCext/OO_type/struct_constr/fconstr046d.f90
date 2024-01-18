! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/struct_constr/fconstr046d.f
! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: dcomp fconstr046d.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/26/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : structure constructor (variables with PROTECTED
!                               attribute shall not be used in the structure
!                               constructor as a data target for pointer
!                               component)
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
    integer*4, protected, target :: i1
end module

program fconstr046d
use m

    type base(k1)    ! (4)
        integer, kind        :: k1
        integer(k1), pointer :: i1
    end type

    type container(k2,n1)    ! (4,20)
        integer, kind :: k2
        integer, len  :: n1
        class (*), pointer :: x
    end type

    type (base(4)) :: b1
    type (container(4,20)) :: co

    co = container(4,20) (i1)     !<-- this is illegal

    b1 = base(4) (i1)      !<-- this is illegal
end
