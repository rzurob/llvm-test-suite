! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_poly/allocate/falloc501d.f
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
! %POSTCMD: dcomp falloc501d.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 08/25/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : ALLOCATE (source-expr must be conformable to
!                               the allocate object in shape)
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

program falloc501d
    type base(k1,k2)    ! (8,2)
        integer, kind :: k1,k2
        real(k1)      :: r1 = 1.0
        integer(k2)   :: i1 = 1
    end type

    type (base(8,2)), allocatable :: b1(:), b3
    type (base(8,2)) :: b2 (10)

    b2 = base(8,2) (10.0, 10)

    allocate (b1(5), source=(/(b2(j),j=10,6)/))  !<-- this is illegal
end
