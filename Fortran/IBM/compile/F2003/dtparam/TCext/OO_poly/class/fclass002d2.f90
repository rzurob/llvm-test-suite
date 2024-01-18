! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/class/fclass002d2.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/25/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : CLASS keyword (C601, the component in data-ref
!                               is based on the declared type of a derived type)
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

program fclass002d2

    class (*), pointer :: x(:)

    type p(k1)    ! (4)
        integer, kind :: k1
        integer(k1)      i
    end type

    type (p(4)), target :: p1(10)

    x => p1

    print *, size(x%i)      !! this is illegal
end
