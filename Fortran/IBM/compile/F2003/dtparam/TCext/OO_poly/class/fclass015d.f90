! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/class/fclass015d.f
! SCCS ID Information
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/14/2005
!*
!*  DESCRIPTION                : CLASS keyword (try a declaration stmt as the
!                               branching target)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

!program fclass015d
    type A(k1)    ! (4)
        integer, kind :: k1
        integer(k1)      i
    end type

1    class (A(4)), pointer :: a1

    integer :: i1 = 1

2    i = 10

    print *, i
    i1 = i1 + 1

    if (i1 < 20) goto 1     !<-- illegal braching target

    end
