!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fext016.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Nov. 07, 2003
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : derived-type extension (base type and extended
!*                               type both have no components)
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
    type base(k)
        integer, kind :: k
    end type

    type, extends(base) :: child(n)
        integer, len :: n
    end type

    type (base(4)) :: b1_m
    type (child(4,20)) :: c1_m
end module

program fext016
    use m

    type, extends(base) :: secondChild(ks,ns)
        integer, kind :: ks
        integer, len :: ns
    end type

    type (child(4,20)) :: c1
    type (base(4)) :: b1
    type (secondChild(4,4,20)) :: s1

    print *, b1, c1
    print *, b1_m, c1_m
    print *, s1

end
