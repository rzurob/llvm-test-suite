! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/struct_constr/fconstr033a1d.f
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
! %POSTCMD: tcomp fconstr033a1d.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 12/22/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : structure constructor (actually a test case on
!                               select type syntax; select type statement must
!                               be followed by a type-gaurd statement or an end
!                               select statement)
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

program fconstr033a1d
    class (*), allocatable :: x1

    type base(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
        class (*), allocatable :: data
    end type

    allocate (x1, source='xlftest 101')

    associate (y1 => base(4,20)(x1))
        select type (z1 => y1%data)
            print *, 'xyz'      !<-- illegal
        end select
    end associate
end
