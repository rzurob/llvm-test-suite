!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fconstr033a1.f
! %VERIFY: fconstr033a1.out:fconstr033a1.vf
! %STDIN:
! %STDOUT: fconstr033a1.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 10/19/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : structure constructor (unlimited
!                               poly-allocatable scalar component; use of
!                               character type and logical type as data-source)
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

program fconstr033a1
    class (*), allocatable :: x1, x2

    type base
        class (*), allocatable :: data
    end type

    allocate (x1, source='xlftest 101')
    allocate (x2, source=(10>1))

    !! test the structure constructors
    associate (y1 => base(x1), y2 => base(x2))
        if ((.not. allocated (y1%data)) .or. (.not. allocated (y2%data))) &
                    error stop 1_4

        !! test y1%data first
        select type (z1 => y1%data)
            type is (character(*))
                print *, z1
            class default
                print *, 'wrong'
        end select

        !! then y2%data
        select type (z2 => y2%data)
            type is (logical)
                print *, z2
            class default
                print *, 'wrong'
        end select
    end associate
end
