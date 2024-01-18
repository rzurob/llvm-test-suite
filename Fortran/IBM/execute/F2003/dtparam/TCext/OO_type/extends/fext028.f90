!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fext028.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Nov. 09, 2003
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : parent type renamed out of module; naming in
!*                               procedure calls
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
        integer(k) :: id
    end type
end module

module m1
use m, oldBase => base
    type, extends(oldBase) :: base(n)
        integer, len :: n
        character(n) :: name
    end type
end module


program fext028
    use m1

    interface
        logical function idMatch (b, intVal)
            use m
            type(base(4)), intent(in) :: b
            integer(4), intent(in) :: intVal
        end function
    end interface

    type (oldBase(4)) :: o1
    type (base(4,20)) :: b1

    b1%oldbase%id = 100
    b1%name = 'newBase'

    if ((b1%id /= 100) .or. (b1%oldbase%id /= 100)) error stop 1_4
    if (b1%name /= 'newBase') error stop 2_4

    o1%id = 16
    if (o1%id /= 16) error stop 3_4

    if (.not. idMatch (o1, 16)) error stop 4_4
    if (.not. idMatch (b1%oldbase, 100)) error stop 5_4
end

logical function idMatch (b, intVal)
    use m
    type(base(4)), intent(in) :: b
    integer(4), intent(in) :: intVal

    idMatch = (b%id .eq. intVal)
end function
