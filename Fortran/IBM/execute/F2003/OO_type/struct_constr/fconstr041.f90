!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fconstr041.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/13/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : structure constructor (for types implied by
!*                               IMPLICIT statement)
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
    IMPLICIT type(base) (b), type (child) (c)

    type base
        integer*4 :: id = 0
    end type

    type, extends(base) :: child
        character(20) :: name = ''
    end type

    dimension c1_m(10)
    save b1_m, c1_m
end module

program fconstr041
use m
    IMPLICIT type(base) (b), type (child) (c)

    dimension b1(10)

    allocatable c1(:)
    pointer c2

    b1 = (/(base (i*10), i=0,9)/)

    b1_m = base()

    c1_m = (/(child (name ='c1_m'), i=1, 10)/)

    allocate (c1(5), c2)

    c1 = (/(child(id = i), i=0, 4)/)

    c2 = child (base = base(10), name = 'c2')

    !! verify all the data
    if (b1_m%id /= 0) error stop 1_4

    do i = 1, 10
        if (b1(i)%id /= (i-1)*10) error stop 2_4
    end do

    if ((c2%id /= 10) .or. (c2%name /= 'c2')) error stop 3_4

    do i = 1, 5
        if ((c1(i)%id /= i-1) .or. (c1(i)%name /= '')) error stop 4_4
    end do

    do i = 1, 10
        if ((c1_m(i)%id /= 0) .or. (c1_m(i)%name /= 'c1_m')) error stop 5_4
    end do

    deallocate (c2)
end
