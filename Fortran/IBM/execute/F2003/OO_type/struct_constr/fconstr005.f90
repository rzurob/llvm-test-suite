!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fconstr005.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Nov. 12, 2003
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : structure constructor (explicit initialization
!*                               in structure constructor)
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
    type base
        integer*4 :: i1
        real*4 :: r(2)
    end type

    type, extends(base) :: child
        character(20) :: c2
        integer*8, pointer :: i2
    end type

    type(base) :: b1_m = base(1, (/0.0, 1.0/))
    type(child) :: c1_m = child (2, (/1.0, 2.0/), 'child data c1_m', null())
    integer*8, target :: i_m = 10
end module

program fconstr005
use m
    interface
        logical function isBaseCorrect (b, intVal, realArray)
        use m
            type (base), intent(in) :: b
            integer*4, intent(in) :: intVal
            real*4, intent(in) :: realArray(2)
        end function
    end interface

    integer*8, target :: i = 100
    integer*8, pointer :: p => null()
    real*4, dimension(2) :: t

    type(base) :: b1 = base (3, (/10.0, 5.0/))
    type(child) :: c1, c2, c3

    c1 = child (4, (/-1.0, -10.0/), 'test data c1', i_m)
    c2 = child (5, (/1.0, 5.0/), 'test data c2', i)
    c3 = child (6, 10.0, 'test data c3', p)

    t = (/0.0, 1.0/)

    if (.not. isBaseCorrect(b1_m, 1, t)) error stop 1_4

    if (.not. isBaseCorrect(b1, 3, (/10.0, 5.0/))) error stop 2_4

    if (.not. isBaseCorrect(c1_m%base, 2, (/1.0, 2.0/))) error stop 3_4
    if ((c1_m%c2 /= 'child data c1_m') .or. associated(c1_m%i2)) error stop 4_4

    if (.not. isBaseCorrect(c1%base, 4, (/-1.0, -10.0/))) error stop 5_4
    if ((c1%c2 /= 'test data c1') .or. (c1%i2 /= 10)) error stop 6_4

    if (.not. isBaseCorrect(c2%base, 5, (/1.0, 5.0/))) error stop 7_4
    if ((c2%c2 /= 'test data c2') .or. (c2%i2 /= 100)) error stop 8_4

    if (.not. isBaseCorrect(c3%base, 6, (/10.0, 10.0/))) error stop 9_4
    if ((c3%c2 /= 'test data c3') .or. associated(c3%i2)) error stop 10_4

end

logical function isBaseCorrect (b, intVal, realArray)
use m
    type (base), intent(in) :: b
    integer*4, intent(in) :: intVal
    real*4, intent(in) :: realArray(2)

    isBaseCorrect = ( (b%i1 == intVal) .and. (b%r(1) == realArray(1)) &
                     .and. (b%r(2) == realArray(2)) )
end function
