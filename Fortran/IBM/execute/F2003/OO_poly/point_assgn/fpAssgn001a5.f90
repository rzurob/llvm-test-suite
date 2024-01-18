!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fpAssgn001a5.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/17/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : data pointer assignment (test of associated();
!*                               use unlimited poly-pointer assgined to array
!*                               sections)
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

program fpAssgn001a5
    class(*), pointer :: x(:), x1(:), x2(:), x3(:)

    integer*4, target :: i1 (2:10)

    i1 = (/(i, i = 2,10)/)

    x => i1(2:3:2)

    if ((.not. associated (x, i1(2:3:2))) .or. (.not. associated  &
        (x, i1(2:3:4))) .or. (.not. associated (x, i1(2:2)))) error stop 1_4

    x1 => i1 (::6)

    x2 => i1 (::2)

    x3 => x2 (::3)

    if ((.not. associated (x1, x3)) .or. (.not. associated (x3, i1(::6)))) &
                error stop 2_4

end
