!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fmisc008.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/22/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : miscellaneous items (defect 288942)
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

program fmisc008
    complex*16 c11

    logical precision_r4, precision_r8, precision_r6

    c11 = (1.0d0, 2.0d0)

    associate (x => real(c11, kind=8), x1 => real(c11, kind=16), &
               x2 => real (c11, 4), x3 => real (c11), y1 => aimag(c11))

        if ((kind (x) /= 8) .or. (kind(x1) /= 16) .or. (kind(x2) /= 4) .or. &
            (kind(x3) /= 8)) error stop 1_4

        if (kind (y1) /= 8) error stop 2_4

        if (.not. precision_r4 (x2, 1.0e0)) error stop 3_4

        if ((.not. precision_r8 (x, 1.0d0)) .or. &
            (.not. precision_r8(x3, 1.0d0))) error stop 4_4

        if (.not. precision_r6 (x1, 1.0q0)) error stop 5_4

        if (.not. precision_r8 (y1, 2.0d0)) error stop 6_4
    end associate
end
