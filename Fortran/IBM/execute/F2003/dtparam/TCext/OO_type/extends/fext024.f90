!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fext024.f
! %VERIFY: 
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: 
! %END
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*                                                                     
!*  TEST CASE TITLE            :
!*                                                                     
!*  PROGRAMMER                 : Jim Xia
!*  DATE                       : Nov. 07, 2003
!*  ORIGIN                     : IBM Software Solutions Toronto Lab
!*                             :
!*                                                                     
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf90
!*
!*  DESCRIPTION                : type extension (empty type's extending type's
!*                               components)
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
    type base(k1,n)
        integer, kind :: k1
        integer, len :: n
    end type

    type, extends(base) :: child(k2)
        integer, kind :: k2
        integer(k2) :: ic
    end type

    type, extends (child) :: thirdGeneration(k3)
        integer, kind :: k3
        integer(k3) :: i3
    end type

    type, extends (thirdGeneration) :: fourthGeneration(k4)
        integer, kind :: k4
        integer(k4) :: i4
    end type

end module


program fext024

use m
    type(fourthGeneration(4,20,4,8,2)) :: f4_1

    f4_1%ic = 10
    f4_1%thirdGeneration%i3 = 100
    f4_1%i4 = -1

    if (f4_1%ic /= 10) error stop 1_4

    if ((f4_1%ic /= f4_1%thirdGeneration%ic) .or. &
        (f4_1%ic /= f4_1%thirdGeneration%child%ic) .or. &
        (f4_1%ic /= f4_1%child%ic)) error stop 2_4


    if (f4_1%i4 /= -1) error stop 3_4

    if (f4_1%i3 /= 100) error stop 4_4

    if (f4_1%i3 /= f4_1%thirdGeneration%i3) error stop 5_4
end
