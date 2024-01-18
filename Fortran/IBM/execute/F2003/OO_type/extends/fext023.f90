!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fext023.f
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
!*                               type, the thirdGeneration, both have no
!*                               component)
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
    end type

    type, extends(base) :: child
        integer*4 :: id
    end type

    type, extends(child) :: thirdGeneration
    end type

    type (base) :: b1_m
    type (child) :: c1_m
    type (thirdGeneration) :: t1_m
end module

program fext023
    use m

    type, extends(child) :: newGeneration
    end type

    type (child) :: c1
    type (base) :: b1
    type (thirdGeneration) :: t1
    type (newGeneration) :: n1

    print *, b1, b1_m

    c1_m%id = 1
    c1%id = 2

    t1_m%id = 3
    t1%id = 4

    n1%id = 5

    if (c1_m%id /= 1) error stop 1_4

    if (c1%id /= 2) error stop 2_4

    if (t1_m%child%id /= 3) error stop 3_4

    if ( (t1_m%id /= t1_m%child%id) ) error stop 4_4

    if (t1%child%id /= 4) error stop 5_4

    if ( (t1%id /= t1%child%id) ) error stop 6_4

    if (n1%child%id /= 5) error stop 7_4

    if ( (n1%id /= n1%child%id) ) error stop 8_4

end
