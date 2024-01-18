!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fext030.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Nov. 10, 2003
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : type extension (2nd generation has no extra
!                               component)
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
    type base(k1)
        integer, kind :: k1
        integer(k1) :: id
    end type

    type, extends(base) :: child(k2,n)
        integer, kind :: k2
        integer, len :: n
    end type

    type, extends(child) :: thirdGeneration(k3)
        integer, kind :: k3
        logical(k3) :: isSet
    end type

end module

program fext030
    use m

    type (thirdGeneration(4,4,20,2)) :: t1
    type (child(4,4,20)) :: c1

    t1%child%base%id = 100
    if (t1%id /= 100) error stop 1_4

    t1%child%id = 10
    if (t1%id /= 10) error stop 2_4

    t1%id = 1
    if (t1%id /= 1) error stop 3_4

    t1%isSet = .true.
    if (.not. t1%isSet) error stop 6_4

    c1%id = 10

    if (c1%id /= 10) error stop 7_4
    if (c1%id /= c1%base%id) error stop 8_4

    t1 = thirdGeneration(4,4,20,2)(1,(1==2))

    if ((t1%id /= 1) .or. t1%isSet) error stop 9_4
end
