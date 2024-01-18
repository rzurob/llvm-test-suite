!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fext017.f
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
!*                               type both have no components, but have
!*                               type-bound procedures)
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
!
! base type and child type are interfaces
!
module m
    type base
        contains
        procedure, nopass :: type => baseType
    end type

    type, extends(base) :: child
        contains
        procedure, nopass :: type => childType
        procedure, nopass :: print => printChild
    end type

    type (base) :: b1_m
    type (child) :: c1_m

    contains

    integer function baseType ()
        baseType = 1
    end function

    integer function childType ()
        childType = 2
    end function

    subroutine printChild
        print *, 'child type'
    end subroutine
end module

program fext017
    use m

    interface
        integer function secondChildType ()
        end function
    end interface

    type, extends(base) :: secondChild
        contains
        procedure, nopass :: type => secondChildType
    end type

    type (child) :: c1
    type (base) :: b1
    type (secondChild) :: s1

    print *, b1, c1
    print *, b1_m, c1_m
    print *, s1

    ! Here is the types: 1 -- base; 2 -- child; 3 -- secondChild
    if (c1%type() /= 2) error stop 1_4
    if (c1_m%type() /= 2) error stop 2_4

    if (b1%type() /= 1) error stop 3_4
    if (b1_m%type() /= 1) error stop 4_4

    if (s1%type() /= 3) error stop 5_4

    ! make sure parent components' typebnd are correct called
    if (c1%base%type() /= 1) error stop 6_4
    if (c1_m%base%type() /= 1) error stop 7_4
    if (s1%base%type() /= 1) error stop 8_4
end

integer function secondChildType ()
    secondChildType = 3
end function
