! GB DTP extension using:
! ftcx_dtp -qck -ql -qreuse=base /tstdev/OO_tpbnd/specific/ftpbnd515.f
! opt variations: -qnock -qnol -qreuse=none

! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: ftpbnd515.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/24/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : specific type bound (elemental type-bound
!                               function for scalar and arrays)
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
    type base(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        integer(k1)   :: id
    end type

    type, extends(base) :: child1(k2)    ! (20,4,1)
        integer, kind             :: k2
        character(kind=k2,len=n1) :: name

        contains

        procedure, pass (c1) :: compare => compareC1C2
    end type

    type, extends(base) :: child2(k3,n2)    ! (20,4,1,15)
        integer, kind             :: k3
        integer, len              :: n2
        character(kind=k3,len=n2) :: name

        contains

        procedure, pass (c2) :: compare => compareC1C2
    end type

    contains

    elemental logical function compareC1C2 (c1, c2)
        class (child1(*,4,1)), intent(in) :: c1
        class (child2(*,4,1,*)), intent(in) :: c2

        compareC1C2 = ((c1%name == c2%name) .and. (c1%id == c2%id))
    end function
end module

program ftpbnd515
use m
    type (child1(20,4,1)) :: c1(10)
    type (child2(20,4,1,15)) :: c2(2:11)

    logical verifyResult (10)

    c1 = (/(child1(20,4,1) (i, 'test'),i=1,10)/)

    c2 = (/(child2(20,4,1,15) (i, 'test'),i=1,10)/)

    !! check for calls using the elements

    do i = 1, 10
        if (.not. (c1(i)%compare (c2(i+1)))) error stop 1_4

        if (.not. (c2(i+1)%compare (c1(i)))) error stop 2_4
    end do

    !! check if c1 and c2 can be temporaries for each element
    do i = 1, 10
        if (.not. (c1(i)%compare (child2(20,4,1,15) (i, 'test')))) error stop 3_4

        if (.not. (c2(i+1)%compare (child1(20,4,1) (i, 'test')))) error stop 4_4
    end do


    !! check if c2 can be a temp scalar for c1 or c2
    verifyResult = .false.

    verifyResult (2) = .true.

    if (any (c1%compare(child2(20,4,1,15)(2, 'test')) .neqv. verifyResult)) error stop 5_4

    verifyResult = .false.

    verifyResult (5) = .true.

   if (any (c2%compare(child1(20,4,1)(5,'test')) .neqv. verifyResult)) error stop 6_4

    !! check for the array constructor

    if (.not. all (c1%compare ((/(child2(20,4,1,15)(i, 'test'),i=1,10)/)))) error stop 7_4

    if (.not. all (c2%compare ((/(child1(20,4,1)(j-1, 'test'),j=2,11)/)))) error stop 8_4

    !! check for 2 whole arrays

    verifyResult = .false.

    verifyResult = c1%compare (c2)

    if (.not. all (verifyResult)) error stop 9_4


    !! check for scalar with array
    verifyResult = c2%compare (c1(3))

    if ((.not. verifyResult(3)) .or. any (verifyResult(4:)) .or. &
        any (verifyResult(1:2))) error stop 10_4


    !! check for array sections
    if (.not. all (c1(2:5)%compare (c2(3:6)))) error stop 11_4
end
