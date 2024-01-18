! GB DTP extension using:
! ftcx_dtp -ql -qreuse=base /tstdev/OO_tpbnd/specific/ftpbnd515a2_1.f
! opt variations: -qck -qnol -qreuse=none

!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Jim Xia
!*  DATE                       : 04/13/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : specific type bound (elemental binding calls;
!                               use array constructors as the actual argument)
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

    type, extends(base) :: child1    ! (20,4)
        character(n1) :: name

        contains

        procedure, pass (c1) :: compare => compareC1C2
    end type

    type, extends(base) :: child2(n2)    ! (20,4,15)
        integer, len  :: n2
        character(n2) :: name

        contains

        procedure, pass (c2) :: compare => compareC1C2
    end type

    contains

    elemental logical function compareC1C2 (c1, c2)
        class (child1(*,4)), intent(in) :: c1
        class (child2(*,4,*)), intent(in) :: c2

        compareC1C2 = ((c1%name == c2%name) .and. (c1%id == c2%id))
    end function
end module

program ftpbnd515a2_1
use m
    type (child1(20,4)) :: c1(10)
    type (child2(20,4,15)) :: c2(2:11)

    c1 = (/(child1(20,4) (i, 'test'),i=1,10)/)

    c2 = (/(child2(20,4,15) (i, 'test'),i=1,10)/)

    !! check for the array constructor
    print *, c2%compare ((/(child1(20,4)(i, 'test'), i=1,10)/))
    print *, all(c2%compare ((/(child1(20,4)(i, 'test'), i=1,10)/)))

    if (.not. all (c2%compare ((/(child1(20,4)(i, 'test'), i=1,10)/)))) error stop 1_4

    !! WE'RE TESTING THAT THE NEXT STATEMENT FAILS
    if (all (c2%compare ((/(child1(20,4)(i, 'test'), i=1,10)/)))) error stop 2_4
end
