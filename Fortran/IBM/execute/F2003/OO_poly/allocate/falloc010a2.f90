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
! %GROUP: falloc010a2.f
! %VERIFY: falloc010a2.out:falloc010a2.vf
! %STDIN:
! %STDOUT: falloc010a2.out
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
!*  DATE                       : 12/14/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : allocate (poly-allocatable components in
!                               allocate object)
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
        integer(8), allocatable :: ids(:, :)

        contains

        procedure :: print => printBase
    end type

    type, extends(base) :: child
        character(17), allocatable :: names(:,:)

        contains

        procedure :: print => printChild
    end type

    type container
        class (base), allocatable :: data(:)
    end type

    contains

    subroutine printBase (b)
        class (base), intent(in) :: b

        if (allocated (b%ids)) then
            print *, 'shape of ids:', shape(b%ids), '; lbound:', &
                    lbound(b%ids), '; ubound:', ubound(b%ids)
            
            print *, 'values of ids:', b%ids
        else
            print *, 'ids not allocated'
        end if
    end subroutine

    subroutine printChild (b)
        class (child), intent(in) :: b

        print *, 'Child type:'
        call b%base%print

        if (allocated (b%names)) then
            print *, 'shape of names:', shape(b%names), '; lbound:', &
                    lbound(b%names), '; ubound:', ubound(b%names)

            print *, 'values of names:', b%names
        else
            print *, 'names not allocated'
        end if
    end subroutine
end module

program falloc010a2
use m
    class (base), allocatable :: b1(:)
    class (container), allocatable :: co1(:)
    type (container), allocatable :: co2(:)

    type (child) :: c1(2:3)
    integer(8) :: ids1(1,2), ids2(2:3,0:0)
    character(17) :: names1(0:0,2:3), names2(2,1)


    allocate (co2(30:32), source = container (b1))

    if ((allocated (co2(30)%data)) .or. (allocated (co2(31)%data)) .or. &
        (allocated (co2(32)%data))) error stop 1_4


    !! set up values for c1
    ids1(1,1) = 1000
    ids1(1,2) = 1001

    ids2(2,0) = 10000
    ids2(3,0) = 20000

    names1 (0,2) = 'xlf'
    names1 (0,3) = 'test'

    names2 (1,1) = 'xlftest'
    names2 (2,1) = 'testxlf'

    c1(2) = child(ids1, names1)
    c1(3) = child(ids2, names2)

    !! use c1's values for b1
    allocate (b1(0:1), source=c1)

    !! use b1 for co1
    allocate (co1 (0:1), source=container(b1))

    !! verify the results
    print *, 'verifying b1'

    call b1(0)%print
    call b1(1)%print

    print *, ''
    print *, 'verifying co1'
    print *, ''

    if ((.not. allocated (co1(0)%data)) .or. (.not. allocated (co1(1)%data))) &
                error stop 2_4

    call co1(0)%data(0)%print
    call co1(1)%data(1)%print
end
