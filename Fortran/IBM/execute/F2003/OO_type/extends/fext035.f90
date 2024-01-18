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
! %GROUP: fext035.f
! %VERIFY: fext035.out:fext035.vf
! %STDIN:
! %STDOUT: fext035.out
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
!*  DATE                       : Nov. 11, 2003
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*                                                                     
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf90
!*
!*  DESCRIPTION                : type extension (20 generations in two modules)
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
        integer*4 :: ic

        contains
        procedure :: print => printChild
    end type

    type, extends (child) :: thirdGeneration
        integer*8 :: i3
    end type

    type, extends (thirdGeneration) :: fourthGeneration
        integer*2 :: i4
    end type

    type, extends (fourthGeneration) :: fifthGeneration
        integer*1 :: i5
    end type

    type, extends (fifthGeneration) :: sixthGeneration
        logical*1 :: l6
    end type

    type, extends (sixthGeneration) :: seventhGeneration
        logical*2 :: l7
    end type

    type, extends (seventhGeneration) :: eighthGeneration
        logical*4 :: l8
    end type

    type, extends (eighthGeneration) :: ninthGeneration
        real*4 :: r9
    end type

    type, extends (ninthGeneration) :: tenthGeneration
        real*8 :: r10
    end type

    type, extends (tenthGeneration) :: eleventhGeneration
        complex (4) :: c11
    end type

    type, extends (eleventhGeneration) :: twelvethGeneration
        complex (8) :: c12
    end type

    type, extends (twelvethGeneration) :: thirteenthGeneration
        character(10) :: c13
    end type

    type, extends (thirteenthGeneration) :: fourteenthGeneration
    end type

    type, extends (fourteenthGeneration) :: fifteenthGeneration
        integer*1, pointer :: i15
    end type

    contains

    subroutine printChild (c)
        class (child), intent(in) :: c

        print *, 'ic = ', c%ic
    end subroutine
end module

module m1
use m

    type, extends (fifteenthGeneration) :: sixteenthGeneration
        type (child), pointer :: c16
    end type

    type, extends (sixteenthGeneration) :: seventeenthGeneration
        type (child), allocatable :: c17
    end type

    type, extends (seventeenthGeneration) :: eighteenthGeneration
        character :: c18
    end type

    type, extends (eighteenthGeneration) :: ninteenthGeneration
        contains

        procedure :: print => print19G
    end type

    type, extends (ninteenthGeneration) :: twentiethGeneration
        type (ninteenthGeneration) :: n19
    end type

    contains

    subroutine print19G (c)
        class (ninteenthGeneration), intent(in) :: c

        print *, 'ninteenthGeneration'
    end subroutine
end module

program fext035

use m1
    type (child), target :: c1
    type (child) :: c2

    type (fifteenthGeneration), target :: t15
    type (twentiethGeneration) :: t20
    type (ninteenthGeneration) :: t19

    integer*1, target :: i1 = 10

    ! test all variables
    c1%ic = 20

    c2%ic = 30

    t15%ic = 40
    t15%i3 = 3
    t15%i4 = 5
    t15%i5 = 100
    t15%l6 = .true._1
    t15%l7 = .false._2
    t15%l8 = .true.
    t15%r9 = 1.0
    t15%r10 = 1.0
    t15%c11 = (1.0, 1.0)
    t15%c12 = (2.0, 2.0)
    t15%c13 = 't15'
    t15%i15 => i1

    if (c1%ic /= 20) error stop 1_4

    if (c2%ic /=30) error stop 2_4

    if ((t15%child%ic /= 40) .or. (t15%sixthGeneration%i3 /= 3) .or. &
        (t15%thirteenthGeneration%i4 /= 5) .or. &
        (t15%twelvethGeneration%i5 /= 100)) error stop 3_4


    if ((.not. t15%sixthGeneration%l6) .or. t15%tenthGeneration%l7 .or. &
        (.not. t15%fourteenthGeneration%l8)) error stop 4_4

    if ((t15%fourteenthGeneration%c13 /= 't15') .or. &
        (.not. associated (t15%i15, i1))) error stop 5_4

    call t15%print

    call t20%print

    !! what about structure constructor
    t19 = ninteenthGeneration (eighteenthGeneration = eighteenthGeneration( &
        c18 = 't', seventeenthGeneration = seventeenthGeneration (c17 = null(),&
        sixteenthGeneration = sixteenthGeneration(c16 = null(), &
        fifteenthGeneration = fifteenthGeneration(fourteenthGeneration=fourteenthGeneration(&
        thirteenthGeneration=thirteenthGeneration(c13 = 'c13', twelvethGeneration= twelvethGeneration (&
        c12 = (1.0, 1.0), eleventhGeneration = eleventhGeneration( &
        c11 = (1.0, 1.0), tenthGeneration = tenthGeneration (r10=1.0, ninthGeneration = ninthGeneration(r9=1.0, eighthGeneration=eighthGeneration(&
        l8 = .true., seventhGeneration = seventhGeneration(l7 = .false., &
        sixthGeneration = sixthGeneration(l6=.true., fifthGeneration=fifthGeneration(&
        i5=1, fourthGeneration=fourthGeneration(i4=10,thirdGeneration=thirdGeneration(&
        i3=1, child=child(1))))))))))))), i15 =i1 ) ))))

    if (t19%c18 /= 't') error stop 10_4
    if (allocated (t19%c17) .or. associated (t19%c16)) error stop 11_4

    if (t19%i15 /= 10) error stop 12_4

    if (t19%c13 /= 'c13') error stop 13_4

    if ((.not. t19%l8) .or. t19%l7 .or. (.not. t19%l6)) error stop 14_4

    if ((t19%i5 /= 1) .or. (t19%i4 /= 10) .or. (t19%i3 /= 1)) error stop 15_4

    call t19%print
end
