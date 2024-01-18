! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodefaultpv -qdeferredlp -qreuse=base /tstdev/OO_type/extends/fext035.f
! opt variations: -qck -qnok -qnol -qdefaultpv -qnodeferredlp -qreuse=none

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Nov. 11, 2003
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
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
    type base(k1,o1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: o1
    end type

    type, extends(base) :: child    ! (4,20)
        integer(k1) :: ic

        contains
        procedure :: print => printChild
    end type

    type, extends (child) :: thirdGeneration(k2)    ! (4,20,8)
        integer, kind :: k2
        integer(k2)   :: i3
    end type

    type, extends (thirdGeneration) :: fourthGeneration(k3)    ! (4,20,8,2)
        integer, kind :: k3
        integer(k3)   :: i4
    end type

    type, extends (fourthGeneration) :: fifthGeneration(k4)    ! (4,20,8,2,1)
        integer, kind :: k4
        integer(k4)   :: i5
    end type

    type, extends (fifthGeneration) :: sixthGeneration    ! (4,20,8,2,1)
        logical(k4) :: l6
    end type

    type, extends (sixthGeneration) :: seventhGeneration    ! (4,20,8,2,1)
        logical(k3) :: l7
    end type

    type, extends (seventhGeneration) :: eighthGeneration    ! (4,20,8,2,1)
        logical(k1) :: l8
    end type

    type, extends (eighthGeneration) :: ninthGeneration    ! (4,20,8,2,1)
        real(k1) :: r9
    end type

    type, extends (ninthGeneration) :: tenthGeneration    ! (4,20,8,2,1)
        real(k2) :: r10
    end type

    type, extends (tenthGeneration) :: eleventhGeneration    ! (4,20,8,2,1)
        complex(k1)  :: c11
    end type

    type, extends (eleventhGeneration) :: twelvethGeneration    ! (4,20,8,2,1)
        complex(k2)  :: c12
    end type

    type, extends (twelvethGeneration) :: thirteenthGeneration(o2)    ! (4,20,8,2,1,10)
        integer, len  :: o2
        character(o2) :: c13
    end type

    type, extends (thirteenthGeneration) :: fourteenthGeneration    ! (4,20,8,2,1,10)
    end type

    type, extends (fourteenthGeneration) :: fifteenthGeneration    ! (4,20,8,2,1,10)
        integer(k4), pointer :: i15
    end type

    contains

    subroutine printChild (c)
        class (child(4,*)), intent(in) :: c

        print *, 'ic = ', c%ic
    end subroutine
end module

module m1
use m

    type, extends (fifteenthGeneration) :: sixteenthGeneration    ! (4,20,8,2,1,10)
        type (child(k1,:)), pointer :: c16
    end type

    type, extends (sixteenthGeneration) :: seventeenthGeneration    ! (4,20,8,2,1,10)
        type (child(k1,:)), allocatable :: c17
    end type

    type, extends (seventeenthGeneration) :: eighteenthGeneration(o3)    ! (4,20,8,2,1,10,1)
        integer, len  :: o3
        character(o3) :: c18
    end type

    type, extends (eighteenthGeneration) :: ninteenthGeneration    ! (4,20,8,2,1,10,1)
        contains

        procedure :: print => print19G
    end type

    type, extends (ninteenthGeneration) :: twentiethGeneration    ! (4,20,8,2,1,10,1)
        type (ninteenthGeneration(k1,o1,k2,k3,k4,o2,o3)) :: n19
    end type

    contains

    subroutine print19G (c)
        class (ninteenthGeneration(4,*,8,2,1,*,*)), intent(in) :: c

        print *, 'ninteenthGeneration'
    end subroutine
end module

program fext035

use m1
    type (child(4,20)), target :: c1
    type (child(4,20)) :: c2

    type (fifteenthGeneration(4,20,8,2,1,10)), target :: t15
    type (twentiethGeneration(4,20,8,2,1,10,1)) :: t20
    type (ninteenthGeneration(4,20,8,2,1,10,1)) :: t19

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
    t19 = ninteenthGeneration(4,20,8,2,1,10,1) (eighteenthGeneration = eighteenthGeneration(4,20,8,2,1,10,1)( &
        c18 = 't', seventeenthGeneration = seventeenthGeneration(4,20,8,2,1,10) (c17 = null(),&
        sixteenthGeneration = sixteenthGeneration(4,20,8,2,1,10)(c16 = null(), &
        fifteenthGeneration = fifteenthGeneration(4,20,8,2,1,10)(fourteenthGeneration=fourteenthGeneration(4,20,8,2,1,10)(&
        thirteenthGeneration=thirteenthGeneration(4,20,8,2,1,10)(c13 = 'c13', twelvethGeneration= twelvethGeneration(4,20,8,2,1) (&
        c12 = (1.0, 1.0), eleventhGeneration = eleventhGeneration(4,20,8,2,1)( &
        c11 = (1.0, 1.0), tenthGeneration = tenthGeneration(4,20,8,2,1) (r10=1.0, ninthGeneration = ninthGeneration(4,20,8,2,1)(r9=1.0, eighthGeneration=eighthGeneration(4,20,8,2,1)(&
        l8 = .true., seventhGeneration = seventhGeneration(4,20,8,2,1)(l7 = .false., &
        sixthGeneration = sixthGeneration(4,20,8,2,1)(l6=.true., fifthGeneration=fifthGeneration(4,20,8,2,1)(&
        i5=1, fourthGeneration=fourthGeneration(4,20,8,2)(i4=10,thirdGeneration=thirdGeneration(4,20,8)(&
        i3=1, child=child(4,20)(1))))))))))))), i15 =i1 ) ))))

    if (t19%c18 /= 't') error stop 10_4
    if (allocated (t19%c17) .or. associated (t19%c16)) error stop 11_4

    if (t19%i15 /= 10) error stop 12_4

    if (t19%c13 /= 'c13') error stop 13_4

    if ((.not. t19%l8) .or. t19%l7 .or. (.not. t19%l6)) error stop 14_4

    if ((t19%i5 /= 1) .or. (t19%i4 /= 10) .or. (t19%i3 /= 1)) error stop 15_4

    call t19%print
end
