! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/struct_constr/fconstr022.f
! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fconstr022.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 12/22/2003
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : structure constructor (type conversion is done
!*                               implicitly in the struct_constr)
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
    type base(k1,k2)    ! (4,4)
        integer, kind :: k1,k2
        integer(k1)   :: id
        real(k2)      :: value
    end type


    type, extends(base) :: child(k3,n1)    ! (4,4,1,20)
        integer, kind             :: k3
        integer, len              :: n1
        character(kind=k3,len=n1) :: name
    end type

    type (base(4,4)) :: b1_m = base(4,4) (1.0, 1)     ! type conversion
    type (child(4,4,1,20)) :: c1_m = child(4,4,1,20) ((1.2*2.0), 6/3, 'c1_m')
end module

module m1
use m

    type, extends(child) :: thirdGeneration(k4)    ! (4,4,1,20,1)
        integer, kind :: k4
        logical(k4)   :: isSet
    end type

    type (child(4,4,1,20)) :: c2_m = child(4,4,1,20) (name = 'c2_m', id = 10.4, value &
                            = (1.0, -10.0))

    type (thirdGeneration(4,4,1,20,1)) :: t1_m = thirdGeneration(4,4,1,20,1) (child = child(4,4,1,20) ( &
            base = base(4,4) (value = 1, id = 1.1), name = 't1'//"_m"), &
            isSet = ((1.2 + 2.2) > 3))
end module

program fconstr022
use m1

    interface
        logical function validateThirdGeneration (t, intVal, realVal, charVal, logVal)
        use m1

            type (thirdGeneration(4,4,1,*,1)), intent(in) :: t
            integer*4, intent(in) :: intVal
            real*4, intent(in) :: realVal
            logical*1, intent(in) :: logVal
            character(*), intent(in) :: charVal
        end function
    end interface

    character(10) :: cname = 'data'

    type (thirdGeneration(4,4,1,20,1)) :: t1 = thirdGeneration(4,4,1,20,1) (13.3/13.0, 1, 't1', (1==1.0))
    type (thirdGeneration(4,4,1,20,1)) :: t2

    if (.not. validateThirdGeneration (t1, 1, 1.0, 't1', .true._1)) &
                            error stop 1_4

    t1 = thirdGeneration(4,4,1,20,1) (2, 2, 't1', (1 == 2))

    if (.not. validateThirdGeneration (t1, 2, 2.0, 't1', .false._1)) &
                            error stop 3_4

    t2 = thirdGeneration(4,4,1,20,1) ((1.0+1.8), 2, 't'//'2', (1 == 1.0))

    if (.not. validateThirdGeneration (t2, 2, 2.0, 't2', .true._1)) &
                            error stop 4_4

    t1 = thirdGeneration(4,4,1,20,1) (name = cname, id = 1_8, value = (1.0, 2.0), &
                        isSet = .true._4)

    if (.not. validateThirdGeneration (t1, 1, 1.0, cname, .true._1)) &
                            error stop 5_4

    t1 = thirdGeneration(4,4,1,20,1) (child = child(4,4,1,20) (base = base(4,4) (value = (10.0, 0.0), &
            id = 10.0), name = 't1'//cname), isSet = validateThirdGeneration &
                (t2, 2, 1.0, 't2', .true._1))

    if (.not. validateThirdGeneration (t1, 10, 10.0, 't1data', .false._1)) &
                            error stop 6_4


    t1 = thirdGeneration(4,4,1,20,1) (t1%id + 10, t1%value*0.0, isSet=(.not. t1%isSet), &
                name = t1%name(1:6)//t1%name(3:))

    if (.not. validateThirdGeneration (t1, 20, 0.0, 't1datadata', .true._1)) &
                            error stop 7_4

    ! validate module variables
    if (.not. validateThirdGeneration (t1_m, 1, 1.0, 't1_m', .true._1)) &
                            error stop 11_4

    if ((b1_m%id /= 1) .or. (b1_m%value /= 1.0)) error stop 8_4

    if ((c1_m%id /= 2) .or. (c1_m%value /= 2.0) .or. (c1_m%name /= 'c1_m')) &
                            error stop 9_4

    if ((c2_m%id /= 10) .or. (c2_m%value /= 1.0) .or. (c2_m%name /= 'c2_m')) &
                            error stop 10_4
end

logical function validateThirdGeneration (t, intVal, realVal, charVal, logVal)
use m1
    type (thirdGeneration(4,4,1,*,1)), intent(in) :: t
    integer*4, intent(in) :: intVal
    real*4, intent(in) :: realVal
    logical*1, intent(in) :: logVal
    character(*), intent(in) :: charVal

    validateThirdGeneration = ( (t%id == intVal) .and. (t%value == realVal) &
                    .and. (t%name == charVal) .and. (t%isSet .eqv. logVal))
end function
