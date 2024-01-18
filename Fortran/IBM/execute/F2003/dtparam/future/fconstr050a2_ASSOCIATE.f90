! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/struct_constr/fconstr050a2.f
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
! %GROUP: fconstr050a2.f
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
!*  DATE                       : 10/18/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : structure constructor (array sections used as
!                               the data-source for allocatable components)
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
    type base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)   :: id
    end type

    type, extends(base) :: child(k2,n1)    ! (4,1,20)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name
    end type

    type container1(k3)    ! (4)
        integer, kind               :: k3
        type(base(k3)), allocatable :: data(:)
    end type

    type container2(k4,k5,n2)    ! (4,1,20)
        integer, kind                      :: k4,k5
        integer, len                       :: n2
        type(child(k4,k5,n2)), allocatable :: data(:)
    end type
end module

program fconstr050a2
use m
    type, extends(child) :: gen3(k6,n3)    ! (4,1,20,4,20)
        integer, kind :: k6
        integer, len  :: n3
        double precision d1
    end type

    class (base(4)), allocatable :: b1(:)

    type(child(4,1,20)), allocatable :: c1(:)

    class (child(4,1,20)), allocatable :: c2(:)

    !! test type container1 first
    allocate (b1(-1:0), source=(/gen3(4,1,20,4,20)(1, 'gen3_1', 1.0d0), &
                        gen3(4,1,20,4,20)(2, 'gen3_2', 2.0d1)/))

    associate (x => container1(4)(b1), y => container1(4) (b1(-1:0)))
        if ((.not. allocated (x%data)) .or. (.not. allocated(y%data))) error stop 1_4


        if ((lbound(x%data,1) /= -1) .or. (lbound(y%data,1) /= 1)) error stop 2_4
        if ((ubound(x%data,1) /= 0) .or. (ubound(y%data,1) /= 2)) error stop 3_4

        if (any(y%data%id /= (/1,2/)) .or. any (x%data%id /= (/1,2/)))  &
                    error stop 4_4
    end associate


    !! test type container2
    allocate (c2(0:5), source=(/(gen3(4,1,20,4,20)(i, 'c2_of_6', i*1.1d0), i=0,5)/))

    associate (x => container2(4,1,20) (c2(::2)))
        if (.not. allocated (x%data)) error stop 5_4

        if ((lbound(x%data,1) /= 1) .or. (ubound(x%data,1) /= 3)) error stop 6_4

        if (any (x%data%id /= (/0,2,4/)) .or. any (x%data%name /= 'c2_of_6')) &
                    error stop 7_4
    end associate
end
