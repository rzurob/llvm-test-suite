! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/struct_constr/fconstr006.f
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
! %GROUP: fconstr006.f
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
!*  DATE                       : Nov. 12, 2003
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*                                                                     
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf90
!*
!*  DESCRIPTION                : structure constructor (allocatable components
!*                               in structure constructor)
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
        integer, kind         :: k1,k2
        integer(k1)           :: id
        real(k2), allocatable :: value(:)
    end type

    type, extends(base) :: child(k3,n1)    ! (4,4,1,20)
        integer, kind             :: k3
        integer, len              :: n1
        character(kind=k3,len=n1) :: name
    end type

    type(base(4,4))  b1_m
    type(child(4,4,1,20)) c1_m

    contains

    subroutine initializeModuleData
        b1_m = base(4,4)(1, (/0.0, 1.0/))

        c1_m = child(4,4,1,20) (2, null(), 'child data c1_m')
    end subroutine
end module

program fconstr006
use m
    interface
        logical function isBaseCorrect (b, intVal, realArray)
        use m
            type (base(4,4)), intent(in) :: b
            integer*4, intent(in) :: intVal
            real*4, intent(in) :: realArray(:)
        end function
    end interface

    real*4, dimension(2) :: t = (/1.0, 10.0/)
    real*4, allocatable :: t1(:), t2(:)

    type(base(4,4)) :: b1
    type(child(4,4,1,20)) :: c1, c2, c3

    allocate (t1(2))

    t1 = (/-1.0, 3.0/)

    b1 = base(4,4) (3, (/10.0, 5.0/))

    c1 = child(4,4,1,20) (4, null(), 'test data c1')
    c2 = child(4,4,1,20) (5, t, 'test data c2')
    c3 = child(4,4,1,20) (6, t1, 'test data c3')

    t1 = (/0.0, 0.0/) !<-- this has no effect on c3
    deallocate (t1) !<-- deallocation of t1 has no effect on c3

    call initializeModuleData

    if (.not. isBaseCorrect (b1, 3, (/10.0, 5.0/))) error stop 1_4

    if (.not. isBaseCorrect (c1%base, 4, t2)) error stop 2_4
    if (allocated(c1%value)) error stop 3_4
    if (c1%name /= 'test data c1') error stop 4_4

    if (.not. isBaseCorrect (c2%base, 5, (/1.0, 10.0/))) error stop 5_4
    if (c2%name /= 'test data c2') error stop 6_4

    if (.not. isBaseCorrect (c3%base, 6, (/-1.0, 3.0/))) error stop 7_4
    if (c3%name /= 'test data c3') error stop 8_4

    if (.not. isBaseCorrect (b1_m, 1, (/0.0, 1.0/))) error stop 9_4

    if (.not. isBaseCorrect (c1_m%base, 2, t2)) error stop 10_4
    if (allocated(c1_m%value)) error stop 11_4
    if (c1_m%name /= 'child data c1_m') error stop 12_4
end

logical function isBaseCorrect (b, intVal, realArray)
use m
    type (base(4,4)), intent(in) :: b
    integer*4, intent(in) :: intVal
    real*4, intent(in) :: realArray(:)

    integer i

    isBaseCorrect = (b%id == intVal)

    if (isBaseCorrect .and. allocated (b%value)) then
        isBaseCorrect = (size(b%value) == size(realArray))

        if (isBaseCorrect) then
            do i = 1, size(realArray)
                if (b%value(i) /= realArray(i)) then
                    isBaseCorrect = .false.
                    exit
                end if
            end do
        end if
    end if
end function
