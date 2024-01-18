! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/struct_constr/fconstr038.f
! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fconstr038.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/19/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : structure constructor (module entities used
!*                               across multiple compilation units)
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
    type base(k1,k2,k3)    ! (4,4,4)
        integer, kind            :: k1,k2,k3
        integer(k1)              :: id
        integer(k2), pointer     :: data1 => null()
        logical(k3), allocatable :: data2(:)
    end type

    type, extends (base) :: child(k4,n1)    ! (4,4,4,1,20)
        integer, kind             :: k4
        integer, len              :: n1
        character(kind=k4,len=n1) :: name
    end type
end module

module m1
use m
    type (base(4,4,4)) :: b1_m = base(4,4,4)(1, data2=null())
    type (child(4,4,4,1,20)) :: c1_m = child(4,4,4,1,20) (2, data2=null(), name = 'c1_m')
end module

program fconstr038
use m1

    interface update
        subroutine updateB1_m (i1, i_ptr, i_allo)
        use m1
            integer*4, intent(in) :: i1
            integer*4, pointer, intent(in) :: i_ptr
            logical*4, allocatable :: i_allo(:)
        end subroutine

        subroutine updateC1_m (i1, i_ptr, i_allo, char)
        use m1
            integer*4, intent(in) :: i1
            integer*4, pointer, intent(in) :: i_ptr
            logical*4, allocatable :: i_allo(:)
            character(*), intent(in) :: char
        end subroutine
    end interface

    interface validData
        logical*2 function validB1_m (i1, i_ptr, i_allo)
        use m1
            integer*4, intent(in) :: i1
            integer*4, pointer, intent(in) :: i_ptr
            logical*4, allocatable :: i_allo(:)
        end function

        logical*4 function validC1_m (i1, i_ptr, i_allo, char)
        use m1
            integer*4, intent(in) :: i1
            integer*4, pointer, intent(in) :: i_ptr
            logical*4, allocatable :: i_allo(:)
            character(*), intent(in) :: char
        end function
    end interface

    logical*1 isInitialVal
    integer*4, pointer :: i_ptr, i_rep
    logical*4, allocatable :: l_allo(:)

    if (.not. isInitialVal()) error stop 1_4

    !! update b1_m first
    allocate (i_ptr, l_allo(5))

    i_ptr = -100
    l_allo = (/.true., .true., .false., .false., .true./)

    call update (10, i_ptr, l_allo)

    if (isInitialVal()) error stop 2_4

    i_rep => i_ptr

    if (.not. validData (10, i_rep, l_allo)) error stop 3_4

    !! update c1_m here

    call update (20, null(), l_allo, 'c1_m_updated')

    if (isInitialVal()) error stop 4_4

    i_rep => null()

    if (.not. validData (20, i_rep, l_allo, 'c1_m_updated')) error stop 5_4

    !! let's do a manual verification
    if ((b1_m%id /= 10) .or. (.not. associated(b1_m%data1, i_ptr)) .or. &
        (size (b1_m%data2) /= 5)) error stop 6_4


    if (.not. ALL (b1_m%data2((/1,2,5/)))) error stop 7_4
    if (ANY (b1_m%data2((/3,4/))))  error stop 8_4

    if ((c1_m%id /= 20) .or. associated (c1_m%data1) .or. (c1_m%name /= &
            'c1_m_updated'))  error stop 8_4

end

logical*1 function isInitialVal()
    use m1

    isInitialVal = .true.

    if ((b1_m%id /= 1) .or. associated (b1_m%data1) .or. allocated (b1_m%data2) &
        .or. (c1_m%id /= 2) .or. associated (c1_m%data1) .or. &
        allocated (c1_m%data2) .or. (c1_m%name /='c1_m')) isInitialVal = .false.

end function

subroutine updateB1_m (i1, i_ptr, i_allo)
use m1
    integer*4, intent(in) :: i1
    integer*4, pointer, intent(in) :: i_ptr
    logical*4, allocatable, intent(in) :: i_allo(:)

    b1_m = base(4,4,4)(i1, i_ptr, i_allo)
end subroutine

subroutine updateC1_m (i1, i_ptr, i_allo, char)
use m1
    integer*4, intent(in) :: i1
    integer*4, pointer, intent(in) :: i_ptr
    logical*4, allocatable :: i_allo(:)
    character(*), intent(in) :: char

    c1_m = child(4,4,4,1,20) (i1, i_ptr, i_allo, char)
end subroutine

logical*2 function validB1_m (i1, i_ptr, i_allo)
use m1
    integer*4, intent(in) :: i1
    integer*4, pointer, intent(in) :: i_ptr
    logical*4, allocatable :: i_allo(:)

    validB1_m = (b1_m%id == i1)

    if (validB1_m) then
        if (associated(i_ptr)) then
            validB1_m = associated (b1_m%data1, i_ptr)
        else
            validB1_m = .not. associated (b1_m%data1)
        end if
    end if

    if (validB1_m) then
        if (allocated (b1_m%data2)) validB1_m = all (b1_m%data2 .eqv. i_allo)
    end if
end function

logical*4 function validC1_m (i1, i_ptr, i_allo, char)
use m1
    integer*4, intent(in) :: i1
    integer*4, pointer, intent(in) :: i_ptr
    logical*4, allocatable :: i_allo(:)
    character(*), intent(in) :: char

    validC1_m = (c1_m%id == i1)

    if (validC1_m) then
        if (associated(i_ptr)) then
            validC1_m = associated (c1_m%data1, i_ptr)
        else
            validC1_m = .not. associated (c1_m%data1)
        end if
    end if

    if (validC1_m) then
        if (allocated (c1_m%data2)) validC1_m = all (c1_m%data2 .eqv. i_allo)
    end if

    validC1_m = validC1_m .and. (c1_m%name == char)
end function
