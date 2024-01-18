!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fdtio056a.f
! %VERIFY: fdtio056a.out:fdtio056a.vf
! %STDIN:
! %STDOUT: fdtio056a.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 12/01/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : DTIO generics (handle the inheritance
!                               relationship in DTIO; formatted read)
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
        character(18) :: name

        contains

        procedure :: print => printBase
    end type

    type, extends(base) :: child
        integer(8) :: id

        contains

        procedure :: print => printChild
    end type

    interface read(formatted)
        subroutine formattedRead (dtv, unit, iotype, v_list, iostat, iomsg)
        import base
            class (base), intent(inout) :: dtv
            integer, intent(in) :: unit
            character(*), intent(in) :: iotype
            integer, intent(in) :: v_list(:)
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface

    contains

    subroutine printChild (b)
        class (child), intent(in) :: b

        print *, b%name, b%id
    end subroutine

    subroutine printBase (b)
        class (base), intent(in) :: b

        print *, b%name
    end subroutine
end module


program fdtio056a
use m
    type (child) :: c1, c2(0:2), cc(3)
    class (base), pointer :: b1, b2(:)

    integer stat
    character(200) err

    cc%name = (/'xlftest abc', 'xlftest xyz', 'xlftest 123'/)
    cc%id = (/10, 5, 1/)

    open (1, file='fdtio056a.data')

    write (1, '(3(a18,i5))') (cc(i)%name, cc(i)%id, i=1,3)

    !! test read scalar of nonpoly
    rewind 1

    read (1, *, iostat=stat, iomsg=err) c1

    if (stat /= 0) then
        print *, stat, err
        error stop 1_4
    end if


    !! test read array of nonpoly
    rewind (1)

    read (1, *, iostat=stat, iomsg=err) c2

    if (stat /= 0) then
        print *, stat, err
        error stop 2_4
    end if


    !! verify c1 and c2
    if ((c1%name /= 'xlftest abc') .or. (c1%id /= 10)) error stop 3_4
    if (any (c2%name /= (/'xlftest abc', 'xlftest xyz', 'xlftest 123'/)) .or. &
        any (c2%id /= (/10, 5, 1/)))   error stop 4_4

    allocate (child:: b1, b2(0:2))

    !! test read scalar of poly
    rewind 1

    read (1, *, iostat=stat, iomsg=err) b1

    if (stat /= 0) then
        print *, stat, err
        error stop 5_4
    end if

    call b1%print

    !! test read array of poly
    rewind (1)

    read (1, *, iostat=stat, iomsg=err) b2

    if (stat /= 0) then
        print *, stat, err
        error stop 6_4
    end if

    call b2(0)%print
    call b2(1)%print
    call b2(2)%print

    close (1, status='delete')
end

subroutine formattedRead (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base, child
    class (base), intent(inout) :: dtv
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    read (unit, '(a18)', iostat=iostat, iomsg=iomsg) dtv%name

    if (iostat /= 0) return

    select type (dtv)
        type is (base)

        type is (child)
            call readChildSpecific (dtv, unit, iostat, iomsg)

        class default
            error stop 10_4
    end select
end subroutine


subroutine readChildSpecific (b, unit, iostat, iomsg)
use m
    type (child), intent(inout) :: b
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    read (unit, '(i5)', iostat=iostat, iomsg=iomsg) b%id
end subroutine
