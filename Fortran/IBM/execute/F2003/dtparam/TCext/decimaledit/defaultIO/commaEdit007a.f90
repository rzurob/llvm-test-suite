! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qnodefaultpv -qdeferredlp -qreuse=self /tstdev/F2003/decimaledit/defaultIO/commaEdit007a.f
! opt variations: -qnock -qnok -qnol -qdefaultpv -qnodeferredlp -qreuse=none

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/01/2006
!*
!*  DESCRIPTION                : DECIMAL EDIT MODE
!                               A modified test case from commaEdit007 to use
!                               inquire statement to determine the file position
!                               for a stream access mode during data reading.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(k1,k2,n1)    ! (4,1,15)
        integer, kind             :: k1,k2
        integer, len              :: n1
        integer(k1)               :: id
        character(kind=k2,len=n1) :: desc

        contains

        procedure ::  readFormattedStream => readFormattedStreamBase
    end type

    type, extends(base) :: child(k3)    ! (4,1,15,8)
        integer, kind :: k3
        complex(k3)   :: cx

        contains

        procedure ::  readFormattedStream => readFormattedStreamChild
    end type

    type node(k4,n2,k5)    ! (4,20,1)
        integer, kind                 :: k4,k5
        integer, len                  :: n2
        class(base(k4,k5,:)), pointer :: data => null()
        type(node(k4,:,k5)), pointer  :: next => null()

        contains

        final :: finalizeNode
    end type

    type listNode(k6,n3,k7)    ! (4,20,1)
        integer, kind                :: k6,k7
        integer, len                 :: n3
        type(node(k6,:,k7)), pointer :: head => null()

        contains

        procedure :: readFormattedStream => readFormattedStreamList
        procedure :: resetList
        final :: finalizeList
    end type

    contains

    recursive subroutine finalizeNode (n)
        type(node(4,*,1)), intent(inout) :: n

        if (associated(n%data)) deallocate(n%data)

        if (associated(n%next)) deallocate(n%next)
    end subroutine

    subroutine resetList (list)
        class (listNode(4,*,1)), intent(out) :: list
    end subroutine

    subroutine finalizeList (list)
        type(listNode(4,*,1)), intent(inout) :: list

        if (associated(list%head)) deallocate (list%head)
    end subroutine


    subroutine readFormattedStreamList (list, unit)
        class (listNode(4,*,1)), intent(out) :: list
        integer, intent(in) :: unit

        type (node(4,:,1)), pointer :: tail => null()

        character(5) :: marker

        integer ipos, stat

        stat = 0

        do while (stat == 0)

            inquire (unit, pos=ipos)

            read(unit, '(a5)', pos=ipos, advance='no', iostat=stat) marker

            if (stat /= 0) exit

            if (.not. associated(list%head)) then
                allocate (node(4,list%n3,1) :: list%head)

                tail => list%head
            else
                allocate (node(4,tail%n2,1) :: tail%next)

                tail => tail%next
            end if

            if (marker == 'base') then
                allocate(base(4,1,15) :: tail%data)

            else if (marker == 'child') then
                allocate(child(4,1,15,8) :: tail%data)
            else
                stop 30
            end if

            call tail%data%readFormattedStream (unit)
        end do
    end subroutine


    subroutine readFormattedStreamBase (b, unit)
        class(base(4,1,*)), intent(out) :: b
        integer, intent(in) :: unit

        integer currentPos, stat

        inquire (unit, pos=currentPos, iostat=stat)

        if (stat /= 0) stop 10

        read (unit, pos=currentPos, fmt='(i5, 1x, a15)', advance='no') b%id, b%desc
    end subroutine

    subroutine readFormattedStreamChild (b, unit)
        class(child(4,1,*,8)), intent(out) :: b
        integer, intent(in) :: unit

        integer currentPos, stat
        character(10) :: decimalMode

        call b%base%readFormattedStream (unit)

        inquire (unit, pos=currentPos, iostat=stat, decimal=decimalMode)

        if (stat /= 0) stop 20

        if (decimalMode == 'COMMA') then
            read (unit, pos=currentPos, fmt='(2g15.5)', advance='no') b%cx
        else
            read (unit, pos=currentPos, fmt='(dc,2g15.5)', advance='no') b%cx
        end if
    end subroutine
end module


program commaEdit007a
use m

    type(listNode(4,20,1)) list
    character(20) :: inFile

    type(node(4,:,1)), pointer :: iterator

    inFile = "commaEdit007a.in  "

    call createData (inFile, 'forward')

    open (3, file=inFile, access='stream', form='formatted', decimal='COMMA', &
        status='old')

    call list%readFormattedStream(3)
    close(3)

    !! verify the data read in

    call printList


    call createData (inFile, 'backward')

    open (3, file=inFile, access='stream', form='formatted', status='old')

    call list%readFormattedStream(3)

    call printList

    contains

    subroutine printList
        iterator => list%head

        do while (associated(iterator))
            select type (x => iterator%data)
                type is (base(4,1,*))
                    print *, x

                type is (child(4,1,*,8))
                    write(*, '(i5, 1x, a, 2f12.3)') x

                class default
                    stop 15
            end select

            iterator => iterator%next
        end do
    end subroutine
end


subroutine createData (file, order)
    use m
    character(*), intent(in) ::  file, order

    type(base(4,1,15)) :: b1(10) = (/(base(4,1,15)(2*i-1, 'base type'), i=1,10)/)
    type(child(4,1,15,8)):: c1(10) = (/(child(4,1,15,8)(2*i, 'child type', cmplx(i, i, 8)), i=1,10)/)

    integer ipos

    open (15, file=file, access='stream', form='formatted', decimal='COMMA')

    if (order == 'forward') then
        do i = 1, 10
            inquire (15, pos=ipos)

            write(15, pos=ipos, fmt='("base ", i5, 1x, a15)', advance='no') &
                    b1(i)

            inquire (15, pos=ipos)

            write(15, pos=ipos, fmt='("child", i5, 1x, a15, 2g15.5)', &
                advance='no') c1(i)
        end do
    else
        do i = 1, 10
            inquire (15, pos=ipos)

            write(15, pos=ipos, fmt='("child", i5, 1x, a15, 2g15.5)', &
                advance='no') c1(i)

            inquire (15, pos=ipos)

            write(15, pos=ipos, fmt='("base ", i5, 1x, a15)', advance='no') &
                    b1(i)
        end do
    end if

    close(15)
end subroutine
