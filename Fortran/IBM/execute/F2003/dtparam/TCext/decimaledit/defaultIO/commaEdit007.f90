! GB DTP extension using:
! ftcx_dtp -qk -qnol -qnodefaultpv -qdeferredlp -qreuse=self /tstdev/F2003/decimaledit/defaultIO/commaEdit007.f
! opt variations: -qck -qnok -ql -qdefaultpv -qnodeferredlp -qreuse=none

! SCCS ID Information
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/25/2006
!*
!*  DESCRIPTION                : DECIMAL EDIT MODE
!                               A program that reads data from input file into a
!                               linked-list and then write formatted of the list
!                               in stream-access mode.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(k1,n1,k2)    ! (8,15,4)
        integer, kind :: k1,k2
        integer, len  :: n1
        real(k1)         d1
        character(n1) :: desc
        integer(k2)      i
    end type

    type nodeType(k3,k4)    ! (4,8)
        integer, kind                :: k3,k4
        type(nodeType(k3,k4)), pointer :: next => null()
        type(base(k4,:,k3)), pointer   :: data => null()

        contains

        procedure :: writeStream
    end type

    type listNode(k5,k6)    ! (4,8)
        integer, kind                  :: k5,k6
        type(nodeType(k5,k6)), pointer :: head => null()

        contains

        procedure :: readListUnformatted
    end type

    contains

    subroutine readListUnformatted (list, file)
        class(listNode(4,8)), intent(out) :: list
        character(*), intent(in) :: file

        integer istat

        type (nodeType(4,8)), pointer :: tail => null(), newNode

        istat = 0

        open (10, file=file, form='unformatted', status='old')

        do while (.true.)
            allocate(newNode)
            allocate(base(8,15,4) :: newNode%data)

            read (10, iostat=istat) newNode%data

            if (istat == 0) then    !<-- read is successful
                if (.not. associated(list%head)) then
                    list%head => newNode
                    tail => list%head
                else
                    tail%next => newNode

                    tail => tail%next
                end if
            else
                exit
            end if
        end do

        deallocate (newNode%data)
        deallocate (newNode)
    end subroutine

    recursive subroutine writeStream (node, unit)
        class(nodeType(4,8)), intent(in) :: node
        integer, intent(in) :: unit

        integer :: pos = 1

        character(10) decimalMode

        inquire (unit, decimal=decimalMode)

        if (decimalMode == 'POINT') then
            write (unit, '(dc, d12.5, 3x, a15, i5)', pos=pos) node%data
        else if (decimalMode == 'COMMA') then
            write (unit, '(d12.5, 3x, a15, i5)', pos=pos) node%data
        else
            stop 10
        end if

        pos = pos + 35

        if (associated(node%next)) then
            call node%next%writeStream (unit)
        else
            pos = 1
        end if
    end subroutine
end module


subroutine createData
use m
    type (base(8,:,4)), allocatable :: b1(:)

    open (1, file='test1.in', status='new', form='unformatted')

    allocate (base(8,15,4) :: b1(10))

    b1%d1 = (/(i**2, i=1,10)/)
    b1%i = (/(i, i=10,1,-1)/)

    write (b1%desc, '("square of", i5)') (i, i=1,10)

    do i = 1, 10
        write (1) b1(i)
    end do

    close(1)
end subroutine

program commaEdit007
use m
    type (listNode(4,8)) list

    call createData

    call list%readListUnformatted ("test1.in")

    open (20, file='test1.out', access='stream', form='formatted', &
        decimal='COMMA')

    open (21, file='test2.out', access='stream', form='formatted')


    if (associated(list%head)) then
        call list%head%writeStream(20)

        call list%head%writeStream(21)
    end if
end
