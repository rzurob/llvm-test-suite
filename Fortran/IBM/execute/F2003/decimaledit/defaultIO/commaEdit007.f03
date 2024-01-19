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
    type base
        real(8) d1
        character(15) :: desc
        integer i
    end type

    type nodeType
        type(nodeType), pointer :: next => null()
        type(base), pointer :: data => null()

        contains

        procedure :: writeStream
    end type

    type listNode
        type (nodeType), pointer :: head => null()

        contains

        procedure :: readListUnformatted
    end type

    contains

    subroutine readListUnformatted (list, file)
        class(listNode), intent(out) :: list
        character(*), intent(in) :: file

        integer istat

        type (nodeType), pointer :: tail => null(), newNode

        istat = 0

        open (10, file=file, form='unformatted', status='old')

        do while (.true.)
            allocate(newNode)
            allocate(newNode%data)

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
        class(nodeType), intent(in) :: node
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
    type (base), allocatable :: b1(:)

    open (1, file='test1.in', status='new', form='unformatted')

    allocate (b1(10))

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
    type (listNode) list

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
