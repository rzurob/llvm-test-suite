!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : April 11, 2011
!*
!*  PRIMARY FUNCTIONS TESTED   : Test that reading and writing character coarray to one file
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : No Feature Number
!*
!*  REQUIRED COMPILER OPTIONS  : -qcaf -q64
!*
!*  KEYWORD(S)                 : character, CAF, io
!*
!*  TARGET(S)                  : read/write character coarray to one file
!*
!*  DESCRIPTION:
!*  -----------
!*  The testcase aim to
!*  1. test that reading and writing character coarray to one file
!*  -----------
!*  Fix by JX: 2011-08-04: there is no guarantee (either by Fortran standard or
!   XLF that two files opened by two images using the same name will be the same
!   physical file.  A parallel IO is likely to be supported in future, but
!   that'll require new syntax extending the Open statement.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program charIO002
    implicit none

    integer, parameter :: P = 1, Q = 2

    character (len=1),  save :: singleChar[*]
    character (len=5),  save :: chars5[*]
    character (len=20), save :: chars20[*]

    character (len=1)   :: loSingleChar
    character (len=5)   :: loChars5
    character (len=20)  :: loChars20
    character (50) :: myImageFile

    integer :: me, ne, io_stat, i, m, n, l
    character (len=100) :: err_msg

    me = this_image()
    ne = num_images()

    if (ne < max(P,Q)) error stop 2

    write (myImageFile, '(A,I6.6,A)') 'coarray_char_IO_',me,'.txt'

    ! image P open one file, then write some characters into it.
    if(me == P) then
        singleChar[P] = "P"
        chars5[P] = "11111"
        chars20[P] = repeat('O',20)

        open(UNIT = 100, FILE = myImageFile, STATUS = 'REPLACE', IOSTAT = io_stat, IOMSG = err_msg, ACTION = 'WRITE')
        if ( io_stat /= 0 ) then
            print *, "[11] error when P open ",myImageFile," due to ", err_msg
            error stop 11
        end if

        write(UNIT = 100,fmt=*) singleChar[P]," ",chars5[P]
        write(UNIT = 100,fmt=*) chars20[P]

        close(UNIT = 100, STATUS = 'keep', IOSTAT = io_stat, IOMSG = err_msg)
        if ( io_stat /= 0 ) then
            print *, "[12] error when P close coarray_char_IO.txt due to ", err_msg
            error stop 12
        end if
    end if

    ! image Q read from the file created by image P, then append some data to the end.
    if(me == Q) then

        open(UNIT = 100, FILE = myImageFile, STATUS = 'new', IOSTAT = io_stat, IOMSG = err_msg, ACTION = 'READWRITE')
        if ( io_stat /= 0 ) then
            print *, "[13] error when Q open coarray_char_IO.txt due to ", err_msg
            error stop 13
        end if

        singleChar[Q] = 'Q'
        chars5[Q] = '22222'
        chars20[Q] = repeat('E',20)

        write(UNIT = 100,fmt=*) singleChar[Q], " ", chars5[Q]
        write(UNIT = 100,fmt=*) chars20[Q]

        close(UNIT = 100, STATUS = 'keep', IOSTAT = io_stat, IOMSG = err_msg)
        if ( io_stat /= 0 ) then
            print *, "[15] error when Q close coarray_char_IO.txt due to ", err_msg
            error stop 15
        end if

    end if

    sync all

    ! image P read the data from the file then verify it.
    if(me == P) then

        open(UNIT = 100, FILE = myImageFile, STATUS = 'OLD', IOSTAT = io_stat, IOMSG = err_msg, ACTION = 'READ')
        if ( io_stat /= 0 ) then
            print *, "[16] error when Q open coarray_char_IO.txt due to ", err_msg
            error stop 16
        end if

        read(UNIT = 100,fmt=*) loSingleChar, lochars5
        read(UNIT = 100,fmt=*) lochars20

        if(singleChar[P] /= loSingleChar .OR. chars5[P] /= lochars5 .OR. chars20[P] /= lochars20) then
            error stop 17
        end if

        close(UNIT = 100, STATUS = 'delete', IOSTAT = io_stat, IOMSG = err_msg)
        if ( io_stat /= 0 ) then
            print *, "[19] error when Q close coarray_char_IO.txt due to ", err_msg
            error stop 19
        end if

    else if (me == Q) then
        open(UNIT = 100, FILE = myImageFile, STATUS = 'OLD', IOSTAT = io_stat, IOMSG = err_msg, ACTION = 'READ')
        if ( io_stat /= 0 ) then
            print *, "[16] error when Q open coarray_char_IO.txt due to ", err_msg
            error stop 16
        end if

        read(UNIT = 100,fmt=*) loSingleChar, lochars5
        read(UNIT = 100,fmt=*) lochars20

        if(singleChar[Q] /= loSingleChar .OR. chars5[Q] /= loChars5 .OR. chars20[Q] /= loChars20) then
            error stop 18
        end if

        close(UNIT = 100, STATUS = 'delete', IOSTAT = io_stat, IOMSG = err_msg)
        if ( io_stat /= 0 ) then
            print *, "[19] error when Q close coarray_char_IO.txt due to ", err_msg
            error stop 19
        end if
    end if

end program charIO002
