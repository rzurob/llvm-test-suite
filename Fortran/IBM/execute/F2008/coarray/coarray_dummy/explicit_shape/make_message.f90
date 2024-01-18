! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2010-08-25
!*  ORIGIN                     :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : coarray explicit shape dummy. Character type.
!                               Test on the character function returns
!                               concurrently.
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
    implicit none
    contains

    ! this function build the message this way: three strings are concatnated
    ! together to make one message on each image: msg_me + msg_left + msg_right
    function create_message (comsg) result (message)
        character(*), intent(in) :: comsg[*]
        character(:), allocatable :: message

        integer :: me, np, left, right

        me = this_image()
        np = num_images()

        if (me == 1) then
            left = np
        else
            left = me - 1
        end if

        if (me == np) then
            right = 1
        else
            right = me + 1
        end if

        ! now paste three msgs
        message = comsg // comsg[left] // comsg[right]
    end function
end module

program make_message
use m
    implicit none
    character(15), save, codimension[*] :: msg1
    character(30), save, codimension[2,*] :: msg2
    character(:), allocatable :: msg
    character(11) myself

    integer :: me, np

    me = this_image()
    np = num_images()

    ! set up msg1
    write (msg1,'(A3,i12)') 'img', me

    ! set up msg2
    write (myself, *) me

    msg2 = 'Hello world from' // myself

    sync all

    msg = create_message (msg1)

    ! verify msg on all images
    call verify_msg1

    msg = create_message (msg2)

    ! verify msg on all images
    call verify_msg2
    contains

    subroutine verify_msg1
        integer :: left, right
        character(45) local_msg
        if (len(msg) /= len(local_msg)) then
            print *, 'Image:', me, ', msg length should be 45. Actual len =', len(msg)
            error stop 1
        end if

        if (me == 1) then
            left = np
        else
            left = me - 1
        end if

        if (me == np) then
            right = 1
        else
            right = me + 1
        end if

        write (local_msg, '(3(A3,i12))') 'img', me, 'img', left, 'img', right

        if (local_msg /= msg) then
            print *, 'failed to verify msg on image', me
            print *, msg, '|vs|', local_msg
            error stop 1
        end if
    end subroutine

    subroutine verify_msg2
        character(30) local_msg(3)
        character(11) myself, myleft, myright
        integer :: left, right

        if (len(msg) /= 90) then
            print *, 'image', me, ', length of msg should be 90. Actual length =', len(msg)
            error stop 1
        end if

        if (me == 1) then
            left = np
        else
            left = me - 1
        end if

        if (me == np) then
            right = 1
        else
            right = me + 1
        end if

        write (myself,*) me
        write (myleft,*) left
        write (myright,*) right

        local_msg(1) = 'Hello world from' // myself
        local_msg(2) = 'Hello world from' // myleft
        local_msg(3) = 'Hello world from' // myright

        if (msg /=  local_msg(1) // local_msg(2) // local_msg(3)) then
            print *, 'failed to verify msg on image', me
            print *, msg, '|vs|', local_msg(1)//local_msg(2)// local_msg(3)
            error stop 1
        end if
    end subroutine
end
