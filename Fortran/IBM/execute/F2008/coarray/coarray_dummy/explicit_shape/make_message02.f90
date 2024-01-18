! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2010-09-07
!*  ORIGIN                     :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Test character coarray scalar dummy of explicit
!                               length.  The actual could be sting with larger
!                               length parameter.  Test external routine.
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

program make_message02
    implicit none

    interface
        subroutine marker (str)
            character(11), intent(out) :: str[*]
        end subroutine
    end interface

    character(30), save :: message[2,*]

    integer me, i_val
    me = this_image()

    ! the following two procedures can happen concurrently
    call prepare_message(message(12:))
    call marker (message)

    sync all

    ! now verify the data
    read (message(:11),*) i_val

    if (i_val /= me) then
        print *, 'verification of marking failed on image', me
        print *, i_val, 'vs', me
        error stop 1
    end if

    if (message(12:) /= 'done work') then
        print *, 'verification of message body failed on image', me
        print *, message(12:), ' vs ', 'done work'
        error stop 1
    end if

    contains

    subroutine prepare_message (str)
        character(*), intent(out) :: str[*]

        integer :: np, right

        np = num_images()
        if (me == np) then
            right = np
        else
            right = me + 1
        end if

        if (mod (me, 2) == 1) then
            str(:) = 'done work'
            str(:)[right] = 'done work'
        end if
    end subroutine
end

subroutine marker (str)
    implicit none
    character(11), intent(out) :: str[*]

    integer :: me, np, left

    me = this_image()
    np = num_images()

    if (me == 1) then
        left = np
    else
        left = me - 1
    end if

    write (str(:)[left], '(i11)') left
end subroutine
