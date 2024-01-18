!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : charIO001.f
!*
!*  DATE                       : April 11, 2011
!*  ORIGIN                     : Compiler Development, IBM CDL
!*
!*  PRIMARY FUNCTIONS TESTED   : Test that reading character coarray from STDINPUT and writing character coarray to STDOUTPUT
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : No Feature Number
!*
!*  REQUIRED COMPILER OPTIONS  : -qcaf -q64
!*
!*  KEYWORD(S)                 : character, CAF, io
!*
!*  TARGET(S)                  : read/write character coarray
!*
!*  DESCRIPTION:
!*  -----------
!*  The testcase aim to
!*  1. test that reading character coarray from STDINPUT and writing character coarray to STDOUTPUT
!*  -----------
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program charIO001
    implicit none

    integer, parameter :: P = 1, Q = 2

    character (len=1),  save :: singleChar[*]
    character (len=5),  save :: chars5[*]
    character (len=20), save :: chars20[*]

    integer :: me, ne, i, m, n, l

    me = this_image()
    ne = num_images()

    sync all

    if (ne < max(P,Q)) error stop 2

    if(me == P) then
        read (*,*) singleChar[P], singleChar[Q]
        read (*,*) chars5[P], chars5[Q]
    end if

    sync all

    if(me == P) then
        read (*,*) chars20[P], chars20[Q]
    end if

    sync all

    if (me == P) then
        write (*,*) singleChar[P], " ", singleChar[Q]
    else if(me == Q) then
        write (*,*) chars5[P], " ", chars5[Q]
        write (*,*) chars20[P], " ", chars20[Q]
    end if

end program charIO001
