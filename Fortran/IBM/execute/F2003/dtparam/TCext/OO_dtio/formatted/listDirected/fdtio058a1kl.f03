! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-06-20 (original: 12/02/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : DTIO generics (use static library for DTIO;
!                               test using arrays)
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

program fdtio058a1kl
use m
    class (base(8)), pointer :: b1(:)

    type (base(8)), target :: b2 (10)

    integer stat
    character (200) err

    b2 = (/(base(8)(i), i=1, 10)/)

    b1 => b2(::2)

    write (1,*, iostat=stat, iomsg=err) b1

    if (stat /= 0) then
        print *, stat, err
        error stop 1_4
    end if

    b1 => b2(2::2)

    rewind 1

    read (1, *, iostat=stat, iomsg=err) b1

    if (stat /= 0) then
        print *, stat, err
        error stop 2_4
    end if

    !! verify b2
    do i = 1, 10, 2
        if (b2(i)%i1 /= b2(i+1)%i1) error stop 3_4

        if (b2(i)%i1 /= i)  error stop 4_4
    end do

    close (1, status='delete')

end