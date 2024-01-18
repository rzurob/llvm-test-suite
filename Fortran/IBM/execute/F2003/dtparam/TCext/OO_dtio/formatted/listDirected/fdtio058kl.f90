! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE NAME             : fdtio058kl
!*
!*  PROGRAMMER                 : David Forster (derived from fdtio058a2 by Jim Xia)
!*  DATE                       : 2007-06-20 (original: 12/02/2004)
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : DTIO generics (test the cases DTIO routines are
!                               built in library: static lib)
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

program fdtio058kl
use m
    class (base(8)), pointer :: b1

    type (base(8)), target :: b2

    integer stat
    character(200) err

    b1 => b2

    open (1, file='fdtio058kl.data')

    write (1, *, iostat=stat, iomsg=err) base(8) (100)

    if (stat /= 0) then
        print *, stat, err
        error stop 1_4
    end if

    rewind 1

    read (1, *, iostat=stat, iomsg=err) b1

    if (stat /= 0) then
        print *, stat, err
        error stop 2_4
    end if

    if (.not. allocated (b2%i1)) error stop 3_4

    if (b2%i1 /= 100) error stop 4_4

    close (1, status='delete')
end
