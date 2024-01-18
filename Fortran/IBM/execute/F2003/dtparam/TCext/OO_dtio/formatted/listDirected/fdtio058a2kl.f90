! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE NAME             : fdtio058a2kl
!*
!*  PROGRAMMER                 : David Forster (derived from fdtio058a2 by Jim Xia)
!*  DATE                       : 2007-06-20
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                :
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

program fdtio058a2kl
use m
    type, extends(base) :: child(lc)
      integer, len :: lc
      character(lc) :: name
    end type

    class (child(8,:)), allocatable :: c1(:)

    integer stat1
    character(200) error

    allocate (child(8,20):: c1(2))

    c1%name = (/'test 1', 'test 2'/)

    open (1, file='fdtio058a2kl.data')

    write (1, *) 'val1=', -100_8
    write (1, *) 'val2=', 100_8

    rewind 1

    read (1, *, iostat = stat1, iomsg = error) c1

    if (stat1 /= 0) then
        print *, stat1, error
        error stop 1_4
    end if

    !! verify the results
    if ((.not. allocated (c1(1)%i1)) .or. (.not. allocated (c1(2)%i1))) &
                error stop 2_4


    if (any (c1%name /= (/'test 1', 'test 2'/))) error stop 3_4

    if ((c1(1)%i1 /= -100) .or. (c1(2)%i1 /= 100)) error stop 4_4

    close(1, status='delete')
end
