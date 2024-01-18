! GB DTP extension using:
! ftcx_dtp -ql -qreuse=none /tstdev/F2003/round/test/roundX8WriteEdit02.f
! opt variations: -qnol -qreuse=base

!#######################################################################
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  PROGRAMMER                 : William Zhang 
!*  DATE                       : 24/07/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : ROUND with different data edit descriptor.
!*                             
!*
!*  DESCRIPTION                : 
!*    test different ROUND mode with different data edit descriptor with
!*    complex(8) component in derived type. 
!*    9.4.1  The modes of a connection to an external file may be changed
!*           by a subsequent OPEN statement that modifies the connection.
!* ===================================================================

  module m

    type base(n1,k1)    ! (20,8) 
       integer, kind :: k1
       integer, len  :: n1
       complex(k1)      w1
    end type

    type, extends(base):: child(n2,k2)    ! (20,8,20,8)
       integer, kind :: k2
       integer, len  :: n2
       complex(k2)      w2 
    end type

  end module  m

  program roundX8WriteEdit02 

    use m

    implicit none
 
    character(18) :: r_mode(6), r_verify
    integer i
    type(child(20,8,20,8)) :: c1

    integer, parameter::unit = 2 

    open(unit, file='roundX8WriteEdit02.out', action='write')

    c1%base%w1 = (1.25005865103755D0, -1.25005865103555D0)
    c1%w2 = (3.14159265358955D0, 2.71828182845755D0)

    r_mode=(/"up               ", "down             ",           &
             "zero             ", "nearest          ",           &
             "processor_defined", "compatible       "/)

    do i =1,6

       write(unit, '(35A,20A)') "rounding mode for complex8 now is " ,r_mode(i)

       write(unit, '(1x, 2f16.13, 1x, 2f22.14)',round=r_mode(i)) &
         & c1%base%w1, c1%w2

       write(unit, '(1x, 2en22.13, 1x, 2es22.13, 1x, 2g22.14, 1x, &
        & 2d22.14, 1x,2e22.14)', round=r_mode(i)) c1%base%w1,     &
        & c1%w2, c1%base%w1, c1%w2, c1%base%w1

       inquire(unit, round=r_verify)

       if(r_verify .ne. "PROCESSOR_DEFINED") then
          error stop 1_4
       endif

    end do

   close(unit)

  end program roundX8WriteEdit02 
