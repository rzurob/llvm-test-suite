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
!*    test different ROUND mode using format declaration on different
!*    data edit descriptor. 
!*    9.4.1  The modes of a connection to an external file may be changed
!*           by a subsequent OPEN statement that modifies the connection.
!* ===================================================================

  program roundR4WriteEdit05 

    implicit none

    integer i 
    real*4 w1, w2, w3, w4

    integer, parameter::unit = 2 

    w1 = 1.25005865156034490
    w2 = -1.25005865156034490
    w3 = 3.141592653589034
    w4 = 2.718281828459845

    open(unit, file='roundR4WriteEdit05.out', action='write')

    write(unit, 11) w1, w2 , w3, w4
    write(unit, 12) w1, w2 , w3, w4
    write(unit, 13) w1, w2 , w3, w4
    write(unit, 14) w1, w2 , w3, w4
    write(unit, 15) w1, w2 , w3, w4
    write(unit, 16) w1, w2 , w3, w4

11 format(4x,ru, f7.5, 1x,f8.5,1x,f13.6,1x, f13.6) 
12 format(4x,rd, en13.5, 1x,en13.5,1x,en13.6,1x, en13.6)
13 format(4x,rz, es13.5, 1x,es13.5,1x,es13.6,1x, es13.6)
14 format(4x,rn, g13.6, 1x,g13.6,1x,g13.7,1x, g13.7)
15 format(4x,rp, e13.6, 1x,e13.6,1x,e13.7,1x, e13.7)
16 format(4x,ru, d13.6, 1x, d13.6,1x,d13.7,1x, d13.7)

    close(unit)

  end program roundR4WriteEdit05 
