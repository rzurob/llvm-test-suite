!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/23/2006
!*
!*  DESCRIPTION                : DECIMAL EDIT MODE
!                               Use more complex character expression form that
!                               is evaluated to comma.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program dcmlCharExprOpen005
    character comma(5)

    comma = (/'c', 'O', 'm', 'm', 'a'/)


    open (1, file='test', decimal=achar(icomma(comma(1)))//&
        achar(icomma(comma(2)))//achar(icomma(comma(3)))//&
        achar(icomma(comma(4)))//achar(icomma(comma(5))))

    write (1, '(f12.2, dp)') 1.24, (1.21, 3.22)

    contains

    function icomma (c)
        character, intent(in) :: c
        icomma = iachar(c)
    end function
    end
