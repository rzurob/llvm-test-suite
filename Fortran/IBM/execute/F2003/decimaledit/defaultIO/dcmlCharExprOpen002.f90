!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/23/2006
!*
!*  DESCRIPTION                : DECIMAL EDIT MODE (R905)
!                               Test the decimal= specifier in OPEN statement
!                               can be character expression; use deferred char
!                               length and other variable character length; use
!                               inquire statement to verify.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program dcmlCharExprOpen002
    character(:), pointer :: decimalMode1(:)

    allocate (character(5) :: decimalMode1(6))

    decimalMode1(1:2)=(/'COMMA', 'POINT'/)

    open (10, form='formatted', decimal=decimalMode1(1), access='direct', &
            recl=100)

    open (11, form='formatted', decimal=decimalMode1(2), access='stream')

    inquire (10, decimal=decimalMode1(3))
    inquire (11, decimal=decimalMode1(4))

    if (decimalMode1(3) /= 'COMMA') error stop 1_4

    if (decimalMode1(4) /= 'POINT') error stop 2_4

    close (11)

    open (10, decimal=switch(5, 10), ERR=100, ASYNCHRONOUS='YES', file='test1')
    open (12, file='test2', decimal=switch (25, 10), ACTION='READ')

    inquire (10, decimal=decimalMode1(3))
    inquire (12, decimal=decimalMode1(4))

    if (decimalMode1(3) /= 'POINT') error stop 3_4
    if (decimalMode1(4) /= 'COMMA') error stop 4_4

    close(10)
    close(12)

    stop
100 stop 12
    contains

    character(i) function switch (i, j)
        integer, intent(in) :: i, j

        if (i >= j) then
            switch = 'COMMA'
        else
            switch = 'POINT'
        end if
    end function
end
