!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/21/2006
!*
!*  DESCRIPTION                : DECIMAL EDIT MODE
!                               Test the use of DECIMAL= specifier together with
!                               POSITION= in an OPEN statement.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

    character(10) c1, decMode, pos
    integer i1
    real(8) r1
    complex(8) cx

    logical(4), external :: precision_r4, precision_x8

    open (1, file='test', status='new', position='rewind')

    inquire (1, decimal=decMode, position=pos)

    if ((decMode /= 'POINT') .or. (pos /= 'REWIND')) error stop 1_4

    write (1, *, decimal='COmma') 'abc', cmplx(2.2, 1.1), 10, 3.1

    write (1, '(DP, SP, I5)', decimal='COMMA') 105

    rewind (1)

    open (1, position='rewind', status='old', decimal='coMMA')

    inquire (1, decimal=decMode, position=pos)

    if ((decMode /= 'COMMA') .or. (pos /= 'REWIND')) error stop 2_4

    read (1, *) c1, cx, i1, r1

    inquire (1, decimal=decMode, position=pos)

    if ((decMode /= 'COMMA') .or. (pos /= 'ASIS')) error stop 3_4

    if (c1 /= 'abc') error stop 4_4

    if (i1 /= 10) error stop 5_4

    if (.not. precision_r4 (3.1, real(r1,4))) error stop 6_4

    if (.not. precision_x8 (cmplx(cx, kind=4), (2.2,1.1))) error stop 7_4
    end
