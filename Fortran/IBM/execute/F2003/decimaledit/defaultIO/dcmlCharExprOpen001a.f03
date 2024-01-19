! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/12/2006
!*
!*  DESCRIPTION                : DECIMAL EDIT MODE
!                               Test that the default  vlaue of "POINT" is
!                               applied if the char-expr evalued to an invalid
!                               value; also the previous value is not changed if
!                               the expr evaluates to an invalid value.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program dcmlCharExprOpen001a
    character(10) modeExpr, modeVar

    modeExpr = 'COMA'

    open (1, file='test', decimal=modeExpr, iostat=istat)

    if (istat == 0) error stop 1_4

    inquire (1, decimal=modeVar)

    if (modeVar /= 'POINT') error stop 2_4

    write (1, '(EN15.4)') 5.23d11, 1.33d134, 1.0e22

    modeExpr = 'COMMAND'

    open (1, decimal=modeExpr(1:5), iostat=istat)

    if (istat /= 0) error stop 3_4

    write (1, *, decimal='POINT') 'test 2'

    write (1, '(ES15.4 )') 5.23d11, 1.33d134, 1.0e22

    modeExpr = 'POINTER'

    open (1, decimal=modeExpr, iostat=istat)

    if (istat == 0) error stop 4_4

    inquire (1, decimal=modeVar)

    if (modeVar /= 'COMMA') error stop 5_4

    write (1, *, decimal='POINT') 'test3'

    write (1, '(g12.3)') 1.2, 3.4
    end
