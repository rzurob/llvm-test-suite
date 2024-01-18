!*********************************************************************
!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : February 13, 2004
!*
!*  REFERENCE                  : Feature 239812
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : This test case will verify that a single array variable inside of
!*                               BIND(C) common block with a binding label is interoperable
!*                               with a C variable that is not in a structure.
!*
!*                               Data type being tested: character
!*
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012


program fxcmn_blk042
	use iso_c_binding
        implicit none

	character  :: char1(16)

! ----------------------------------------------------------------------------
!      COMMON block with one variable in it;
!      BIND(C) statement with a binding label in mixed characters
! ----------------------------------------------------------------------------
        common /blk1/    char1
        bind(c, name='Bnd_Lbl15') ::       /blk1/


        !*** Default char array
        !-- check compatibility with C for some escape sequences (such as \f, \t, \s, \b)
        !-- check single quotation and double quotation
        !-- check black spaces
        !-- check UPPER and lower case letters as well as some other characters (such as ! and .)
        !-- The following will output:          I'm a "TrEe"!

        char1  =       (/'\f','I','''','m',' ','a','\t','\"',"T",'r','E','e',"""","\.",'\b',"!"/)


! ----------------------------------------------------------------------------
!      1) Check values before passing to C sub-program
!      2) Pass values into C sub-program
!      3) Check values passed back from C sub-program after modifications
! ----------------------------------------------------------------------------

        if ( char1(1)  .ne.    '\f'    )                        error stop 5
        if ( char1(2)  .ne.    'I'     )                        error stop 6
        if ( char1(3)  .ne.    ''''    )                        error stop 7
        if ( char1(4)  .ne.    'm'     )                        error stop 8
        if ( char1(5)  .ne.    ' '     )                        error stop 9
        if ( char1(6)  .ne.    'a'     )                        error stop 10
        if ( char1(7)  .ne.    '\t'    )                        error stop 11
        if ( char1(8)  .ne.    """"    )                        error stop 12
        if ( char1(9)  .ne.    'T'     )                        error stop 13
        if ( char1(10) .ne.    'r'     )                        error stop 14
        if ( char1(11) .ne.    'E'     )                        error stop 15
        if ( char1(12) .ne.    'e'     )                        error stop 16
        if ( char1(13) .ne.    '\"'    )                        error stop 17
        if ( char1(14) .ne.    '\.'    )                        error stop 18
        if ( char1(15) .ne.    '\b'    )                        error stop 19
        if ( char1(16) .ne.    '!'     )                        error stop 20

	!***  Call to C subprogram
	CALL CSUB1()

        if ( char1(1)   .ne.    '\f'    )                       error stop 25
        if ( char1(2)   .ne.    'N'     )                       error stop 26
        if ( char1(3)   .ne.    'o'     )                       error stop 27
        if ( char1(4)   .ne.    ','     )                       error stop 28
        if ( char1(5)   .ne.    ' '     )                       error stop 29
        if ( char1(6)   .ne.    'I'     )                       error stop 30
        if ( char1(7)   .ne.    '\''    )                       error stop 31
        if ( char1(8)   .ne.    "m"     )                       error stop 32
        if ( char1(9)   .ne.    ' '     )                       error stop 33
        if ( char1(10)  .ne.    'a'     )                       error stop 34
        if ( char1(11)  .ne.    ' '     )                       error stop 35
        if ( char1(12)  .ne.    'd'     )                       error stop 36
        if ( char1(13)  .ne.    'o'     )                       error stop 37
        if ( char1(14)  .ne.    'g'     )                       error stop 38
        if ( char1(15)  .ne.    '!'     )                       error stop 39
        if ( char1(16)  .ne.    '\n'    )                       error stop 40

end program

