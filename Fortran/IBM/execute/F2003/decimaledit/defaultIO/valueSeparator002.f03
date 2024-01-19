! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/08/2006
!*
!*  DESCRIPTION                : DECIMAL EDIT MODE
!                               Test the of separator (;) for the list-directed
!                               READ in COMMA mode; read for character type with
!                               and without the delimitor.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program valueSeparator002
    character (10) c1, c2, c3

    character(*), parameter :: string = 'xlf;,test'

    write (1, '(a)') string
    write (1, *, delim="APOSTROPHE") string
    write (1, *, delim="QUOTE", decimal="COMMA") string

    rewind 1

    read (1, *, decimal='POINT') c1, c2

    if ((c1 /= 'xlf;') .or. (c2 /= 'test')) error stop 1_4

    backspace 1

    read (1, *, decimal='COMMA') c1, c2

    if ((c1 /= 'xlf') .or. (c2 /= ',test')) error stop 2_4

    read (1, *, decimal='COMMA') c1

    backspace 1

    read (1, *, decimal='POINT') c2

    if ((c1 /= string) .or. (c2 /= string)) error stop 3_4

    read (1, *, decimal='COMMA') c3

    if (c3 /= string) error stop 4_4
end
