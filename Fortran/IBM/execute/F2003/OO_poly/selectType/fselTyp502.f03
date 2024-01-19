! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 10/21/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : select type (unlimited poly-array data of character
!                               type as selector and used in IO stmt as internal
!                               file)
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

program fselTyp502
    class (*), allocatable :: x(:)

    allocate (character(6) ::x(4))

    select type (x)
        type is (character(*))
            write (x(::2), '(a3,i3)') 'abc', 10, 'abc', 30
            write (x(2::2), '(a3,i3)') 'xyz',20, 'xyz', 40
        class default
            print *, 'wrong'
    end select

    select type (x)
        type is (character(*))
            write (*, '(4a10)') x
        class default
            print *, 'wrong'
    end select
end
