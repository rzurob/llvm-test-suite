!*  ===================================================================
!*
!*  DATE                       : 07/25/2006
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : ALLOCATE with DTP
!*                               (type-spec uses len() intrinsic)
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ==================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program dtParamAlloc06

    type base (k, n)
        integer, kind :: k
        integer, len  :: n

        integer(k) :: data(n) = 0
    end type

    character(:), pointer :: astr
    character(20), target :: name_str

    class(*), pointer :: typtr

    astr => name_str

    allocate(base(4, len(astr)) :: typtr)

    if (sizeof(typtr) /= 80) stop 1

    call sub1(astr)
    contains
      subroutine sub1(pstr)
        character(*) pstr
        type(base(4, len(pstr))) :: local_var
        if (local_var%n /= 20) stop 2
      end subroutine

end
