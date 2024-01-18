!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            : valueAttrDeferredLen001.f
!*
!*  PROGRAMMER                 : Vicram Uppal
!*  DATE                       : 04/03/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : Pass characters with run time length
!*                               to subroutines with Value attribute 
!*				 dummy args. The actual argument size
!*				 will be equal to the dummy arg size
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

program valueAttrDeferredLen001

    character(:), allocatable :: ch
    integer :: len

    len = 8

    call testV(ch, len)
    if (ch /= 'abcdefgh') error stop 1_4

    deallocate(ch)

    call test1(ch, len)
    if (ch /= 'abcdefgh') error stop 2_4

    deallocate(ch)

    call test2(ch, len)
    if (ch /= 'zzzzzzzz') error stop 3_4

    contains

    subroutine test1(tmp, n)

        character(:), allocatable :: tmp
	integer :: n
	allocate(character(n) :: tmp)
	tmp = 'abcdefgh'
	call test12(tmp)

    end subroutine

	subroutine test12(x)

	   character (8), value :: x
	   x = 'zzzzzzzz'

        end subroutine

    subroutine test2(tmp, n)

        character(:), allocatable :: tmp
	integer :: n
	allocate(character(n) :: tmp)
	tmp = 'abcdefgh'
	call test22(tmp)

    end subroutine

	subroutine test22(x)
	   character (8) :: x
	   x = 'zzzzzzzz'
        end subroutine

    subroutine testV(tmp, n)

        character(:), allocatable :: tmp
	integer :: n
	allocate(character(n) :: tmp)
	tmp = 'abcdefgh'
	call testV2(tmp)

    end subroutine

	subroutine testV2(x)
	   character (8) :: x
	   value :: x
	   x = 'zzzzzzzz'
        end subroutine

   end
