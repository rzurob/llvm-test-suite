!*  ===================================================================
!*
!*  DATE                       : 04/03/2006
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Pass characters with run time length
!*                               to subroutines with Value attribute
!*				 dummy args. The actual argument size
!*				will be greater than to the dummy arg size
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

program valueAttrDeferredLen003

    character(:), allocatable :: ch
    integer :: len

    len = 64

    call testV(ch, len)
    if (ch /= 'abcdefghijklmopqrstuvwxyzabcdefgabcdefghijklmopqrstuvwxyzabcdefg') error stop 1_4

    deallocate(ch)

    call test1(ch, len)
    if (ch /= 'abcdefghijklmopqrstuvwxyzabcdefgabcdefghijklmopqrstuvwxyzabcdefg') error stop 2_4

    deallocate(ch)

    call test2(ch, len)
    if (ch /= 'zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzg') error stop 3_4

    contains

    subroutine test1(tmp, n)

        character(:), allocatable :: tmp
	integer :: n
	allocate(character(n) :: tmp)
	tmp = 'abcdefghijklmopqrstuvwxyzabcdefgabcdefghijklmopqrstuvwxyzabcdefg'
	call test12(tmp)

    end subroutine

	subroutine test12(x)

	   character (63), value :: x
	   x = 'zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz'

        end subroutine

    subroutine test2(tmp, n)

        character(:), allocatable :: tmp
	integer :: n
	allocate(character(n) :: tmp)
	tmp = 'abcdefghijklmopqrstuvwxyzabcdefgabcdefghijklmopqrstuvwxyzabcdefg'
	call test22(tmp)

    end subroutine

	subroutine test22(x)
	   character (63) :: x
	   x = 'zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz'
        end subroutine

    subroutine testV(tmp, n)

        character(:), allocatable :: tmp
	integer :: n
	allocate(character(n) :: tmp)
	tmp = 'abcdefghijklmopqrstuvwxyzabcdefgabcdefghijklmopqrstuvwxyzabcdefg'
	call testV2(tmp)

    end subroutine

	subroutine testV2(x)
	   character (63) :: x
	   value :: x
	   x = 'zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz'
        end subroutine

   end
