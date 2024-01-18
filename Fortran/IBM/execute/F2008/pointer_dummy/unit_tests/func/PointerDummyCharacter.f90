!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : May 13, 2011
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature Number 916820
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION:
!*  -----------
!*  Fortran 2008 allows procedure calls where:
!*	  1. The dummy argument has the POINTER and INTENT(IN) attributes, and
!*	  2. The actual argument is a nonpointer that has the TARGET attribute
!*
!234567890123456789012345678901234567890123456789012345678901234567890

character(11), target :: t
character(12), target :: t2
character(:), pointer :: p

interface
  subroutine charcmp(ptr1, ptr2)
    character(*), pointer, intent(in) :: ptr1, ptr2
  end subroutine
end interface

t = "hello world"

call subsub(t)

print *, funcfunc(t)

t2 = t // '.'
call charcmp(t, t2)

p => t
call ptr_with_tar(p, t, t)

contains
  subroutine subsub(ptr)
    character(*), pointer, intent(in) :: ptr

    print *, ptr
    print *, len(ptr)
  end

  character(20) function funcfunc(ptr)
    character(*), pointer, intent(in) :: ptr

    funcfunc = TRIM(ptr) // ", right?"
  end

  subroutine ptr_with_tar(ptr, tar, t)
    character(:), pointer, intent(in) :: ptr
    character(*), target :: tar
    character(*), pointer, intent(in) :: t

    print *, "ptr is: ", ptr
    print *, "target is: ", tar
    print *, "t is: ", t
    print *, associated(ptr, tar)
    print *, associated(t, tar)
  end
end

subroutine charcmp(ptr1, ptr2)
  character(*), pointer, intent(in) :: ptr1, ptr2

  if (len(ptr1) >= len(ptr2)) then
    print *, "'", ptr1, "'", " wins"
  else
    print *, "'", ptr2, "'", " wins"
  endif
end subroutine
