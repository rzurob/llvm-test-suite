!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : acetdt476cd
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : David Forster
!*  DATE                       : 2006-07-19
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : C476 (R455) type-name must be accessible derived type
!*
!*  REFERENCE                  : Feature Number 289053
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : array constructor, accessible, derived type
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Constraint C476 on rule R455:
!*     "derived-type-spec is type-name [(type-param-spec-list)]"
!*  requires type-name to be an accessible derived type.  This diagnostic tests
!*  that using an unknown derived type generates a compile error.
!*  We're testing three contexts: as an actual argument, as source to allocate,
!*  and as RHS of simple assignment.
!*
!* =============================================================================
!2345678901234567890123456789012345678901234567890123456789012345678901234567890

program acetdt476cd

  implicit none
  integer :: i
  class(*), allocatable :: al(:)

!!! Definition intentionally commented out: the program should compile and run
!!! with no output if the following type is defined; we're testing that the
!!! compiler generates error messages in its absence:
!!type :: unk; end  type unk

  call test((/ unk:: /))       ! Error: unknown type in type specifier
  call test((/ unk:: unk() /)) ! same, plus "Entity unk has undefined type" at end

  allocate(al(0), source=(/ unk:: /))
  deallocate(al)
  allocate(al(0), source=(/ unk:: (unk(),i=1,0) /))
  deallocate(al)
  allocate(al(1), source=(/ unk:: (unk(),i=1,1) /))
  deallocate(al)

  ! Also test []:
  call test( [ unk:: ])       ! Error: unknown type in type specifier
  call test( [ unk:: unk() ]) ! same, plus "Entity unk has undefined type" at end

  allocate(al(0), source= [ unk:: ])
  deallocate(al)
  allocate(al(0), source= [ unk:: (unk(),i=1,0) ])
  deallocate(al)
  allocate(al(1), source= [ unk:: (unk(),i=1,1) ])

  call testunk

contains

  ! This is just something to hang an array constructor on:
  subroutine test(arr)
    class (*) :: arr(:)
  end subroutine test

  subroutine testunk()
    type (unk) :: arr0(0), arr(1)
    arr0 = (/ unk:: /)
    arr0 = (/ unk:: (unk(),i=1,0) /)
    arr  = (/ unk:: unk() /)
    arr0 =  [ unk:: ]
    arr0 =  [ unk:: (unk(),i=1,0) ]
    arr  =  [ unk:: unk() ]
  end subroutine testunk

end program acetdt476cd
