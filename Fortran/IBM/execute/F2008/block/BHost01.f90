!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : BHost01
!*
!*  PROGRAMMER                 : dforster
!*  DATE                       : 2010-11-15
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : F2008 BLOCK
!*  SECONDARY FUNCTIONS TESTED : host association
!*  ADAPTED FROM               : -
!*
!*  DESCRIPTION
!*
!*  An inner procedure invoked from within a block can access host-associated
!*  vars, but not any defined in the block (this is a syntax error if implicit
!*  none is specified and no such variable exists in the host (tested in BHost01d).
!*  Note: "b" is not defined until after the first block, so we don't print it
!*  when inner is invoked from there (pb is false).
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program BHost01
  block
    character(3) :: b
    b = 'xyz'
    print *, 'block: ', b
    call inner (b, .false.)
  end block

  b = 3.5
  call inner(b, .true.)

  block
    logical :: b
    b = .true.
    print *, 'block 2: ', b
    call inner (b, .true.)
  end block

contains

  subroutine inner(v, pb)
    class(*) :: v
    logical  :: pb
    select type (v)
      type is (real)
        print *, 'inner: real:', v
      type is (logical)
        print *, 'inner: logical:', v
      type is (character(*))
        print *, 'inner: character:', v
      class default
        print *, 'unknown'
    end select
    if (pb) print *, 'inner: b=', b
  end subroutine inner

end program BHost01
