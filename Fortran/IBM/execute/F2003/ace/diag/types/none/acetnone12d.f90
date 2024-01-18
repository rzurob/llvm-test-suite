!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : acetnone12d
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : David Forster
!*  DATE                       : 2006-11-20
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : no TS, all ac_values of different numeric types
!*
!*  REFERENCE                  : Feature Number 289053
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  KEYWORD(S)                 : 
!*  TARGET(S)                  : 
!*  NUMBER OF TESTS CONDITIONS : 
!*
!*  DESCRIPTION
!*
!*  Try to print, assign, and call subroutines on array constructors containing
!*  different values of different types.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module acetnone12dmod

  implicit none
  type derived
  end type derived

end module acetnone12dmod


program acetnone12d

  use acetnone12dmod
  implicit none
  integer      :: ivar, iarr(3), i
  real         :: rvar, rarr(3)
  complex      :: zvar, zarr(3)
  character    :: cvar, carr(3)
  character(2) :: c2var, c2arr(3)
  character(9) :: c9var
  logical      :: lvar, larr(3)
  type(derived):: dvar, darr(3)

  ! These don't actually need to be initialised for this test, but let's be neat:
  ivar  = 1
  rvar  = 1.1
  zvar  = (1.,2.)
  cvar  = 'a'
  c2var = 'bc'
  c9var = 'defghijkl'
  lvar  = .true.
  dvar  = derived()

  ! Baseline: show that the sort of thing we want to do is legal, if types are the same
  iarr = [ivar, (int(i),i=1,1), 1]
  print *, [ivar, (int(i),i=1,1), 1, iarr]
  rarr = [rvar, (real(i),i=1,1), 1.2]
  print *, [rvar, (real(i),i=1,1), 1.2, rarr]
  zarr = [zvar, (cmplx(i,i),i=1,1), (1.2,1.1)]
  print *, [zvar, (cmplx(i,i),i=1,1), (1.2,1.1), zarr]
  carr = [cvar, (char(i),i=1,1), 'a']
  print *, [cvar, (char(i),i=1,1), 'a', carr]
  c2arr = [c2var, (repeat('a',2),i=1,1), 'aa']
  print *, [c2var, (repeat('a',2),i=1,1), 'aa']
  larr = [lvar, (logical(i==i),i=1,1), .true.]
  print *, [lvar, (logical(i==i),i=1,1), .true., larr]
  darr = [dvar, (derived(),i=1,1), derived()]
  print *, [dvar, (derived(),i=1,1), derived()]

  ! Now mix and match, but with type-specifiers (i.e., still legal):
  iarr  = [integer:: ivar, rvar, 1]
  iarr  = [integer:: ivar, zvar, 1]
  rarr  = [real::    rvar, ivar, 1.1]
  c2arr = [character(2):: cvar, c2var, c9var]

  ! Repeat with literals:
  iarr = [integer:: ivar, 1.2, 1]
  iarr = [integer:: ivar, (1.,2.), 1]
  rarr = [real::    rvar, 1, 1.1]
  c2arr = [character(2):: 'a', 'bc', 'defghijkl']

  ! And with implied-do's:
  iarr = [integer:: ivar, (1.2,i=1,1), 1]
  iarr = [integer:: ivar, ((1.,2.),i=1,1), 1]
  rarr = [real::    rvar, (1,i=1,1), 1.1]
  c2arr = [character(2):: (cvar, c2var, c9var, i=1,1)]

  ! And intrinsics:
  iarr = [integer:: ivar, real(ivar), 1]
  iarr = [integer:: ivar, cmplx(rvar,rvar), 1]
  rarr = [real::    rvar, int(rvar), 1.1]
  c2arr = [character(2):: char(ivar), repeat('a',2), repeat('a',9)]


  ! Now comes the illegal stuff:
  iarr = [ivar, rvar, 1]       ! no auto-conversion
  iarr = [ivar, zvar, 1]       ! no auto-conversion
  iarr = [ivar, cvar, 1]       ! no characters allowed at all
  iarr = [ivar, lvar, 1]       ! no logicals allowed at all
  rarr = [rvar, ivar, 1.1]     ! no auto-conversion
  c2arr = [cvar, c2var, c9var] ! lengths must be the same
  darr = [dvar, ivar, dvar]    ! no auto-conversion

  iarr = [ivar, 1.2, 1]
  iarr = [ivar, (1.,2.), 1]
  iarr = [ivar, 'a', 1]
  iarr = [ivar, .true., 1]
  rarr = [rvar, 1, 1.1]
  c2arr = ['a', 'bc', 'defghijkl']
  darr = [dvar, 1, dvar]

  iarr = [ivar, (1.2,i=1,1), 1]
  iarr = [ivar, ((1.,2.),i=1,1), 1]
  iarr = [ivar, ('a',i=1,1), 1]
  iarr = [ivar, (.true.,i=1,1), 1]
  rarr = [rvar, (1,i=1,1), 1.1]
  c2arr = [(cvar, c2var, c9var, i=1,1)]
  darr = [dvar, (1,i=1,1), dvar]

  iarr = [ivar, real(ivar), 1]
  iarr = [ivar, cmplx(rvar,rvar), 1]
  iarr = [ivar, char(ivar), 1]
  iarr = [ivar, logical(ivar==1), 1]
  rarr = [rvar, int(rvar), 1.1]
  c2arr = [char(ivar), repeat('a',2), repeat('a',9)]

  ! Round things out with print statements:
  print *, [zvar, rvar]
  print *, [ivar, lvar]
  print *, [cvar, dvar]
  print *, [cvar, c2var, c9var]

  print *, [(1.,2.), 1.2]
  print *, [1, .true.]
  print *, ['a', derived()]
  print *, ['a', 'bc', 'defghijkl']

  print *, [(1.,2.), (1.2,i=1,1)]
  print *, [1, (.true.,i=1,1)]
  print *, ['a', (derived(),i=1,1)]
  print *, ['a', ('bc', 'defghijkl',i=1,1)]

  print *, [real(ivar), cmplx(rvar,rvar)]
  print *, [char(ivar), logical(ivar==1)]
  print *, [dvar, int(rvar)]
  print *, [char(ivar), repeat('a',2), repeat('a',9)]

end program acetnone12d
