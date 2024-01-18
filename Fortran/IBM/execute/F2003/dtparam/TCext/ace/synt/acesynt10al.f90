!***********************************************************************
!* =====================================================================
!*
!*  TEST CASE NAME             : acesynt10al
!*
!*                               by David Forster)
!*  DATE                       : 2007-12-06 (original: 2006-10-06)
!*  ORIGIN                     : Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*                               (+ Array Constructor Enhancements)
!*  SECONDARY FUNCTIONS TESTED : Array Constructor Enhancement Type
!*                               specifier syntax (character, doubles, derived)
!*
!*  REFERENCE                  : Feature Number 289057(.F2003TCx)
!*                               (original: Feature Number 289053)
!*
!*  REQUIRED COMPILER OPTIONS  : none
!*
!*  KEYWORD(S)                 : syntax, type specifier, array constructor,
!*                               character, double, derived
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Verify that a type specifier is permitted at the start of an array
!*  constructor and that nested array constructors may themselves contain an
!*  initial type specifier.
!*
!*  A corresponding set of tests which should not work are given in acesynt1?d.f
!*
!*  Same set of tests as acesynt10, but applied to character, double precision,
!*  double complex, and derived.
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

module acesynt10amod

  implicit none
  type derived (lderived_1) ! lderived_1=5
     integer, len :: lderived_1
     integer(4) :: i = -1
  end type derived

end module acesynt10amod


program acesynt10al

  use acesynt10amod
  implicit none

  character (1)    :: c1arr(3), c1empty(0)
  character (2)    :: c2arr(3), c2empty(0)
  double precision :: dparr(3), dpempty(0)
  double complex   :: dcarr(3), dcempty(0)
  type(derived(5))    :: dtparr(3), dtpempty(0) ! tcx: (5)

  ! We don't need to test this with characters (already covered in acetint10c),
  ! but we did forget to test an empty constructor for character.
  c1empty = (/ character :: /)
  c2empty = (/ character :: /)

  c1empty = (/ character(1) :: /)
  c2empty = (/ character(1) :: /)

  c1empty = (/ character(2) :: /)
  c2empty = (/ character(2) :: /)

  c1empty = (/ character(2000) :: /)
  c2empty = (/ character(2000) :: /)

  c1empty = [ character :: ]
  c2empty = [ character :: ]

  c1empty = [ character(1) :: ]
  c2empty = [ character(1) :: ]

  c1empty = [ character(2) :: ]
  c2empty = [ character(2) :: ]

  c1empty = [ character(2000) :: ]
  c2empty = [ character(2000) :: ]


  ! Test double precision, double complex, and derived in the same way as acesynt10
  dparr = (/double precision:: 1.1d0, 2.2d0, 3.3d0/)
  dparr = (/double precision:: 1.1d0, (/double precision:: 2.2d0, (/double precision:: 3.3d0/)/)/)
  dparr = (/real:: 1.1d0, (/double precision:: 2.2d0, (/double precision:: 3.3d0/)/)/)
  dparr = (/real:: 1.1d0, (/double precision:: 2.2d0, (/3.3d0/)/)/)
  dparr = (/double precision:: 1.1d0, (/2.2d0, (/double precision:: 3.3d0/)/)/)
  dpempty = [double precision:: ]
  dpempty = (/double precision:: (/double precision::/)/)
  dpempty = (/double precision:: [real:: [double precision::]]/)


  dcarr = (/ double complex:: (1d0,1d0), 1d0, (0d0,1d0)/)
  dcarr = (/ double complex:: (1d0,1d0), [complex:: 1d0, [double complex:: (0d0,1d0)]]/)
  dcarr = (/ double complex:: (1d0,1d0), [double complex:: 1d0, [(0d0,1d0)]]/)
  dcempty = [double complex:: ]
  dcempty = (/double complex:: (/double complex:: /)/)
  dcempty = (/double complex:: [complex:: [double complex:: ]]/)


  dtparr = (/ [derived(5):: derived(5)(1)], [derived(5):: derived(5)(3)], (/derived(5):: derived(5)(1)/) /) ! tcx: (5) ! tcx: (5) ! tcx: (5) ! tcx: (5) ! tcx: (5) ! tcx: (5)
  dtparr = (/ [derived(5)::], [derived(5)::[derived(5)::]], [derived(5):: derived(5)(3)], (/derived(5):: derived(5)(1)/), (/(/derived(5):: derived(5)(3)/)/) /) ! tcx: (5) ! tcx: (5) ! tcx: (5) ! tcx: (5) ! tcx: (5) ! tcx: (5) ! tcx: (5) ! tcx: (5) ! tcx: (5)
  dtpempty = [derived(5):: ] ! tcx: (5)
  dtpempty = (/derived(5):: (/derived(5):: /)/) ! tcx: (5) ! tcx: (5)
  dtpempty = (/derived(5):: [derived(5):: [derived(5):: ]]/) ! tcx: (5) ! tcx: (5) ! tcx: (5)

end program acesynt10al


! Extensions to introduce derived type parameters:
! type: derived - added parameters (lderived_1) to invoke with (5)/declare with (*) - 22 changes
