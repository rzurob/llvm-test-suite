!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2006-10-06
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : Type specifier syntax (character, doubles, derived)
!*
!*  REFERENCE                  : Feature Number 289053
!*
!*  REQUIRED COMPILER OPTIONS  : none
!*
!*  KEYWORD(S)                 : syntax, type specifier, array constructor, character, double, derived
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
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module acesynt10amod

  implicit none
  type derived
     integer :: i = -1
  end type derived

end module acesynt10amod


program acesynt10a

  use acesynt10amod
  implicit none

  character (1)    :: c1arr(3), c1empty(0)
  character (2)    :: c2arr(3), c2empty(0)
  double precision :: dparr(3), dpempty(0)
  double complex   :: dcarr(3), dcempty(0)
  type(derived)    :: dtparr(3), dtpempty(0)

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


  dtparr = (/ [derived:: derived(1)], [derived:: derived(3)], (/derived:: derived(1)/) /)
  dtparr = (/ [derived::], [derived::[derived::]], [derived:: derived(3)], (/derived:: derived(1)/), (/(/derived:: derived(3)/)/) /)
  dtpempty = [derived:: ]
  dtpempty = (/derived:: (/derived:: /)/)
  dtpempty = (/derived:: [derived:: [derived:: ]]/)

end program acesynt10a
