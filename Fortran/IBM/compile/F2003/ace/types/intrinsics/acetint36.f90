!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : acetint36
!*
!*  DATE                       : 2006-11-14
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : AC in specification expressions
!*
!*  REFERENCE                  : Feature Number 289053
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  From 7.1.6:
!*   "A specification expression is an expression with limitations that make it
!*    suitable for use in specifications such as length type parameters (C501) and
!*    array bounds (R512, R513)." [So, we need to test these in lengths, lower bounds
!*   and upper bounds (including the explicit-shape-spec-list and lower
!*   bound of an assumed size spec).  Kind selectors should be tested elsewhere.]
!*
!*  Primaries in these expressions include "(1) A constant or subobject of a
!*  constant, (2) [a dummy argument] (3) [an object in a common block] (4) [an
!*  object accessible by use or host association] (5) An array constructor where
!*  each element and each scalar-int-expr of each ac-implied-do-control is a
!*  restricted expression, ... (11) An ac-do-variable within an array constructor
!*  where each scalar-int-expr of the corresponding ac-implied-do-control is a
!*  restricted expression,...".
!*  Strategy: Use AC's with restricted expressions in specification expressions,
!*  i.e., lengths (chars), lbounds, ubounds, and dimensions as a whole
!*  in intrinsic declarations (e.g., "real(4)::r(3)"), dimension statements, and
!*  dimension attributes;
!*  these will be found in in regular declaration parts and declarations of dummy
!*  arguments, both explicit and assumed shape.
!*  Also, we test the use in lengths in character ACs which are not in declaration parts.
!*
!*  We specifically do not test DTP specification expressions.  These will be
!*  found in dtparam/ace/....
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program acetint36

  implicit none
  integer (4)      :: iarr(size([real::99,88,77,66]))
  real (4)         :: rarr(len([character(3)::]):digits([real(8)::]), kind([logical(4)::]):kind([logical(8)::]))
  double precision :: darr
  logical (1)      :: larr(bit_size(iarr))
  complex (4), dimension(len([character(5)::])) :: zarr
  character (size([integer:: 1,2,3,4])) :: ch

  dimension darr(kind([larr]))

  common /zort/ larr, iarr

  integer :: i

  print *, 'L:', lbound(iarr), ', U:', ubound(iarr), ', Sh:', shape(iarr)
  print *, 'L:', lbound(rarr), ', U:', ubound(rarr), ', Sh:', shape(rarr)
  print *, 'L:', lbound(darr), ', U:', ubound(darr), ', Sh:', shape(darr)
  print *, 'L:', lbound(larr), ', U:', ubound(larr), ', Sh:', shape(larr)
  print *, 'L:', lbound(zarr), ', U:', ubound(zarr), ', Sh:', shape(zarr)
  print *, len(ch)

  call sub([integer(4):: (i,i=1,19)], [character(1):: (achar(64+i),achar(94+i),i=1,26)], &
           [real(4):: 1.1, 2.2], [double complex:: ((i-1.,i*2.),i=-1,1,2)]) ! 52
  i = 5

  print *, [character(i):: 'a'], 'x'
  print *, [character(4):: [character(1):: 'a'] // 'b', [character(2):: 'a'] // 'b', [character(3):: 'a'] // 'b'], 'x'
  print *, [character(4):: ([character(i):: 'a'] // 'b', i=1,3)], 'x'
  print *, [character(kind([integer(4)::])):: 'a'], 'x'
  print *, [character(lbound([real(4)::1.0])):: 'a'], 'x'
  print *, [character(size([logical(1):: .true.,.false.])):: 'a'], 'x'
  print *, (len([character(i)::]), i=1,4)
  print *, [integer(2):: (len([character(i)::]), i=1,4)]

contains

  ! Test use in dummy arguments:
  subroutine sub(ar1, ar2, ar3, ar4)
    integer (4) :: ar1(size([integer(1):: (i,i=1,3)]):*)
    character (4) :: ar2(kind([logical(2)::.true.]):len([character(3)::]),digits([real(8)::]):*)
    real (4), dimension((kind([logical(2)::]))) :: ar3
    double complex :: ar4
    dimension :: ar4(len([character(2)::]))
    print *, lbound(ar1), ": *"
    print *, lbound(ar2,1), ':', ubound(ar2,1), ',', lbound(ar2,2), ": *"
    print *, lbound(ar3), ':', ubound(ar3)
    print *, lbound(ar4), ':', ubound(ar4)
  end subroutine sub

end program acetint36
