!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2006-10-25
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : data pointers in AC's with intrinsic type specifiers (character)
!*
!*  REFERENCE                  : Feature Number 289053
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : data pointer, character
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Include initialised data pointer references in AC's and later test their
!*  values, to verify that they are allowed and have the correct value.
!*  The data pointers refer to variables apart from the array as well as to
!*  members and sections of the array.  Character pointers can also have different
!*  lengths, and, in fact, can have a deferred length.
!*
!*  We start by initialising text targets and pointers, making sure that the pointers
!*  refer to portions of the arrays to be defined.  Section 7.4.1.3 of the standard
!*  tells us that "The execution of the assignment shall have the same effect as if
!*  the evaluation of all operations in expr [the RHS] and variable [the LHS]
!*  occurred before any portion of variable is defined by the assignment."  To
!*  verify this, we print the expected values before the assignment and the actual
!*  values after.  We also use some different lengths, to verify that appropriate
!*  conversions are made.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program acetint09c

  implicit none
  integer :: i, j

  character (5), target  :: c5t, c5tarr5(5), c5tarr5D2(5,0:1)
  character (2), pointer :: c2p, c2parr(:)
  character (5), pointer :: c5p, c5parr(:)
  character (:), pointer :: cdp, cdparr(:)

  ! Preamble: initialise data and show that it is correct:
  c5t = 'help!'
  print *, ">", c5t, "<"

  c5tarr5 = [character(5):: 'abcde','fghij', 'klmno', 'pqrst', 'uvwxy']
  print *, ">", c5tarr5, "<"

  c2p => c5t(3:4)
  print *, ">", c2p, "<"

  c2parr => c5tarr5(5:1:-2)(1:2)
  print *, ">", c2parr, "<"

  cdp => c5t(2:4)
  print *, ">", cdp, "<"

  c5tarr5D2 = reshape([character(5):: 'ABCDE','FGHIJ', 'KLMNO', 'PQRST', 'UVWXY', 'abcde','fghij', 'klmno', 'pqrst', 'uvwxy'],[5,2])
  print *, ">", c5tarr5D2, "<"

  ! Sample multidimensional data, same length each time:
  print *, ">", [character(5):: (c5tarr5D2(i,i/3)(i:i), i=1,5)], "<"
  c5tarr5 = [character(5):: (c5tarr5D2(i,i/3)(i:i), i=1,5)]
  print *, ">", c5tarr5, "<"

  ! Sample multidimensional data, increasing length:
  print *, ">", [character(5):: (c5tarr5D2(i,i/3)(1:i), i=1,5)], "<"
  c5tarr5 = [character(5):: (c5tarr5D2(i,i/3)(1:i), i=1,5)]
  print *, ">", c5tarr5, "<"

  c5tarr5D2 = reshape([character(5):: 'ABCDE','FGHIJ', 'KLMNO', 'PQRST', 'UVWXY', 'abcde','fghij', 'klmno', 'pqrst', 'uvwxy'],[5,2])
  print *, ">", reshape([character(5):: ((c5tarr5D2(i,j)(i:i), i=5,1,-1),j=0,1)], [5,2]), "<"
  c5tarr5D2 = reshape([character(5):: ((c5tarr5D2(i,j)(i:i), i=5,1,-1),j=0,1)], [5,2])
  print *, ">", c5tarr5D2, "<"

  ! Sample multidimensional data, decreasing length:
  c5tarr5D2 = reshape([character(5):: 'ABCDE','FGHIJ', 'KLMNO', 'PQRST', 'UVWXY', 'abcde','fghij', 'klmno', 'pqrst', 'uvwxy'],[5,2])
  print *, ">", reshape([character(5):: ((c5tarr5D2(i,j)(1:i), i=5,1,-1),j=0,1)], [5,2]), "<"
  c5tarr5D2 = reshape([character(5):: ((c5tarr5D2(i,j)(1:i), i=5,1,-1),j=0,1)], [5,2])
  print *, ">", c5tarr5D2, "<"

  c5tarr5D2 = reshape([character(5):: 'ABCDE','FGHIJ', 'KLMNO', 'PQRST', 'UVWXY', 'abcde','fghij', 'klmno', 'pqrst', 'uvwxy'],[5,2])
  print *, ">", reshape([character(5):: ((c5tarr5D2(i,j), i=5,1,-1),j=0,1)], [5,2]), "<"
  c5tarr5D2 = reshape([character(5):: ((c5tarr5D2(i,j), i=5,1,-1),j=0,1)], [5,2])
  print *, ">", c5tarr5D2, "<"

  c5tarr5 = [character(5):: 'ABCDE','FGHIJ', 'KLMNO', 'PQRST', 'UVWXY']
  c2parr  => c5tarr5(2:4:2)(2:3)
  c2p     => c5tarr5(3)(4:5)
  print *, ">", [character(5):: (c2parr,j=0,1), c2p], "/", c2parr, "/", c2p, "<"
  c5tarr5 = [character(5):: (c2parr,j=0,1), c2p]
  print *, ">", c5tarr5, "/", c2parr, "/", c2p, "<"

  c5tarr5 = [character(5):: 'ABCDE','FGHIJ', 'KLMNO', 'PQRST', 'UVWXY']
  c5parr  => c5tarr5(2:4:2)
  c5p     => c5tarr5(3)
  print *, ">", [character(5):: (c5parr,j=0,1), c5p], "/", c5parr, "/", c5p, "<"
  c5tarr5 = [character(5):: (c5parr,j=0,1), c5p]
  print *, ">", c5tarr5, "/", c5parr, "/", c5p, "<"

  c5tarr5D2 = reshape([character(5):: 'ABCDE','FGHIJ', 'KLMNO', 'PQRST', 'UVWXY', 'abcde','fghij', 'klmno', 'pqrst', 'uvwxy'],[5,2])
  cdparr => c5tarr5D2(5:1:-1,0)
  print *, ">", cdparr, "<"

  print *, ">", reshape([character(5):: (cdparr,j=0,1)], [5,2]), "<"
  c5tarr5D2 = reshape([character(5):: (cdparr,j=0,1)], [5,2])
  print *, ">", c5tarr5D2, "<"

end program acetint09c
