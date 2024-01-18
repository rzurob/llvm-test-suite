!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : acesynt10
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : David Forster
!*  DATE                       : 2006-07-05
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : Type specifier syntax
!*
!*  REFERENCE                  : Feature Number 289053
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  KEYWORD(S)                 : syntax, type specifier, array constructor
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
!*  The test is successful if the output is correct.
!*
!* =============================================================================
!2345678901234567890123456789012345678901234567890123456789012345678901234567890

program acesynt10

  integer :: iarr (4), iempty(0)
  real    :: rarr (3), rempty(0)
  logical :: larr (3), lempty(0)
  complex :: zarr (3), zempty(0)

  iarr = [1, 2, (/3, 4/)]
  iarr = [integer::    1, 2, (/3, 4/)]
  iarr = [integer(1):: 1, 2, (/3, 4/)]
  iarr = [integer(2):: 1, 2, (/3, 4/)]
  iarr = [integer(4):: 1, 2, (/3, 4/)]
  iarr = [integer(8):: 1, 2, (/3, 4/)]
  iarr = [integer(2):: 1, [integer(1):: 2, [integer:: 3, 4]]]
  iarr = [integer(8):: 1, [integer(4):: 2, [integer:: 3, 4]]]
  iempty = [integer:: ]
  iempty = (/integer:: (/integer::/) /)
  iempty = (/integer:: [integer(4):: [integer(8):: ]]/)

  rarr = (/real:: 1.1, 2.2, 3.3/)
  rarr = (/real(4):: 1.1, (/real(8):: 2.2, (/real:: 3.3/)/)/)
  rarr = (/double precision:: 1.1, (/real(8):: 2.2, (/real:: 3.3/)/)/)
  rarr = (/double precision:: 1.1, (/real(4):: 2.2, (/3.3/)/)/)
  rarr = (/real:: 1.1, (/2.2, (/real(4):: 3.3/)/)/)
  rempty = [real:: ]
  rempty = (/real:: (/real::/)/)
  rempty = (/real:: [real(4):: [real(8)::]]/)

  larr = (/ [logical:: .true.], [logical:: .false.], (/logical:: .true./) /)
  larr = (/ [logical::], [logical::[logical::]], [logical:: .false.], (/logical:: .true./), (/(/logical:: .false./)/) /)
  lempty = [logical:: ]
  lempty = (/logical:: (/logical:: /)/)
  lempty = (/logical:: [logical(4):: [logical(2):: ]]/)

  zarr = (/ complex:: (1,1), 1, (0,1)/)
  zarr = (/ complex(8):: (1,1), [complex(4):: 1, [complex:: (0,1)]]/)
  zarr = (/ complex:: (1,1), [complex(8):: 1, [(0,1)]]/)
  zempty = [complex:: ]
  zempty = (/complex:: (/complex:: /)/)
  zempty = (/complex:: [complex(4):: [complex(8):: ]]/)

end program acesynt10
