!**********************************************************************
!* ====================================================================
!* XL Fortran Test Case                           IBM INTERNAL USE ONLY
!* ====================================================================
!*
!*  TEST CASE NAME             : d344278
!*
!*  PROGRAMMER                 : Glen Mateer (derived from acetdt33dext)
!*
!*  DATE                       : 2007-11-28
!*
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : Array Constructor Enhancement
!*  REFERENCE                  : Feature Number 289057(.F2003TCx)
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DEFECT ABSTRACT            : DTPARAM: DIAG: Unexpected Results (Array
!*                               Constructor in ASSOCIATE Construct)
!*
!*  DESCRIPTION                :
!*  When run, this Test Case returns the following unexpected Diagnostic
!*  for line 22:
!*
!*  line 22.28: 1516-044 (S) A conversion from type associate is not permitted.
!*
!*  As well, 2 other Diagnostics from the original Test Case are omitted:
!*
!*  1516-036 (S) Entity bland has undefined type.
!*  1516-036 (S) Entity complex has undefined type.
!*
!*
!*  Given the above, I suspect that the Diagnostics for lines 26 and 30
!*  (each of which are expected to produce a message) are incorrect:
!*
!*  line 26.43: 1516-044 (S) A conversion from type associate is not permitted.
!*  line 30.39: 1516-044 (S) A conversion from type derived is not permitted.
!*
!*  From "F2003/dtparam/TCext/ace/diag/types/derived/acetdt33d.scenario"
!*  (the original Test Case), these messages probably should be:
!*
!*  line 26.43: 1516-044 (S) A conversion from type LOGICAL is not permitted.
!*  line 30.39: 1516-044 (S) A conversion from type INTEGER is not permitted.
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

module mod

  implicit none
  type :: associate(k1)    ! (4)
     integer, kind :: k1
     complex(k1)   :: val
  end type associate

  type :: derived(l1)    ! (1)
     integer, len  :: l1
     character(l1) :: val
  end type derived

end module mod

program d344278

  use mod
  implicit none
  integer :: i

  print *, [associate(4):: associate(4)((1.1,2.2))]

  associate(associate => [logical:: (i==1, i=1,2)], derived => [integer:: (i, i=1,1)] )

     associate(complex => [associate(4):: associate(4)((1.1,2.2))])
        print *, associate, complex
     end associate

     associate(bland => [derived(1):: derived(1)('a')])
        print *, derived, bland
     end associate

  end associate

end program d344278
