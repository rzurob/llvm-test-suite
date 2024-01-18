!***********************************************************************
!* =====================================================================
!*
!*  TEST CASE NAME             : acesynt41adk
!*
!*                               by David Forster)
!*  DATE                       : 2007-11-16 (original: 2006-10-18)
!*  ORIGIN                     : Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters Array Constructor
!*                               Enhancements
!*  SECONDARY FUNCTIONS TESTED : Array Constructor Enhancement AC containing
!*                               assumed-size array (assignment)
!*
!*  REFERENCE                  : Feature Number 289057(.F2003TCx)
!*                               (original: Feature Number 289053)
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : dummy argument, array constructor,
!*                               assumed-size, assignment
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Assumed-size arrays are not allowed as a "a whole array reference" anywhere
!*  but "as an actual argument in a procedure reference for which the shape is
!*  not required" (section 5.1.2.5 of the standard), so they cannot appear alone
!*  in an AC, e.g., [assumedSizeArray], but they can appear if more detail
!*  is added, e.g., [assumedSizeArray(1)] or [(assumedSizeArray(i),i=1,n)].
!*  This case simply verifies that assumed-size arrays appearing as whole array
!*  references in array constructors in assignment statements are flagged.
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

program acesynt41adk

  implicit none

  type derived (kderived_1) ! kderived_1=4
     integer, kind :: kderived_1
     integer(kderived_1) :: field = 1
  end type derived

  integer(4)   :: irank1 (2), irank2 (2,2)
  logical(4)   :: lrank1 (2), lrank2 (2,2)
  real(4)      :: rrank1 (2), rrank2 (2,2)
  complex(4)   :: zrank1 (2), zrank2 (2,2)
  character(4) :: crank1 (2), crank2 (2,2)
  type(derived(4)):: drank1 (2), drank2 (2,2) ! tcx: (4)
  integer      :: i

  irank1 = [99, 100];                irank2 = reshape([integer(4):: 4,5,6,7],[2,2])
  lrank1 = [.true., .false.];        lrank2 = reshape([logical(4):: ((i/2)==0,i=1,4)],[2,2])
  rrank1 = [9.9, 10.0];              rrank2 = reshape([real(4):: .4,.5,.6,.7],[2,2])
  zrank1 = [(1.,1.), (0.,0.)];       zrank2 = reshape([complex(4):: ((1.1,1.1),i=1,4)], [2,2])
  crank1 = ['abcd', 'efgh'];         crank2 = reshape([character(4):: '1234','5678','9abc','def0'],[2,2])
  drank1 = [derived(4)(9), derived(4)(5)]; drank2 = reshape([derived(4):: (derived(4)(i),i=1,4)],[2,2]) ! tcx: (4) ! tcx: (4) ! tcx: (4) ! tcx: (4)

  ! None of these variations should matter, actually, but we'll include them
  ! in case one or more optimisation pathway is not otherwise exercised:

  call buildWithAssumedInteger (2, [2, 3])
  call buildWithAssumedInteger (3, [integer(4):: 3,4,5])

  call buildWithAssumedInteger (size(irank1), irank1)
  call buildWithAssumed2Integer(size(irank2), irank2)
  call buildWithAssumed3Integer(1, 2, size(irank2), irank2)

  call buildWithAssumedLogical (size(lrank1), lrank1)
  call buildWithAssumed2Logical(size(lrank2), lrank2)
  call buildWithAssumed3Logical(1, 2, size(lrank2), lrank2)

  call buildWithAssumedReal (size(rrank1), rrank1)
  call buildWithAssumed2Real(size(rrank2), rrank2)
  call buildWithAssumed3Real(1, 2, size(rrank2), rrank2)

  call buildWithAssumedComplex (size(zrank1), zrank1)
  call buildWithAssumed2Complex(size(zrank2), zrank2)
  call buildWithAssumed3Complex(1, 2, size(zrank2), zrank2)

  call buildWithAssumedCharacter (size(crank1), crank1)
  call buildWithAssumed2Character(size(crank2), crank2)
  call buildWithAssumed3Character(1, 2, size(crank2), crank2)

  call buildWithAssumedDerived (size(drank1), drank1)
  call buildWithAssumed2Derived(size(drank2), drank2)
  call buildWithAssumed3Derived(1, 2, size(drank2), drank2)

contains

  subroutine buildWithAssumedInteger(m, arr)
    integer :: m
    integer(4) :: arr(*), array(m)
    array =  (/ integer(4):: arr /)
  end subroutine buildWithAssumedInteger

  subroutine buildWithAssumed2Integer(m, arr)
    integer :: m
    integer(4) :: arr(2,*), array(m)
    array =  (/ integer(4):: arr /)
  end subroutine buildWithAssumed2Integer

  subroutine buildWithAssumed3Integer(l, m, n, arr)
    integer :: l, m, n
    integer(4) :: arr(m,l:*), array(n)
    array =  (/ integer(4):: arr /)
  end subroutine buildWithAssumed3Integer


  subroutine buildWithAssumedLogical(m, arr)
    integer :: m
    logical(4) :: arr(*), array(m)
    array =  (/ logical(4):: arr /)
  end subroutine buildWithAssumedLogical

  subroutine buildWithAssumed2Logical(m, arr)
    integer :: m
    logical(4) :: arr(2,*), array(m)
    array =  (/ logical(4):: arr /)
  end subroutine buildWithAssumed2Logical

  subroutine buildWithAssumed3Logical(l, m, n, arr)
    integer :: l, m, n
    logical(4) :: arr(m,l:*), array(n)
    array =  (/ logical(4):: arr /)
  end subroutine buildWithAssumed3Logical


  subroutine buildWithAssumedComplex(m, arr)
    integer :: m
    complex(4) :: arr(*), array(m)
    array =  (/ complex(4):: arr /)
  end subroutine buildWithAssumedComplex

  subroutine buildWithAssumed2Complex(m, arr)
    integer :: m
    complex(4) :: arr(2,*), array(m)
    array =  (/ complex(4):: arr /)
  end subroutine buildWithAssumed2Complex

  subroutine buildWithAssumed3Complex(l, m, n, arr)
    integer :: l, m, n
    complex(4) :: arr(m,l:*), array(n)
    array =  (/ complex(4):: arr /)
  end subroutine buildWithAssumed3Complex


  subroutine buildWithAssumedReal(m, arr)
    integer :: m
    real(4) :: arr(*), array(m)
    array =  (/ real(4):: arr /)
  end subroutine buildWithAssumedReal

  subroutine buildWithAssumed2Real(m, arr)
    integer :: m
    real(4) :: arr(2,*), array(m)
    array =  (/ real(4):: arr /)
  end subroutine buildWithAssumed2Real

  subroutine buildWithAssumed3Real(l, m, n, arr)
    integer :: l, m, n
    real(4) :: arr(m,l:*), array(n)
    array =  (/ real(4):: arr /)
  end subroutine buildWithAssumed3Real


  subroutine buildWithAssumedCharacter(m, arr)
    integer :: m
    character(4) :: arr(*), array(m)
    array =  (/ character(4):: arr /)
  end subroutine buildWithAssumedCharacter

  subroutine buildWithAssumed2Character(m, arr)
    integer :: m
    character(4) :: arr(2,*), array(m)
    array =  (/ character(4):: arr /)
  end subroutine buildWithAssumed2Character

  subroutine buildWithAssumed3Character(l, m, n, arr)
    integer :: l, m, n
    character(4) :: arr(m,l:*), array(n)
    array =  (/ character(4):: arr /)
  end subroutine buildWithAssumed3Character


  subroutine buildWithAssumedDerived(m, arr)
    integer :: m, iarray(m)
    type(derived(4)) :: arr(*), array(m) ! tcx: (4)
    array  = (/ derived(4):: arr /) ! tcx: (4)
    iarray = (/ integer:: arr % field /)
  end subroutine buildWithAssumedDerived

  subroutine buildWithAssumed2Derived(m, arr)
    integer :: m, iarray(m)
    type(derived(4)) :: arr(2,*), array(m) ! tcx: (4)
    array =  (/ derived(4):: arr /) ! tcx: (4)
    iarray = (/ integer:: arr % field /)
  end subroutine buildWithAssumed2Derived

  subroutine buildWithAssumed3Derived(l, m, n, arr)
    integer :: l, m, n, iarray(m)
    type(derived(4)) :: arr(m,l:*), array(n) ! tcx: (4)
    array =  (/ derived(4):: arr /) ! tcx: (4)
    iarray = (/ integer:: arr % field /)
  end subroutine buildWithAssumed3Derived

end program acesynt41adk

! Extensions to introduce derived type parameters:
! type: derived - added parameters (kderived_1) to invoke with (4)/declare with (4) - 11 changes
