!***********************************************************************
!* =====================================================================
!*
!*                               by David Forster)
!*  DATE                       : 2007-11-16 (original: 2006-10-18)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters (+ Array Constructor
!*                               Enhancements)
!*  SECONDARY FUNCTIONS TESTED : Array Constructor Enhancement AC containing
!*                               assumed-size array (print)
!*
!*  REFERENCE                  : Feature Number 289057(.F2003TCx)
!*                               (original: Feature Number 289053)
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : dummy argument, array constructor,
!*                               assumed-size, print
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
!*  references in array constructors in print statements are flagged.
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

program acesynt41pdkl

  implicit none

  type derived (kderived_1,lderived_1) ! kderived_1,lderived_1=4,7
     integer, kind :: kderived_1
     integer, len :: lderived_1
     integer(kderived_1) :: field = 1
  end type derived

  integer(4)   :: irank1 (2), irank2 (2,2)
  logical(4)   :: lrank1 (2), lrank2 (2,2)
  real(4)      :: rrank1 (2), rrank2 (2,2)
  complex(4)   :: zrank1 (2), zrank2 (2,2)
  character(4) :: crank1 (2), crank2 (2,2)
  type(derived(4,7)):: drank1 (2), drank2 (2,2) ! tcx: (4,7)
  integer      :: i

  irank1 = [99, 100];               irank2 = reshape([integer(4):: 4,5,6,7],[2,2])
  lrank1 = [.true., .false.];       lrank2 = reshape([logical(4):: ((i/2)==0,i=1,4)],[2,2])
  rrank1 = [9.9, 10.0];             rrank2 = reshape([real(4):: .4,.5,.6,.7],[2,2])
  zrank1 = [(1.,1.), (0.,0.)];      zrank2 = reshape([complex(4):: ((1.1,1.1),i=1,4)], [2,2])
  crank1 = ['abcd', 'efgh'];        crank2 = reshape([character(4):: '1234','5678','9abc','def0'],[2,2])
  drank1 = [derived(4,7)(), derived(4,7)()];  drank2 = reshape([derived(4,7):: (derived(4,7)(),i=1,4)],[2,2]) ! tcx: (4,7) ! tcx: (4,7) ! tcx: (4,7) ! tcx: (4,7)

  ! None of these variations should matter, actually, but we'll include them
  ! in case one or more optimisation pathway is not otherwise exercised:

  call printAssumedInteger (2, [2, 3])
  call printAssumedInteger (3, [integer(4):: 3,4,5])

  call printAssumedInteger (size(irank1), irank1)
  call printAssumed2Integer(size(irank2), irank2)
  call printAssumed3Integer(1, 2, irank2)

  call printAssumedLogical (size(lrank1), lrank1)
  call printAssumed2Logical(size(lrank2), lrank2)
  call printAssumed3Logical(1, 2, lrank2)

  call printAssumedReal (size(rrank1), rrank1)
  call printAssumed2Real(size(rrank2), rrank2)
  call printAssumed3Real(1, 2, rrank2)

  call printAssumedComplex (size(zrank1), zrank1)
  call printAssumed2Complex(size(zrank2), zrank2)
  call printAssumed3Complex(1, 2, zrank2)

  call printAssumedCharacter (size(crank1), crank1)
  call printAssumed2Character(size(crank2), crank2)
  call printAssumed3Character(1, 2, crank2)

  call printAssumedDerived (size(drank1), drank1)
  call printAssumed2Derived(size(drank2), drank2)
  call printAssumed3Derived(1, 2, drank2)

contains

  subroutine printAssumedInteger(m, arr)
    integer :: m
    integer(4) :: arr(*), array(m)
    print *, (/ integer(4):: arr /)
  end subroutine printAssumedInteger

  subroutine printAssumed2Integer(m, arr)
    integer :: m
    integer(4) :: arr(2,*), array(m)
    print *, (/ integer(4):: arr /)
  end subroutine printAssumed2Integer

  subroutine printAssumed3Integer(l, m, arr)
    integer :: l, m
    integer(4) :: arr(m,l:*)
    print *, (/ integer(4):: arr /)
  end subroutine printAssumed3Integer


  subroutine printAssumedLogical(m, arr)
    integer :: m
    logical(4) :: arr(*), array(m)
    print *, (/ logical(4):: arr /)
  end subroutine printAssumedLogical

  subroutine printAssumed2Logical(m, arr)
    integer :: m
    logical(4) :: arr(2,*), array(m)
    print *, (/ logical(4):: arr /)
  end subroutine printAssumed2Logical

  subroutine printAssumed3Logical(l, m, arr)
    integer :: l, m
    logical(4) :: arr(m,l:*)
    print *, (/ logical(4):: arr /)
  end subroutine printAssumed3Logical


  subroutine printAssumedComplex(m, arr)
    integer :: m
    complex(4) :: arr(*), array(m)
    print *, (/ complex(4):: arr /)
  end subroutine printAssumedComplex

  subroutine printAssumed2Complex(m, arr)
    integer :: m
    complex(4) :: arr(2,*), array(m)
    print *, (/ complex(4):: arr /)
  end subroutine printAssumed2Complex

  subroutine printAssumed3Complex(l, m, arr)
    integer :: l, m
    complex(4) :: arr(m,l:*)
    print *, (/ complex(4):: arr /)
  end subroutine printAssumed3Complex


  subroutine printAssumedReal(m, arr)
    integer :: m
    real(4) :: arr(*), array(m)
    print *, (/ real(4):: arr /)
  end subroutine printAssumedReal

  subroutine printAssumed2Real(m, arr)
    integer :: m
    real(4) :: arr(2,*), array(m)
    print *, (/ real(4):: arr /)
  end subroutine printAssumed2Real

  subroutine printAssumed3Real(l, m, arr)
    integer :: l, m
    real(4) :: arr(m,l:*)
    print *, (/ real(4):: arr /)
  end subroutine printAssumed3Real


  subroutine printAssumedCharacter(m, arr)
    integer :: m
    character(4) :: arr(*), array(m)
    print *, (/ character(4):: arr /)
  end subroutine printAssumedCharacter

  subroutine printAssumed2Character(m, arr)
    integer :: m
    character(4) :: arr(2,*), array(m)
    print *, (/ character(4):: arr /)
  end subroutine printAssumed2Character

  subroutine printAssumed3Character(l, m, arr)
    integer :: l, m
    character(4) :: arr(m,l:*)
    print *, (/ character(4):: arr /)
  end subroutine printAssumed3Character


  subroutine printAssumedDerived(m, arr)
    integer :: m
    type(derived(4,7)) :: arr(*), array(m) ! tcx: (4,7)
    print *, (/ derived(4,7):: arr /) ! tcx: (4,7)
    print *, (/ integer:: arr % field /)
  end subroutine printAssumedDerived

  subroutine printAssumed2Derived(m, arr)
    integer :: m
    type(derived(4,7)) :: arr(2,*), array(m) ! tcx: (4,7)
    print *, (/ derived(4,7):: arr /) ! tcx: (4,7)
    print *, (/ integer:: arr % field /)
  end subroutine printAssumed2Derived

  subroutine printAssumed3Derived(l, m, arr)
    integer :: l, m
    type(derived(4,7)) :: arr(m,l:*) ! tcx: (4,7)
    print *, (/ derived(4,7):: arr /) ! tcx: (4,7)
    print *, (/ integer:: arr % field /)
  end subroutine printAssumed3Derived

end program acesynt41pdkl


! Extensions to introduce derived type parameters:
! type: derived - added parameters (kderived_1,lderived_1) to invoke with (4,7)/declare with (4,*) - 11 changes
