!***********************************************************************
!* =====================================================================
!* XL Fortran Test Case                            IBM INTERNAL USE ONLY
!* =====================================================================
!*
!*  TEST CASE NAME             : acesynt41cdl
!*
!*  PROGRAMMER                 : Glen Mateer (derived from acesynt41cd
!*                               by David Forster)
!*  DATE                       : 2007-11-16 (original: 2006-10-19)
!*  ORIGIN                     : Compiler Development,
!*                               IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*                               (+ Array Constructor Enhancements)
!*  SECONDARY FUNCTIONS TESTED : Array Constructor Enhancement AC containing
!*                               assumed-size array (print)
!*
!*  REFERENCE                  : Feature Number 289057(.F2003TCx)
!*                               (original: Feature Number 289053)
!*
!*  DRIVER STANZA              : xlf2003 (original: xlf2003)
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  KEYWORD(S)                 : dummy argument, array constructor,
!*                               assumed-size, call
!*  TARGET(S)                  : 
!*  NUMBER OF TESTS CONDITIONS : 
!*
!*  DESCRIPTION
!*
!*  Assumed-size arrays are not allowed as a "a whole array reference" anywhere but
!*  "as an actual argument in a procedure reference for which the shape is not required"
!*  (section 5.1.2.5 of the standard), so they cannot appear alone in an AC, e.g.,
!*  [assumedSizeArray], but they can appear if more detail is added, e.g.,
!*  [assumedSizeArray(1)] or [(assumedSizeArray(i),i=1,n)].  This case simply verifies
!*  that assumed-size arrays appearing as whole array references in array constructors
!*  in subroutine and function calls, as well as intrinsics, are flagged.
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

program acesynt41cdl

  implicit none

  type derived (lderived_1) ! lderived_1=3
     integer, len :: lderived_1
     integer(4) :: field = 1
  end type derived

  integer(4)   :: irank1 (2), irank2 (2,2)
  logical(4)   :: lrank1 (2), lrank2 (2,2)
  real(4)      :: rrank1 (2), rrank2 (2,2)
  complex(4)   :: zrank1 (2), zrank2 (2,2)
  character(4) :: crank1 (2), crank2 (2,2)
  type(derived(3)):: drank1 (2), drank2 (2,2) ! tcx: (3)
  integer      :: i

  irank1 = [99, 100];                irank2 = reshape([integer(4):: 4,5,6,7],[2,2])
  lrank1 = [.true., .false.];        lrank2 = reshape([logical(4):: ((i/2)==0,i=1,4)],[2,2])
  rrank1 = [9.9, 10.0];              rrank2 = reshape([real(4):: .4,.5,.6,.7],[2,2])
  zrank1 = [(1.,1.), (0.,0.)];       zrank2 = reshape([complex(4):: ((1.1,1.1),i=1,4)], [2,2])
  crank1 = ['abcd', 'efgh'];         crank2 = reshape([character(4):: '1234','5678','9abc','def0'],[2,2])
  drank1 = [derived(3)(9), derived(3)(5)]; drank2 = reshape([derived(3):: (derived(3)(i),i=1,4)],[2,2]) ! tcx: (3) ! tcx: (3) ! tcx: (3) ! tcx: (3)

  ! None of these variations should matter, actually, but we'll include them
  ! in case one or more optimisation pathway is not otherwise exercised:

  call callAssumedInteger (2, [2, 3])
  call callAssumedInteger (3, [integer(4):: 3,4,5])

  call callAssumedInteger (size(irank1), irank1)
  call callAssumed2Integer(size(irank2), irank2)
  call callAssumed3Integer(1, 2, size(irank2), irank2)

  call callAssumedLogical (size(lrank1), lrank1)
  call callAssumed2Logical(size(lrank2), lrank2)
  call callAssumed3Logical(1, 2, size(lrank2), lrank2)

  call callAssumedReal (size(rrank1), rrank1)
  call callAssumed2Real(size(rrank2), rrank2)
  call callAssumed3Real(1, 2, size(rrank2), rrank2)

  call callAssumedComplex (size(zrank1), zrank1)
  call callAssumed2Complex(size(zrank2), zrank2)
  call callAssumed3Complex(1, 2, size(zrank2), zrank2)

  call callAssumedCharacter (size(crank1), crank1)
  call callAssumed2Character(size(crank2), crank2)
  call callAssumed3Character(1, 2, size(crank2), crank2)

  call callAssumedDerived (size(drank1), drank1)
  call callAssumed2Derived(size(drank2), drank2)
  call callAssumed3Derived(1, 2, size(drank2), drank2)

contains

  subroutine callAssumedInteger(m, arr)
    integer :: m, val
    integer(4) :: arr(*), array(m), result
    call subTest   ([integer(4):: arr])
    call subTestE  ([integer(4):: arr])
    result = maxval([integer(4):: arr])
    val = funTest  ([integer(4):: arr])
  end subroutine callAssumedInteger

  subroutine callAssumed2Integer(m, arr)
    integer :: m, val
    integer(4) :: arr(2,*), array(m), result
    call subTest   ([integer(4):: arr])
    call subTestE  ([integer(4):: arr])
    result = minval([integer(4):: arr])
    val = funTest  ([integer(4):: arr])
  end subroutine callAssumed2Integer

  subroutine callAssumed3Integer(l, m, n, arr)
    integer :: l, m, n, val
    integer(4) :: arr(m,l:*), array(n), result
    call subTest   ([integer(4):: arr])
    call subTestE  ([integer(4):: arr])
    result = sum   ([integer(4):: arr])
    val = funTest  ([integer(4):: arr])
  end subroutine callAssumed3Integer


  subroutine callAssumedLogical(m, arr)
    integer :: m, val
    logical(4) :: arr(*), array(m), result
    call subTest   ([logical(4):: arr])
    call subTestE  ([logical(4):: arr])
    result = all   ([logical(4):: arr])
    val = funTest  ([logical(4):: arr])
  end subroutine callAssumedLogical

  subroutine callAssumed2Logical(m, arr)
    integer :: m, val
    logical(4) :: arr(2,*), array(m), result
    call subTest   ([logical(4):: arr])
    call subTestE  ([logical(4):: arr])
    result = any   ([logical(4):: arr])
    val = funTest  ([logical(4):: arr])
  end subroutine callAssumed2Logical

  subroutine callAssumed3Logical(l, m, n, arr)
    integer :: l, m, n, val, result
    logical(4) :: arr(m,l:*), array(n)
    call subTest   ([logical(4):: arr])
    call subTestE  ([logical(4):: arr])
    result = count ([logical(4):: arr])
    val = funTest  ([logical(4):: arr])
  end subroutine callAssumed3Logical


  subroutine callAssumedComplex(m, arr)
    integer :: m, val
    complex(4) :: arr(*), array(m), result
    call subTest   ([complex(4):: arr])
    call subTestE  ([complex(4):: arr])
    result = maxval(real([complex(4):: arr]))
    val = funTest  ([complex(4):: arr])
  end subroutine callAssumedComplex

  subroutine callAssumed2Complex(m, arr)
    integer :: m, val
    complex(4) :: arr(2,*), array(m), result
    call subTest   ([complex(4):: arr])
    call subTestE  ([complex(4):: arr])
    result = minval(aimag([complex(4):: arr]))
    val = funTest  ([complex(4):: arr])
  end subroutine callAssumed2Complex

  subroutine callAssumed3Complex(l, m, n, arr)
    integer :: l, m, n, val
    complex(4) :: arr(m,l:*), array(n), result
    call subTest   ([complex(4):: arr])
    call subTestE  ([complex(4):: arr])
    result = sum   ([complex(4):: arr])
    val = funTest  ([complex(4):: arr])
  end subroutine callAssumed3Complex


  subroutine callAssumedReal(m, arr)
    integer :: m, val
    real(4) :: arr(*), array(m), result
    call subTest   ([real(4):: arr])
    call subTestE  ([real(4):: arr])
    result =product([real(4):: arr])
    val = funTest  ([real(4):: arr])
  end subroutine callAssumedReal

  subroutine callAssumed2Real(m, arr)
    integer :: m, val
    real(4) :: arr(2,*), array(m), result
    call subTest   ([real(4):: arr])
    call subTestE  ([real(4):: arr])
    result = maxval([real(4):: arr])
    val = funTest  ([real(4):: arr])
  end subroutine callAssumed2Real

  subroutine callAssumed3Real(l, m, n, arr)
    integer :: l, m, n, val
    real(4) :: arr(m,l:*), array(n), result
    call subTest   ([real(4):: arr])
    call subTestE  ([real(4):: arr])
    result = minval([real(4):: arr])
    val = funTest  ([real(4):: arr])
  end subroutine callAssumed3Real


  subroutine callAssumedCharacter(m, arr)
    integer :: m, val
    character(4) :: arr(*), array(m), result
    call subTest   ([character(4):: arr])
    call subTestE  ([character(4):: arr])
    result = minval([character(4):: arr])
    val = funTest  ([character(4):: arr])
  end subroutine callAssumedCharacter

  subroutine callAssumed2Character(m, arr)
    integer :: m, val
    character(4) :: arr(2,*), array(m), result
    call subTest   ([character(4):: arr])
    call subTestE  ([character(4):: arr])
    result = maxval([character(4):: arr])
    val = funTest  ([character(4):: arr])
  end subroutine callAssumed2Character

  subroutine callAssumed3Character(l, m, n, arr)
    integer :: l, m, n, val
    character(4) :: arr(m,l:*), array(n), result
    call subTest   ([character(4):: arr])
    call subTestE  ([character(4):: arr])
    result = minval([character(4):: arr])
    val = funTest  ([character(4):: arr])
  end subroutine callAssumed3Character


  subroutine callAssumedDerived(m, arr)
    integer :: m, val, result
    type(derived(3)) :: arr(*), array(m) ! tcx: (3)
    call subTest   ([derived(3):: arr]);    call subTest   ([integer:: arr % field]) ! tcx: (3)
    call subTestE  ([derived(3):: arr]);    call subTestE  ([integer:: arr % field]) ! tcx: (3)
    result = size  ([derived(3):: arr]);    result = size  ([integer:: arr % field]) ! tcx: (3)
    val = funTest  ([derived(3):: arr]);    val = funTest  ([integer:: arr % field]) ! tcx: (3)
  end subroutine callAssumedDerived

  subroutine callAssumed2Derived(m, arr)
    integer :: m, val, result
    type(derived(3)) :: arr(2,*), array(m) ! tcx: (3)
    call subTest   ([derived(3):: arr]);    call subTest   ([integer:: arr % field]) ! tcx: (3)
    call subTestE  ([derived(3):: arr]);    call subTestE  ([integer:: arr % field]) ! tcx: (3)
    result = size  ([derived(3):: arr]);    result = size  ([integer:: arr % field]) ! tcx: (3)
    val = funTest  ([derived(3):: arr]);    val = funTest  ([integer:: arr % field]) ! tcx: (3)
  end subroutine callAssumed2Derived

  subroutine callAssumed3Derived(l, m, n, arr)
    integer :: l, m, n, val, result
    type(derived(3)) :: arr(m,l:*), array(n) ! tcx: (3)
    call subTest   ([derived(3):: arr]);    call subTest   ([integer:: arr % field]) ! tcx: (3)
    call subTestE  ([derived(3):: arr]);    call subTestE  ([integer:: arr % field]) ! tcx: (3)
    result = ubound([derived(3):: arr], 1); result = ubound([integer:: arr % field], 1); ! tcx: (3)
    val = funTest  ([derived(3):: arr]);    val = funTest  ([integer:: arr % field]) ! tcx: (3)
  end subroutine callAssumed3Derived


  subroutine subTest   (arg)
    class(*) :: arg(:)
    print *, size(arg)
  end subroutine subTest

  elemental subroutine subTestE (arg)
    class(*), intent(in) :: arg
  end subroutine subTestE

  integer function funTest(arg)
    class(*) :: arg(:)
    funTest = ubound(arg,1)
  end function funTest

end program acesynt41cdl


! Extensions to introduce derived type parameters:
! type: derived - added parameters (lderived_1) to invoke with (3)/declare with (*) - 20 changes
