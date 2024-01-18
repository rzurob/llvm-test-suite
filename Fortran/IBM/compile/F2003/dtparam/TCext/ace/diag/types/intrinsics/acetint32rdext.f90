! GM DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/ace/diag/types/intrinsics/acetint32rd.f

!***********************************************************************
!* =====================================================================
!*
!*                               by David Forster)
!*  DATE                       : 2007-12-03 (original: 2006-08-30)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*
!*  SECONDARY FUNCTIONS TESTED : Array Constructor Enhancements
!*  REFERENCE                  : Feature Number 289057(.F2003TCx)
!*                               (original: Feature Number 289053)
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : procedure pointer, real
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  AC-values can be any expression consistent with the type of the array to
!*  be constructed.  Procedure pointers are not in this set, when used as
!*  pointers.  Another test verifies that procedure pointers are allowed when
!*  used to invoke the function they reference.  Here we test reals.
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

module acetint32rdextmod

  implicit none

  type :: modtype(k1)    ! (4)
     integer, kind :: k1
     real(k1)      :: myval = 5
     procedure (real), nopass, pointer :: modp
     procedure (myabs), pass, pointer :: myp
   contains
     procedure :: mytest
  end type modtype

  abstract interface
     real function myabs(this)
       import modtype
       class(modtype(4)), intent(in) :: this
     end function myabs
  end interface

contains

  real function moduleFunc(a)
    real :: a
    moduleFunc = a ** 2
  end function moduleFunc

  real function mytest(this)
    class(modtype(4)), intent(in) :: this
    mytest = this % myval - 10
  end function mytest

end module acetint32rdextmod


program acetint32rdext

  use acetint32rdextmod
  implicit none
  interface interfaceFunc
     real function anotherFunc(a)
       real :: a
     end function anotherFunc
  end interface

  abstract interface
     real function aFunc(a)
       real :: a
     end function aFunc
  end interface

  procedure(real) :: externFunc
  procedure(real), pointer :: p
  procedure(myabs), pointer :: p2
  procedure(aFunc), pointer :: pif
  real :: array(2)
  type (modtype(4)) :: mt
  integer :: i

  pif => moduleFunc
  array = (/  (moduleFunc, pif, i=1,1) /)
  print *, array
  array = (/ real:: ( moduleFunc, pif, i=1,1) /)
  print *, array

  p => externFunc
  array = (/  (externFunc, p, i=1,1) /)
  print *, array
  array = (/ real:: ( externFunc, p, i=1,1) /)
  print *, array

  pif => anotherFunc
  array = (/  (anotherFunc, pif, i=1,1) /)
  print *, array
  array = (/ real:: ( anotherFunc, pif, i=1,1) /)
  print *, array

  mt % modp => externFunc
  p => mt % modp
  array = (/  (mt % modp, p, i=1,1) /)
  print *, array
  array = (/ real:: ( mt % modp, p, i=1,1) /)
  print *, array

  mt % myp => mytest
  p2 => mt % myp
  array = (/  (mt % myp, p2, i=1,1) /)
  print *, array
  array = (/ real:: ( mt % myp, p2, i=1,1) /)
  print *, array

end program acetint32rdext

real function externFunc(a)
  real :: a
  externFunc = a + 10
end function externFunc

real function anotherFunc(a)
  real :: a
  anotherFunc = a * 10
end function anotherFunc
