! GM DTP extension using:
! ftcx_dtp -ql /tstdev/F2003/ace/diag/types/intrinsics/acetint32ld.f

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
!*  KEYWORD(S)                 : procedure pointer, logical
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  AC-values can be any expression consistent with the type of the array to
!*  be constructed.  Procedure pointers are not in this set, when used as
!*  pointers.  Another test verifies that procedure pointers are allowed when
!*  used to invoke the function they reference.  Here we test logicals.
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

module acetint32ldlmod

  implicit none

  type :: modtype(l1,k1)    ! (20,4)
     integer, kind :: k1
     integer, len  :: l1
     logical(k1)   :: myval = .true.
     procedure (logical), nopass, pointer :: modp
     procedure (myabs), pass, pointer :: myp
   contains
     procedure :: mytest
  end type modtype

  abstract interface
     logical function myabs(this)
       import modtype
       class(modtype(*,4)), intent(in) :: this
     end function myabs
  end interface

contains

  logical function moduleFunc(a)
    logical :: a
    moduleFunc = .false.
  end function moduleFunc

  logical function mytest(this)
    class(modtype(*,4)), intent(in) :: this
    mytest = this % myval
  end function mytest

end module acetint32ldlmod


program acetint32ldl

  use acetint32ldlmod
  implicit none
  interface interfaceFunc
     logical function anotherFunc(a)
       logical :: a
     end function anotherFunc
  end interface

  abstract interface
     logical function aFunc(a)
       logical :: a
     end function aFunc
  end interface

  procedure(logical) :: externFunc
  procedure(logical), pointer :: p
  procedure(myabs), pointer :: p2
  procedure(aFunc), pointer :: pif
  logical :: array(2)
  type (modtype(20,4)) :: mt
  integer :: i

  pif => moduleFunc
  array = (/  (moduleFunc, pif, i=1,1) /)
  print *, array
  array = (/ logical:: ( moduleFunc, pif, i=1,1) /)
  print *, array

  p => externFunc
  array = (/  (externFunc, p, i=1,1) /)
  print *, array
  array = (/ logical:: ( externFunc, p, i=1,1) /)
  print *, array

  pif => anotherFunc
  array = (/  (anotherFunc, pif, i=1,1) /)
  print *, array
  array = (/ logical:: ( anotherFunc, pif, i=1,1) /)
  print *, array

  mt % modp => externFunc
  p => mt % modp
  array = (/  (mt % modp, p, i=1,1) /)
  print *, array
  array = (/ logical:: ( mt % modp, p, i=1,1) /)
  print *, array

  mt % myp => mytest
  p2 => mt % myp
  array = (/  (mt % myp, p2, i=1,1) /)
  print *, array
  array = (/ logical:: ( mt % myp, p2, i=1,1) /)
  print *, array

end program acetint32ldl

logical function externFunc(a)
  logical :: a
  externFunc = .true. .eqv. a
end function externFunc

logical function anotherFunc(a)
  logical :: a
  anotherFunc = .not. a
end function anotherFunc