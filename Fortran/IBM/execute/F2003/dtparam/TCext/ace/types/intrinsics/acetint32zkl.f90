!***********************************************************************
!* =====================================================================
!* XL Fortran Test Case                            IBM INTERNAL USE ONLY
!* =====================================================================
!*
!*  TEST CASE NAME             : acetint32zkl
!*
!*  PROGRAMMER                 : Glen Mateer (derived from acetint32z
!*                               by David Forster)
!*  DATE                       : 2008-01-25 (original: 2006-08-30)
!*  ORIGIN                     : Compiler Development,
!*                               IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters (+ Array Constructor
!*                               Enhancements)
!*  SECONDARY FUNCTIONS TESTED : Array Constructor Enhancement procedure
!*                               pointers can be invoked in implied-do for
!*                               data (complex)
!*  REFERENCE                  : Feature Number 289057(.F2003TCx)
!*                               (original: Feature Number 289053)
!*
!*  DRIVER STANZA              : xlf2003 (original: xlf2003)
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : procedure pointer, complex
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!*  Procedures referenced by procedure pointers can be invoked to produce data
!*  for the array constructor.  Here we test complex numbers.
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

module acetint32zmod

  implicit none
  
  type :: modtype (kmodtype_1,lmodtype_1) ! kmodtype_1,lmodtype_1=4,5
     integer, kind :: kmodtype_1
     integer, len :: lmodtype_1
     complex(kmodtype_1) :: myval = (1,0)
     procedure (complex(4)), nopass, pointer :: modp
     procedure (myabs), pass, pointer :: myp
   contains
     procedure :: mytest
  end type modtype

  abstract interface
     complex function myabs(this)
       import modtype
       class(modtype(4,*)), intent(in) :: this ! tcx: (4,*)
     end function myabs
  end interface

contains

  complex function moduleFunc(a)
    complex :: a
    moduleFunc = a - (1,2)
  end function moduleFunc

  complex function mytest(this)
    class(modtype(4,*)), intent(in) :: this ! tcx: (4,*)
    mytest = this % myval - (2,1)
  end function mytest

end module acetint32zmod


program acetint32zkl

  use acetint32zmod
  implicit none
  interface interfaceFunc
     complex function anotherFunc(a)
       complex :: a
     end function anotherFunc
  end interface

  abstract interface
     complex function aFunc(a)
       complex :: a
     end function aFunc
  end interface

  procedure(complex) :: externFunc
  procedure(complex), pointer :: p
  procedure(myabs), pointer :: p2
  procedure(aFunc), pointer :: pif
  complex :: array(2)
  type (modtype(4,5)) :: mt ! tcx: (4,5)
  integer :: i

  pif => moduleFunc
  print *, moduleFunc((1,-1)), pif((1,-1))
  array = (/ (moduleFunc((1,-1)), pif((1,-1)), i=1,1) /)
  print *, array
  array = (/ complex:: ( moduleFunc((1,-1)), pif((1,-1)), i=1,1) /)
  print *, array

  p => externFunc
  print *, externFunc((-1,1)), p((-1,1))
  array = (/  (externFunc((-1,1)), p((-1,1)), i=1,1) /)
  print *, array
  array = (/ complex:: ( externFunc((-1,1)), p((-1,1)), i=1,1) /)
  print *, array

  pif => anotherFunc
  print *, anotherFunc((-1,1)), pif((-1,1))
  array = (/  (anotherFunc((-1,1)), pif((-1,1)), i=1,1) /)
  print *, array
  array = (/ complex:: ( anotherFunc((-1,1)), pif((-1,1)), i=1,1) /)
  print *, array

  mt % modp => externFunc
  p => mt % modp
  print *, mt % modp((-1,1)), p((-1,1))
  array = (/  (mt % modp((-1,1)), p((-1,1)), i=1,1) /)
  print *, array
  array = (/ complex:: ( mt % modp((-1,1)), p((-1,1)), i=1,1) /)
  print *, array

  mt % myp => mytest
  p2 => mt % myp
  print *, mt % myp(), p2(mt)
  array = (/  (mt % myp(), p2(mt), i=1,1) /)
  print *, array
  array = (/ complex:: ( mt % myp(), p2(mt), i=1,1) /)
  print *, array

end program acetint32zkl

complex function externFunc(a)
  complex :: a
  externFunc = a + (0,1)
end function externFunc

complex function anotherFunc(a)
  complex :: a
  anotherFunc = a + (10,10)
end function anotherFunc


! Extensions to introduce derived type parameters:
! type: modtype - added parameters (kmodtype_1,lmodtype_1) to invoke with (4,5)/declare with (4,*) - 3 changes
