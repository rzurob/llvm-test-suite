! GM DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/ace/types/intrinsics/acetint31z.f

!***********************************************************************
!* =====================================================================
!*
!*                               by David Forster)
!*  DATE                       : 2008-01-25 (original: 2006-08-30)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : Array Constructor Enhancements
!*  REFERENCE                  : Feature Number 289057(.F2003TCx)
!*                               (original: Feature Number 289053)
!*
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

module acetint31zextmod

  implicit none

  type :: modtype(k1)    ! (4)
     integer, kind :: k1
     complex(k1)   :: myval = (1,0)
     procedure (complex), nopass, pointer :: modp
     procedure (myabs), pass, pointer :: myp
   contains
     procedure :: mytest
  end type modtype

  abstract interface
     complex function myabs(this)
       import modtype
       class(modtype(4)), intent(in) :: this
     end function myabs
  end interface

contains

  complex function moduleFunc(a)
    complex :: a
    moduleFunc = a - (1,2)
  end function moduleFunc

  complex function mytest(this)
    class(modtype(4)), intent(in) :: this
    mytest = this % myval - (2,1)
  end function mytest

end module acetint31zextmod


program acetint31zext

  use acetint31zextmod
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
  type (modtype(4)) :: mt

  pif => moduleFunc
  print *, moduleFunc((1,-1)), pif((1,-1))
  array = (/ moduleFunc((1,-1)), pif((1,-1)) /)
  print *, array
  array = (/ complex:: moduleFunc((1,-1)), pif((1,-1)) /)
  print *, array

  p => externFunc
  print *, externFunc((-1,1)), p((-1,1))
  array = (/ externFunc((-1,1)), p((-1,1)) /)
  print *, array
  array = (/ complex:: externFunc((-1,1)), p((-1,1)) /)
  print *, array

  pif => anotherFunc
  print *, anotherFunc((-1,1)), pif((-1,1))
  array = (/ anotherFunc((-1,1)), pif((-1,1)) /)
  print *, array
  array = (/ complex:: anotherFunc((-1,1)), pif((-1,1)) /)
  print *, array

  mt % modp => externFunc
  p => mt % modp
  print *, mt % modp((-1,1)), p((-1,1))
  array = (/ mt % modp((-1,1)), p((-1,1)) /)
  print *, array
  array = (/ complex:: mt % modp((-1,1)), p((-1,1)) /)
  print *, array

  mt % myp => mytest
  p2 => mt % myp
  print *, mt % myp(), p2(mt)
  array = (/ mt % myp(), p2(mt) /)
  print *, array
  array = (/ complex:: mt % myp(), p2(mt) /)
  print *, array

end program acetint31zext

complex function externFunc(a)
  complex :: a
  externFunc = a + (0,1)
end function externFunc

complex function anotherFunc(a)
  complex :: a
  anotherFunc = a + (10,10)
end function anotherFunc
