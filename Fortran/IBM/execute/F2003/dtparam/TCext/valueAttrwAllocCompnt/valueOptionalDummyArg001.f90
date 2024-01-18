! GB DTP extension using:
! ftcx_dtp -qnol -qreuse=base /tstdev/F2003/valueAttrwAllocCompnt/valueOptionalDummyArg001.f
! opt variations: -ql -qreuse=none

!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Robert Ma
!*  DATE                       : 11/01/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Value Attribute for derived type containing allocatable components
!*                             :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : value attribute with derived type containing allocatable components
!*                                 - type: derived type with intrinsic allocatable components
!*                                 - actual arg: non-polymorphic data arg (non-pointer non-allocatable, pointer, allocatable) with optional attribute
!*                                 - dummy arg: non-polymorphic with value attribute
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m

   type base(k1)    ! (4)
      integer, kind            :: k1
      integer(k1), allocatable :: i
   end type
   
   type, extends(base) :: child    ! (4)
      real(k1), allocatable :: r
   end type

   contains

   subroutine foo ( a, b )
      type(base(4)), value, optional :: a
      type(base(4)), value, optional :: b
      
      print *, 'foo:'
      if ( present(a) ) then
      	 print *, a%i
      	 a%i = -999
      	 print *, a%i
      end if
      
      if ( present(b) ) then
      	 print *, b%i
      	 b%i = -999
      	 print *, b%i
      end if
      
   end subroutine
   
   subroutine bar ( a, b )
      type(child(4)), value, optional :: a
      type(child(4)), value, optional :: b
      
      print *, 'bar:'
      if ( present(a) ) then
      	 print *, a%i, a%r
      	 a%i = -999
      	 a%r = -999.9999
      	 print *, a%i, a%r
      end if
      
      if ( present(b) ) then
      	 print *, b%i, b%r
      	 b%i = -999
      	 b%r = -999.9999
      	 print *, b%i, b%r
      end if
      
   end subroutine

end module

program valueOptionalDummyArg001
   use m

   type(base(4)), pointer :: b1
   class(base(4)), allocatable :: b2
   type(child(4)), allocatable :: c1
   class(child(4)), pointer :: c2
   
   allocate ( b1, source = base(4)(100) )
   allocate ( b2, source = child(4)(200,-999.9999) )
   allocate ( c1, source = child(4)(10,15.0) )
   allocate ( c2, source = child(4)(20,25.0) )
      
   call foo()
   call foo(b=b1)
   print *, b1%i
   call foo(b2)
   print *, b2%i
   call foo(b=b2,a=b1)
   print *, b1%i, b2%i
   
   call bar()
   call bar(b=c1)
   print *, c1%i, c1%r
   call bar(c2)
   print *, c2%i, c2%r
   call bar(b=c2,a=c1)
   print *, c1%i, c1%r, c2%i, c2%r

end program
