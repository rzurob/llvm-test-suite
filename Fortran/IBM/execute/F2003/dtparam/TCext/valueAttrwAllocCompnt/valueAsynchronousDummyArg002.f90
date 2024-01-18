! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodefaultpv -qdeferredlp -qreuse=none /tstdev/F2003/valueAttrwAllocCompnt/valueAsynchronousDummyArg002.f
! opt variations: -qnok -qnol -qdefaultpv -qnodeferredlp -qreuse=self

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
!*                                 - type: derived type with (non-)polymorphic allocatable components
!*                                 - actual arg: non-polymorphic data arg (non-pointer non-allocatable, pointer, allocatable) with asynchronous attribute
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

   type inner(n1,k1)    ! (20,4)
      integer, kind            :: k1
      integer, len             :: n1
      integer(k1), allocatable :: i
   end type

   type base(k2,n2)    ! (4,20)
      integer, kind                  :: k2
      integer, len                   :: n2
      type(inner(:,k2)), allocatable :: in
   end type

   contains

   subroutine foo ( a )
      type(base(4,20)), value, asynchronous :: a

      write( 1, asynchronous="yes" ) a%in%i

   end subroutine

   subroutine bar ( a )
      type(base(4,*)), asynchronous :: a

      read( 1, asynchronous="yes" ) a%in%i

   end subroutine

   subroutine badbar ( a )
      type(base(4,20)), asynchronous, value :: a
      integer :: myid

      print *, 'inside badbar'
      read( 1, asynchronous="yes", id= myid ) a%in%i
      wait(1,id=myid)
      print *, a%in%i

   end subroutine

end module

program valueOptionalDummyArg002
   use m

   type(base(4,20)) :: b1
   type(base(4,:)), allocatable :: b2

   b1 = base(4,20)(inner(20,4)(100))
   allocate ( b2, source =  base(4,20)(inner(20,4)(1000)) )
   open ( 1, file="valueOptionalDummyArg002.1", asynchronous="yes", form="unformatted" )

   call foo ( b1 )
   b1 = base(4,20)(inner(20,4)(-999))
   print *, b1%in%i

   call foo ( b2 )
   b2 = base(4,20)(inner(20,4)(-999))
   print *, b2%in%i

   rewind 1

   call bar(b1)
   print *, 'after bar'
   print *, b1%in%i

   call badbar(b2)
   print *, 'after badbar'
   print *, b2%in%i

   close ( 1, status="delete" )

end program
