! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/valueAttrwAllocCompnt/valueAsynchronousDummyArg001.f
! opt variations: -ql

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Value Attribute for derived type containing allocatable components
!*  SECONDARY FUNCTIONS TESTED :
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

   type base(k1)    ! (4)
      integer, kind            :: k1
      integer(k1), allocatable :: i
   end type

   contains

   subroutine foo ( a )
      type(base(4)), value, asynchronous :: a

      write( 1, asynchronous="yes" ) a%i

   end subroutine

   subroutine bar ( a )
      type(base(4)), asynchronous :: a

      read( 1, asynchronous="yes" ) a%i

   end subroutine

   subroutine badbar ( a )
      type(base(4)), asynchronous, value :: a
      integer :: myid

      print *, 'inside badbar'
      read( 1, asynchronous="yes", id= myid ) a%i
      wait(1,id=myid)
      print *, a%i

   end subroutine

end module

program valueOptionalDummyArg001
   use m

   type(base(4)) :: b1
   type(base(4)), allocatable :: b2

   b1 = base(4)(100)
   allocate ( b2, source = base(4)(1000) )
   open ( 1, file="valueOptionalDummyArg001.1", asynchronous="yes", form="unformatted" )

   call foo ( b1 )
   b1 = base(4)(-999)
   print *, b1%i

   call foo ( b2 )
   b2 = base(4)(-999)
   print *, b2%i

   rewind 1

   call bar(b1)
   print *, 'after bar'
   print *, b1%i

   call badbar(b2)
   print *, 'after badbar'
   print *, b2%i

   close ( 1, status="delete" )

end program
