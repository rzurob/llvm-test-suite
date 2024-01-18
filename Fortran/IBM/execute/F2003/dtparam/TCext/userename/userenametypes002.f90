! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/F2003/userename/userenametypes002.f
! opt variations: -qnol -qnodeferredlp -qreuse=self

!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: userenamenest004.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Mar. 30, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Rename operator in  USE statement
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : rename a public operator binary nested module, multiple renames
!*
!234567890123456789012345678901234567890123456789012345678901234567890
module m

   type baseobj(n1,k1,k2,k3)    ! (20,4,4,4)
      integer, kind :: k1,k2,k3
      integer, len  :: n1
      integer(k1)   :: x = 0
      integer(k2)   :: y = 0
      real(k3)      :: r = 0.0

   end type

   interface operator(.multiply.)
     module procedure mult2
   end interface
   interface operator(.divide.)
     module procedure div2
   end interface
   interface operator(.add.)
     module procedure addit2
   end interface

   contains

   function addit ( a, b )
      class(baseobj(*,4,4,4)), intent(in) :: a
      type(baseobj(*,4,4,4)), intent(in)  :: b
      type(baseobj(20,4,4,4)) :: addit

      addit%x = a%x + b%x
      addit%y = a%y + b%y
      addit%r = a%r + b%r
      print *, 'add'
   end function

   function mult ( a, b )
      class(baseobj(*,4,4,4)), intent(in) :: a
      type(baseobj(*,4,4,4)), intent(in)  :: b
      type(baseobj(20,4,4,4)) :: mult

      mult%x = a%x * b%x
      mult%y = a%y * b%y
      mult%y = a%r * b%r
      print *, 'mult'
   end function

   function div ( a, b )
      class(baseobj(*,4,4,4)), intent(in) :: a
      type(baseobj(*,4,4,4)), intent(in)  :: b
      type(baseobj(20,4,4,4)) :: div

      div%x = a%x / b%x
      div%y = a%y / b%y
      div%r = a%r / b%r
      print *, 'div'
   end function

   function addit2 ( a, b )
      class(baseobj(*,4,4,4)), intent(in) :: a
      type(baseobj(*,4,4,4)), intent(in)  :: b
      type(baseobj(20,4,4,4)) :: addit2

      addit2%x = a%x + b%x
      addit2%y = a%y + b%y
      addit2%r = a%r + b%r
      print *, 'add2'
   end function

   function mult2 ( a, b )
      class(baseobj(*,4,4,4)), intent(in) :: a
      type(baseobj(*,4,4,4)), intent(in)  :: b
      type(baseobj(20,4,4,4)) :: mult2

      mult2%x = a%x * b%x
      mult2%y = a%y * b%y
      mult2%r = a%r * b%r
      print *, 'mult2'
   end function

   function div2 ( a, b )
      class(baseobj(*,4,4,4)), intent(in) :: a
      type(baseobj(*,4,4,4)), intent(in)  :: b
      type(baseobj(20,4,4,4)) :: div2

      div2%x = a%x / b%x
      div2%y = a%y / b%y
      div2%r = a%r / b%r
      print *, 'div2'
   end function



end module


program mainprog
   use m, operator (.plus.) => operator(.add.), operator (.divit.) => operator(.divide.),operator (.multit.) => operator(.multiply.)

   type(baseobj(20,4,4,4)), target      :: b1
   type(baseobj(:,4,4,4)), pointer     :: b2
   type(baseobj(:,4,4,4)), allocatable :: b3
   b1 = baseobj(20,4,4,4) ( 100, 200,2.0 )
   allocate ( b2, source = baseobj(20,4,4,4) ( 101, 201 ,3.0) )
   allocate ( b3, source = baseobj(20,4,4,4) () )

   b3 = b2 .plus. b1
   print *, b3%x
   print *, b3%y
   print *, b3%r

   b3 = b2 .divit. b1
   print *, b3%x
   print *, b3%y
   print *, b3%r

   b3 = b2 .multit. b1
   print *, b3%x
   print *, b3%y
   print *, b3%r



end program
