! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp -qreuse=none /tstdev/F2003/userename/userenamesub002.f
! opt variations: -qnol -qnodeferredlp -qreuse=self

!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: userenamesub002.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : userenamesub002.f
!*
!*  PROGRAMMER                 : Rob Wheeler
!*  DATE                       : Mar. 30, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Rename operator in  USE statement
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  DESCRIPTION                : rename a public operator binary that uses a renamed operator
!*
!234567890123456789012345678901234567890123456789012345678901234567890
module m

   type baseobj(n1,k1,k2)    ! (20,4,4)
      integer, kind :: k1,k2
      integer, len  :: n1
      integer(k1)   :: x = -999
      integer(k2)   :: y = -999
      contains
         procedure :: mybadd
         generic :: operator(.add.) => mybadd
   end type

   interface operator(.add.)
   		module procedure mybadd2
   end interface
   
   contains

   function mybadd ( a,b )
      class(baseobj(*,4,4)), intent(in) :: a,b
      
      type(baseobj(20,4,4)) :: mybadd

      mybadd%y = a%y + b%y
      mybadd%x = a%x + b%x + 1000

      print *, 'mybadd'

   end function
   
   function mybadd2 ( a,b )
      integer, intent(in) :: a,b
      integer :: mybadd2

      mybadd2 = a + b+ 1000
      

      print *, 'mybadd2'

   end function
   


end module
module m2
use m, operator (.plus.) => operator(.add.)

  interface operator(.addten.)
  module procedure addten
  end interface
  
  contains
  
  function addten (a,b)
    class(baseobj(*,4,4)), intent(in) :: a,b
    type(baseobj(20,4,4)) :: addten
    addten= b .add. a
    addten%y=addten%y+10
    addten%x=addten%x+10
    
    print *,"addten"
  end function
end module

program mainprog
   use m2, operator (.addtenmore.) => operator(.addten.)

   type(baseobj(20,4,4)), target      :: b1
   type(baseobj(20,4,4))     :: b2
   type(baseobj(:,4,4)), allocatable :: b3
   integer :: i1=5,i2=5,i3=0
   b1 = baseobj(20,4,4) ( 100, 200 )
   b2 = baseobj(20,4,4) ( 300, 400 )

   allocate ( b3, source = baseobj(20,4,4) () )

   b3 = b2 .addtenmore. b1
   print *, b3%x

   i3 = i1 .plus. i2
   print *, i3

end program
