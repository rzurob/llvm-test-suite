! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/userename/userenametypes001.f
! opt variations: -ql -qreuse=self

!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: userenametypes001.f
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
!*  TEST CASE TITLE            : userenametypes001.f
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
!*  DESCRIPTION                : rename a public operator unary with defined types
!*
!234567890123456789012345678901234567890123456789012345678901234567890
module m

   type baseobj(k1,k2,k3)    ! (4,4,4)
      integer, kind :: k1,k2,k3
      integer(k1)   :: x = 0
      integer(k2)   :: y = 0
      real(k3)      :: r = 0.0
      
   end type

   interface operator(.square.)
     module procedure sq
   end interface
  
   interface operator(.power.)
     module procedure raise
   end interface
   
   contains

   
   function raise ( a )
      class(baseobj(4,4,4)), intent(in) :: a
       type(baseobj(4,4,4)) :: raise

      raise%x = a%x ** 3
      raise%y = a%y ** 3
      raise%r = a%r ** 3
      print *, 'raised'
   end function
   
   function sq ( a )
      class(baseobj(4,4,4)), intent(in) :: a
      type(baseobj(4,4,4)) :: sq

      sq%x = a%x * a%x
      sq%y = a%y * a%y
      sq%r = a%r * a%r
      print *, 'sq'
   end function
   
  
   


end module


program mainprog
   use m, operator (.squareit.) => operator(.square.), operator (.raiseit.) => operator(.power.)

   type(baseobj(4,4,4)), target      :: b1
   type(baseobj(4,4,4)), pointer     :: b2
   type(baseobj(4,4,4)), allocatable :: b3
   b1 = baseobj(4,4,4) ( 3, 4,2.0 )
   allocate ( b2, source = baseobj(4,4,4) ( 10, 20 ,3.0) )
   allocate ( b3, source = baseobj(4,4,4) () )

   b3 = .squareit.b1
   print *, b3%x
   print *, b3%y
   print *, b3%r
   
  
   
   b3 = .raiseit. b1
   print *, b3%x
   print *, b3%y
   print *, b3%r

   

end program
